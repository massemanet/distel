; Network state machine engine

(eval-when-compile
  (require 'cl)
  (or (fboundp 'defvar-local)
      (defmacro defvar-local (var val &optional docstring)
        (declare (debug defvar) (doc-string 3))
        (list 'progn (list 'defvar var val docstring)
              (list 'make-variable-buffer-local (list 'quote var))))))

(defcustom fsm-use-debug-buffer nil
  "*Store fsm debug messages in a buffer."
  :type 'boolean
  :group 'distel)

(defvar-local fsm-buffer-p nil
  "Set to t in buffers belonging to FSMs, for sanity-checking.")
(defvar-local fsm-state nil "Current state.")
(defvar-local fsm-process nil
  "Socket associated with this FSM.")
(defvar-local fsm-cont nil
  "Continuation function called with the result of the FSM, if it
terminates successfully (with fsm-terminate).")
(defvar-local fsm-fail-cont nil
  "Continuation function called with the result of the FSM, if it
terminates in failure.")
(defvar-local fsm-work-buffer nil
  "Buffer used for creating messages, dynamically bound in
`fsm-build-message'.")
(defvar-local fsm-put-data-in-buffer nil
  "When set to `t', new data is appended to the FSM's buffer in
addition to being passed as an argument.")

(defvar fsm-cleanup-hook nil)           ; unused

(defmacro fsm-with-error-cleanup (cleanup &rest body)
  "Execute BODY, and if it hits an error run CLEANUP."
  (declare (indent 1))
  (let ((success (make-symbol "success")))
    `(let (,success)
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,success t))
         (unless ,success ,cleanup)))))

;; ----------------------------------------------------------------------
;; External API
;; ----------------------------------------------------------------------

(defun fsm-open-socket (host port)
  (let ((buf (generate-new-buffer " *net-fsm*")))
    (fsm-with-error-cleanup (kill-buffer buf)
      (let ((p (open-network-stream "netfsm" buf host port)))
        (set-process-coding-system p 'no-conversion 'no-conversion)
        (when (fboundp 'set-process-filter-multibyte)
          (set-process-filter-multibyte p nil))
        p))))

(defun fsm-connect (host port state0 &optional init-arg cont fail-cont buffer)
  "Connect to HOST on PORT and initialize a state machine in
STATE0 to handle the socket.

INIT-ARG is passed to the state machine as the `init' event's
argument. CONT is a function which is called with the FSM's result if
it terminates successfully. FAIL-CONT is called with no arguments if
the FSM fails."
  (fsm-with-error-cleanup (funcall fail-cont)
    (let ((socket (fsm-open-socket host port)))
      (fsm-attach socket state0 init-arg cont fail-cont buffer))))

(defun fsm-attach (socket state0 &optional init-arg cont fail-cont buffer)
  "Attach a new FSM to SOCKET, starting in STATE0.

INIT-ARG is passed to the state machine as the `init' event's
argument. CONT is a function which is called with the FSM's result if
it terminates successfully. FAIL-CONT is called with no arguments if
the FSM fails."
  (when buffer
    (fsm-replace-process-buffer socket buffer))
  (with-current-buffer (process-buffer socket)
    (unless (featurep 'xemacs)
      (set-buffer-multibyte nil))
    (setq fsm-buffer-p t)
    (setq fsm-state state0)
    (setq fsm-process socket)
    (setq fsm-cont cont)
    (setq fsm-fail-cont fail-cont)
    (set-process-sentinel socket #'fsm-sentinel)
    (set-process-filter   socket #'fsm-filter)
    (fsm-init init-arg)))

(defmacro with-fsm (fsm &rest body)     ; unused
  "Execute BODY in the context (buffer) of FSM."
  `(with-current-buffer (process-buffer ,fsm)
     ,@body))

;; ----------------------------------------------------------------------
;; FSM API
;; ----------------------------------------------------------------------

(defun fsm-change-state (next-state &optional run-now)
  "Change to `next-state'."
  (fsm-debug "STATE: %S -> %S\n" fsm-state next-state)
  (setq fsm-state next-state)
  (when run-now
    (fsm-event 'data "")))

(defun fsm-event (event &optional arg)
  "Process `event' in the current state."
  (fsm-assert-invariants)
  (fsm-debug "EVENT: %S - %S\n" event arg)
  (fsm-with-error-cleanup
      (fsm-fail (format "Error on event %S in state %S"
                        event fsm-state))
    (funcall fsm-state event arg)))

(defun fsm-terminate (&optional result)
  "Terminate an FSM with success. The continuation function, if
available, is called with RESULT."
  (fsm-debug "TERM : %S\n" result)
  (fsm-assert-invariants)
  (let ((cont fsm-cont))
    (fsm-shutdown)
    (when cont
      (funcall cont result))))

(defun fsm-fail (&optional why)
  "Terminate an FSM with failure."
  (if why
      (fsm-debug "FAIL : %S (buffer: %S)\n" why (current-buffer))
    (fsm-debug "FAIL : (buffer: %S)\n" (current-buffer)))
  (let ((cont fsm-fail-cont))
    (fsm-shutdown)
    (when cont
      (funcall cont))))

(defun fsm-send-string (string)
  "Send a string to the FSM's socket."
  (fsm-debug "SEND : %S\n" string)
  (process-send-string fsm-process string))

(defun fsm-send-bytes (chars)
  "Send a list of bytes to the FSM's socket."
  (fsm-send-string (apply #'string chars)))

(defun fsm-debug (fmt &rest args)
  "Print a debugging message to the *fsm-debug* buffer."
  (if fsm-use-debug-buffer
      (with-current-buffer (get-buffer-create "*fsm-debug*")
        (unless (featurep 'xemacs)
          (set-buffer-multibyte nil))
        (goto-char (point-max))
        (insert (apply #'format (cons fmt (mapcar #'fsm-summarise args)))))))

(defun fsm-check-event (event &rest allowed)
  "Ensure that an event is allowed. If EVENT is not one of ALLOWED, an
error is signaled."
  (unless (memq event allowed)
    (error "Can't handle event %S in state %S" event fsm-state)))

;; ------------------------------------------------------------
;; Message building
;; ------------------------------------------------------------

(defmacro fsm-build-message (&rest body)
  "Execute BODY, and return the message that it creates via calls to
fsm-{insert,encode}*."
  `(let ((fsm-work-buffer (with-current-buffer (generate-new-buffer " *fsm-msg*")
                            (set-buffer-multibyte nil)
                            (current-buffer))))
     (unwind-protect
         (progn ,@body
                (with-current-buffer fsm-work-buffer (buffer-string)))
       (kill-buffer fsm-work-buffer))))

(defmacro fsm-with-message-buffer (&rest body)
  "Execute BODY in the work buffer setup by fsm-build-message. When
called outside fsm-build-message, BODY is just executed in the current
buffer."
  `(with-current-buffer (or fsm-work-buffer
                            (current-buffer)) ,@body))

(put 'fsm-build-message 'lisp-indent-function 'defun)
(put 'fsm-with-message-buffer 'lisp-indent-function 1)

(defun fsm-encode (n size)
  "Encode N as a SIZE-byte integer."
  (ecase size
    ((1) (fsm-encode1 n))
    ((2) (fsm-encode2 n))
    ((4) (fsm-encode4 n))))
(defun fsm-encode1 (n)
  "Encode N as a 1-byte integer."
  (fsm-with-message-buffer
      (insert n)))
(defun fsm-encode2 (n)
  "Encode N as a 2-byte big-endian integer."
  (fsm-with-message-buffer
      (insert (logand (ash n -8) 255)
              (logand n          255))))
(defun fsm-encode4 (n)
  "Encode N as a 4-byte big-endian integer."
  (fsm-with-message-buffer
      (insert (logand (ash n -24) 255)
              (logand (ash n -16) 255)
              (logand (ash n -8)  255)
              (logand n           255))))
(defun fsm-insert (&rest args)
  "Insert ARGS (characters or strings) into the encoding buffer."
  (fsm-with-message-buffer
      (apply #'insert args)))

;; ----------------------------------------------------------------------
;; Internals
;; ----------------------------------------------------------------------

(defun fsm-init (init-arg)
  "Deliver initial events: INIT, and possibly DATA if some has arrived."
  (let ((data (buffer-string)))
    (erase-buffer)
    (fsm-event 'init init-arg)
    (unless (= 0 (length data))
      (fsm-deliver-data data))))

(defun fsm-filter (socket string)
  (with-current-buffer (process-buffer socket)
    (when fsm-state (fsm-deliver-data string))))

(defun fsm-deliver-data (data)
  (when fsm-put-data-in-buffer
    ;; incorporate the new data into the buffer
    (goto-char (point-max))
    (insert data))
  (fsm-event 'data data))

(defun fsm-sentinel (socket event)
  (with-current-buffer (process-buffer socket)
    (fsm-event 'closed event)))

(defun fsm-shutdown ()
  (setq fsm-state nil)
  (when fsm-process
    (set-process-sentinel fsm-process nil)
    (kill-buffer (process-buffer fsm-process))))

(defun fsm-assert-invariants ()
  (assert fsm-buffer-p)
  (assert (not (null fsm-state))))

(defun fsm-summarise (x)
  (if (stringp x)
      (with-temp-buffer
        (insert x)
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (replace-match "\\n" nil t))
        (fsm-elide-string (buffer-string) 30))
    x))

(defun fsm-elide-string (s len)
  (if (> (length s) len)
      (concat (substring s 0 (- len 3)) "...")
    s))

(defun fsm-replace-process-buffer (process buffer)
  (let ((oldbuffer (process-buffer process)))
    (set-process-buffer process buffer)
    (kill-buffer oldbuffer)))

(provide 'net-fsm)
