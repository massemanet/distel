;;; variables for customization
;; erlang-compile-server-check-on-save default t
;; erlang-compile-server-check-on-interval default nil
;; erlang-compile-server-compile-if-ok default nil
;; erlang-compile-server-verbose default nil
;; erlang-compile-server-interval default 120

(require 'distel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang Compile Server ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customization

(defvar erlang-compile-server-check-on-interval nil
"Checks errors and warnings on given intervals.")

(defvar erlang-compile-server-check-on-save t
"Checks errors and warnings on save.")

(defvar erlang-compile-server-compile-if-ok nil
"Compiles the module if it doesn't have any errors or warnings.")

(defvar erlang-compile-server-verbose nil
"Writes output to *Messages* buffer.")

(defvar erlang-compile-server-interval 120
"Seconds between checks if `erlang-compile-server-check-on-interval' is set.")

;; Check that module is loaded else load it
(add-hook 'erl-nodeup-hook 'ecs-check-backend)

(defun ecs-check-backend (node _fsm)
  "Check if we have the 'erlang_compile_server' module available on `node'.
If not then try to send the module over as a binary and load it in."
  (unless distel-inhibit-backend-check
    (erl-spawn
      (erl-send `[rex ,node]
		`[,erl-self [call
			     code ensure_loaded (erlang_compile_server)
			     ,(erl-group-leader)]])
      (erl-receive (node)
	  ((['rex ['error _]]
	    (&erl-load-backend node))
	   (_ t))))))

;; Initialize
(defun erlang-compile-server-setup ()
   ;; todo: add hooks, what if variable changes while running?
  (when erlang-compile-server-check-on-save
      (add-hook 'after-save-hook 'erlang-compile-server-check-file-ending))
  (add-hook 'erlang-mode-hook 'erlang-compile-server-mode-hook)
  (add-to-list 'minor-mode-alist
	       '(erlang-compile-server-mode
		 " ECS"))
  (if (not (null erlang-compile-server-check-on-interval))
      (erlang-compile-server-start-interval))
)

;;; todo
(defun erlang-compile-server-start-interval ()
;;;  (while t
;;;  (if (buffer-modified-p) (erlang-compile-server-check-for-errors-and-warnings))
;;;  (sleep-for 'erlang-compile-server-interval)
;;;  (message "Checking..."))
)

(defun erlang-compile-server-mode-hook ()
  (erlang-compile-server-mode t))

(define-minor-mode erlang-compile-server-mode
  "Erlang compile server"
  nil
  nil
  '(("\C-x\C-a" 'undefined)))

(defconst ecs-keys
  '(("\C-x\C-a" erlang-compile-server-check-for-errors-and-warnings)
    ("\C-c\C-n" erlang-compile-server-goto-next-error)
    ("\C-c\C-p" erlang-compile-server-goto-previous-error))
  "Keybindings.")

(dolist (binding ecs-keys) (define-key erlang-compile-server-mode-map (car binding) (cadr binding)))

;; On save, check for errors etc
(defun erlang-compile-server-check-file-ending()
  "Checks that it is a valid erlang file, this because of the after-save-hook."
  (let ((path (buffer-file-name)))
    (if (string= (substring path (- 0 (length ".erl"))) ".erl") (erlang-compile-server-check-for-errors-and-warnings))))

(defun erlang-compile-server-message (var msg &rest rest)
  "Prints a message if a given variable or expression is non-nil."
  (when var (message msg rest))
)

(defvar ecs-node nil)

;;; Main function
(defun erlang-compile-server-check-for-errors-and-warnings()
  (interactive)
  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(buffer (current-buffer))
	(inc-dirs (erlang-compile-server-get-includes))
	(inc-libs (erlang-compile-server-get-includes t))
	incs
	(incstring '())
	tmpopts)
    (setq incs (append inc-dirs inc-libs))
    (dolist (inc incs) (add-to-list 'incstring (format "{i, %s}, " (file-name-directory inc))))
    (erl-spawn
      (if (buffer-modified-p)
	  (erl-send-rpc node 'erlang_compile_server 'get_warnings_from_buffer (list (buffer-string) incstring))
	(erl-send-rpc node 'erlang_compile_server 'get_warnings (list path incstring))) ; n m f a

      (erl-receive (buffer incstring)
	  ((['rex ['ok]] ; no errors
	    (setq ecs-error-list '())
	    (erlang-compile-server-remove-overlays buffer)
	    (erlang-compile-server-message erlang-compile-server-verbose "Ok.")

	    ;; compiling stuff
	    (when (and (not (buffer-modified-p))
		       erlang-compile-server-compile-if-ok)
	      (progn (erlang-compile-server-message erlang-compile-server-verbose "Compiling.")
		     (setq tempopts erlang-compile-extra-opts)
		     (setq erlang-compile-extra-opts incstring)
		     (erlang-compile)
		     (setq erlang-compile-extra-opts tempopts))))

	   (['rex ['w warnings]] ; only warnings
	    (set-buffer buffer)
	    (setq ecs-error-list warnings)
	    (erlang-compile-server-remove-overlays buffer)
	    (erlang-compile-server-print-errors-and-warnings warnings))

	   (['rex ['e errors]] ; errors and possibly warnings
	    (set-buffer buffer)
	    (setq ecs-error-list errors)
	    (erlang-compile-server-remove-overlays buffer)
	    (erlang-compile-server-print-errors-and-warnings errors))

	   (['rex ['badrpc rpc]] ; something wrong with rpc, is node active?
	    (message "Something wrong with RPC, %s" rpc))

	   (['rex whatever] ; ...
	    (message "This shouldn't have happend: %s" whatever)))))))

(defun erlang-compile-server-get-includes (&optional is-lib)
  "Find the includefiles for an erlang module."
  (let ((inc-regexp (concat "-include" (when is-lib "_lib") "(\"\\([^\)]*\\)"))
	(include-list '())
	(pt (point-min))
	mesf)
    (while (string-match inc-regexp (buffer-string) pt)
      (add-to-list 'include-list (substring (match-string 1) 1))
      (setq pt (match-end 0)))
    include-list))

(defun erlang-compile-server-goto-error (&optional previous)
  "Goto the next error or warning, or if previous is non-nil, goto the previous error or warning."
  (interactive)
  (let ((litem (do ;; initialising
		   ((x 0 (+ x 1))
		    (y (- (length ecs-error-list) 1) (- y 1)))
		   ;; termination-args
		   ((not (or (and (not previous)
				  (and (nth x ecs-error-list)
				       (<= (tuple-elt (nth x ecs-error-list) 1) (line-number-at-pos))))
			     (and previous
				  (and (>= y 0)
				       (nth y ecs-error-list)
				       (>= (tuple-elt (nth y ecs-error-list) 1) (line-number-at-pos))))))
		    ;; do
		    (if (not previous) (nth x ecs-error-list)
		      (when (and (>= y 0) previous) (nth y ecs-error-list)))))))
    ;; goto-error
    (if litem (erlang-compile-server-goto-beginning-of-line (tuple-elt litem 1))
      (when ecs-error-list
	(if previous (erlang-compile-server-goto-beginning-of-line (tuple-elt (car (last ecs-error-list)) 1))
	  (erlang-compile-server-goto-beginning-of-line (tuple-elt (car ecs-error-list) 1)))))))

(defun erlang-compile-server-goto-next-error ()
  (interactive)
  (erlang-compile-server-goto-error))

(defun erlang-compile-server-goto-previous-error ()
  (interactive)
  (erlang-compile-server-goto-error t))

(defvar ecs-error-list '())

(defun erlang-compile-server-print-errors-and-warnings (errors)
  (save-excursion
    (let (tooltip-text)
      (dolist (x errors)
	(progn
	  (setq tooltip-text (format "%s: %s @line %s " (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1)))
	  (erlang-compile-server-make-overlay (tuple-elt x 1)
					      (if (string= (tuple-elt x 2) "error")
						  'erlang-compile-server-error-line
						'erlang-compile-server-warning-line)
					      tooltip-text)
	(erlang-compile-server-message erlang-compile-server-verbose tooltip-text))))))

;;; Overlays

;; Faces for highlighting
(defface erlang-compile-server-error-line
  '((((class color) (background dark)) (:background "Firebrick"))
    (((class color) (background light)) (:background "LightPink1"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'ecs)

(defface erlang-compile-server-warning-line
  '((((class color) (background dark)) (:background "dark blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'ecs)

(defun erlang-compile-server-remove-overlays (buffer)
    (set-buffer buffer)
    (dolist (ol (overlays-in (point-min) (point-max)))
      (when (and (overlayp ol) (overlay-get ol 'erlang-compile-server-overlay))
	(delete-overlay ol))))


(defun erlang-compile-server-goto-beginning-of-line (line-no)
  (goto-line line-no)
  (goto-char (line-beginning-position))
  (while (looking-at "[ \t]")
    (forward-char))
  (point))

(defun erlang-compile-server-make-overlay (line-no face tooltip-text)
  (goto-line line-no)
  (let* ((beg (erlang-compile-server-goto-beginning-of-line line-no))
	 (end (line-end-position)))
  
  (goto-char end)
  (while (and (looking-at "[ \t\r\n]") (> (point) 1))
    (backward-char))
  (setq end (+ 1 (point)))

  (erlang-compile-server-display-overlay beg end face tooltip-text)
))

(defun erlang-compile-server-display-overlay (beg end face tooltip-text)
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face           face)
;      (overlay-put ov 'mouse-face     tooltip-text)
      (overlay-put ov 'help-echo      tooltip-text)
      (overlay-put ov 'erlang-compile-server-overlay  t)
      (overlay-put ov 'priority 100)
      ov))

(provide 'erlang-compile-server)