;;;; variables for customization
; erlang-compile-server-check-on-save default t
; erlang-compile-server-compile-if-ok default t

(require 'distel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang Compile Server ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (if erlang-compile-server-check-on-save
      (add-hook 'after-save-hook 'erlang-compile-server-check-file-ending))
  (add-hook 'erlang-mode-hook 'erlang-compile-server-mode-hook)
  (add-to-list 'minor-mode-alist
	       '(erlang-compile-server-mode
		 " ECS"))
;  (if (not (null erlang-compile-server-check-on-interval))
;      (erlang-compile-server-start-interval))
)

(defun erlang-compile-server-start-interval ()
;  (while t
;  (if (buffer-modified-p) (erlang-compile-server-check-for-errors-and-warnings))
;  (sleep-for 'erlang-compile-server-interval)
;  (message "Checking..."))
)

(defvar erlang-compile-server-check-on-interval t
"Checks errors and warnings on given intervals.")

(defvar erlang-compile-server-check-on-save t
"Checks errors and warnings on save.")

(defvar erlang-compile-server-compile-if-ok t
"Compiles the module if it doesn't have any errors or warnings.")

(defvar erlang-compile-server-verbose t
"Writes output to *Messages* buffer.")

(defvar erlang-compile-server-interval 10
"Seconds between checks if `erlang-compile-server-check-on-interval' is set.")

(defun erlang-compile-server-mode-hook ()
  (erlang-compile-server-mode t))

(define-minor-mode erlang-compile-server-mode
  "Erlang compile server"
  nil
  nil
  '(("\C-x\C-a" 'undefined)))

;'(("\C-x\C-a" erlang-compile-server-check-for-errors-and-warnings)))

(define-key erlang-compile-server-mode-map "\C-x\C-a" 'erlang-compile-server-check-for-errors-and-warnings)

;; On save, check for errors etc
(defun erlang-compile-server-check-file-ending()
   (let ((path (buffer-file-name)))
   (if (string= (substring path (- 0 (length ".erl"))) ".erl") (erlang-compile-server-check-for-errors-and-warnings)))
)

(defun erlang-compile-server-message (var msg &rest rest)
  "Prints a message if a given variable or expression is non-nil."
  (if var (message msg rest))
)

;;; Main function
(defun erlang-compile-server-check-for-errors-and-warnings()
  (interactive)
  (inferior-erlang-prepare-for-input t)
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
	    (erlang-compile-server-remove-overlays buffer)
	    (erlang-compile-server-message erlang-compile-server-verbose "Ok.")
	    (when (and (not (buffer-modified-p))
		       erlang-compile-server-compile-if-ok)
	      (progn (erlang-compile-server-message erlang-compile-server-verbose "Compiling.")
		     (setq tempopts erlang-compile-extra-opts)
		     (setq erlang-compile-extra-opts incstring)
		     (erlang-compile)
		     (setq erlang-compile-extra-opts tempopts))))
	   (['rex ['w warnings]] ; only warnings
	    (erlang-compile-server-remove-overlays buffer)
	    (erlang-compile-server-print-errors-and-warnings () warnings))
	   (['rex ['e errors warnings]] ; errors and possibly warnings
	    (erlang-compile-server-remove-overlays buffer)
	    (erlang-compile-server-print-errors-and-warnings errors warnings))
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
      (setq pt (match-end 0))
      )
    include-list))

(defun erlang-compile-server-print-errors-and-warnings(errors warnings)
  (save-excursion
    (let ((err (if errors (cadr (tuple-to-list (tuple-elt errors 1))) nil))
	  (war (if warnings (cadr (tuple-to-list (tuple-elt warnings 1))) nil))
	  tooltip-text
	  x)
      (while err ; errors
	(setq x (pop err)
	      tooltip-text (format "%s error: %s @line %s " (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1)))
	(erlang-compile-server-make-overlay (tuple-elt x 1) 'erlang-compile-server-error-line tooltip-text)
	(erlang-compile-server-message
	 erlang-compile-server-verbose tooltip-text))
      (while war ; warnings
	(setq x (pop war)
	      tooltip-text (format "%s warning: %s @line %s" (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1)))
	(erlang-compile-server-make-overlay (tuple-elt x 1) 'erlang-compile-server-warning-line tooltip-text)
	(erlang-compile-server-message
	 erlang-compile-server-verbose tooltip-text)))))

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

(defun erlang-compile-server-make-overlay (line-no face tooltip-text)
  (goto-line line-no)
  (let* ((line-beg (flymake-line-beginning-position))
	 (line-end (flymake-line-end-position))
	 (beg      line-beg)
	 (end      line-end))

  (goto-char line-beg)
  (while (looking-at "[ \t]")
      (forward-char))
  (setq beg (point))
  
  (goto-char line-end)
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

(defalias 'flymake-line-beginning-position
  (if (fboundp 'line-beginning-position)
      'line-beginning-position
    (lambda (&optional arg) (save-excursion (beginning-of-line arg) (point)))))

(defalias 'flymake-line-end-position
  (if (fboundp 'line-end-position)
      'line-end-position
    (lambda (&optional arg) (save-excursion (end-of-line arg) (point)))))

(provide 'erlang-compile-server)