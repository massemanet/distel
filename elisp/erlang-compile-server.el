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

(defvar erlang-compile-server-enable-eunit nil
"If non-nil also checks the eunit tests in the module-file.")

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
;;;  (if (buffer-modified-p) (erlang-compile-server-check-compile))
;;;  (sleep-for 'erlang-compile-server-interval)
;;;  (message "Checking..."))
)

(defun erlang-compile-server-mode-hook ()
  (erlang-compile-server-mode t))

(define-minor-mode erlang-compile-server-mode
  "Extends distel with error evaluation and eunit testing.

Add the following lines to your .emacs file (after distel is initialized):
\(require 'erlang-compile-server)
\(erlang-compile-server-setup)

And then set one or more of the following variables (defaults):
 erlang-compile-server-check-on-save (t)
 erlang-compile-server-check-on-interval (nil) (not supported yet)
 erlang-compile-server-compile-if-ok (nil)
 erlang-compile-server-verbose (nil)
 erlang-compile-server-interval (120) (not supported yet)
 erlang-compile-server-enable-eunit (nil)

\\[erlang-compile-server-check-compile] - check for compile errors/warnings
\\[erlang-compile-server-check-eunit] - check that eunit tests run
\\[erlang-compile-server-goto-next-error] - goto next compile error
\\[erlang-compile-server-goto-previous-error] - goto previous compile error"
  nil
  nil
  '(("\C-x\C-a" 'undefined)))

(defconst ecs-key-binding
  '(("\C-x\C-a" erlang-compile-server-check-compile)
    ("\C-c\C-n" erlang-compile-server-goto-next-error)
    ("\C-c\C-p" erlang-compile-server-goto-previous-error)
    ("\C-x\C-q" erlang-compile-server-check-eunit))
  "Erlang compile server key binding")

(dolist (k ecs-key-binding) (define-key erlang-compile-server-mode-map (car k) (cadr k)))

;; On save, check for errors etc
(defun erlang-compile-server-check-file-ending()
  "Checks that it is a valid erlang file, this because of the after-save-hook."
  (let ((path (buffer-file-name)))
    (if (string= (substring path (- 0 (length ".erl"))) ".erl") (erlang-compile-server-check-compile))))

(defun erlang-compile-server-message (var msg &rest rest)
  "Prints a message if a given variable or expression is non-nil."
  (when var (message msg rest))
)

(defvar ecs-node nil)

;;; Main function
(defun erlang-compile-server-check-compile ()
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
	  (erl-send-rpc node 'erlang_compile_server 'get_warnings_from_string (list (buffer-string) incstring))
	(erl-send-rpc node 'erlang_compile_server 'get_warnings (list path incstring)))

      (erl-receive (buffer incstring)
	  ((['rex ['ok]] ; no errors
	    (setq ecs-error-list '())
	    (erlang-compile-server-remove-overlays buffer 'erlang-compile-server-overlay)
	    (erlang-compile-server-message erlang-compile-server-verbose "Ok, no compile errors/warnings.")

	    ;; compiling stuff
	    (when (and (not (buffer-modified-p))
		       erlang-compile-server-compile-if-ok)
	      (progn (erlang-compile-server-message erlang-compile-server-verbose "Compiling.")
		     (setq tempopts erlang-compile-extra-opts)
		     (setq erlang-compile-extra-opts incstring)
		     (erlang-compile)
		     (setq erlang-compile-extra-opts tempopts)))

	   ;; eunit
	    (when erlang-compile-server-enable-eunit (erlang-compile-server-check-eunit))
	    )
	   (['rex ['w warnings]] ; only warnings
	    (set-buffer buffer)
	    (setq ecs-error-list warnings)
	    (erlang-compile-server-remove-overlays buffer 'erlang-compile-server-overlay)
	    (erlang-compile-server-print-errors-and-warnings warnings))

	   (['rex ['e errors]] ; errors and possibly warnings
	    (set-buffer buffer)
	    (setq ecs-error-list errors)
	    (erlang-compile-server-remove-overlays buffer 'erlang-compile-server-overlay)
	    (erlang-compile-server-print-errors-and-warnings errors))

	   (['rex ['badrpc rpc]] ; something wrong with rpc, is node active?
	    (message "Something wrong with RPC, %s" rpc))

	   (['rex whatever] ; ...
	    (message "This shouldn't have happend: %s" whatever)))))))


(defun erlang-compile-server-check-eunit ()
  (interactive)
  (setq ecs-current-buffer (current-buffer))
  (setq ecs-eunit-list '())
  (save-excursion
    (erlang-compile-server-remove-overlays ecs-current-buffer 'erlang-compile-server-eunit-overlay)
    (let ((node (erl-target-node))
	  (path (buffer-name)))
      (erl-spawn
	(erl-send-rpc node 'erlang_compile_server 'check_eunit (list path erl-self))
	(erlang-compile-server-eunit-receive-loop)))))

(defun erlang-compile-server-eunit-receive-loop ()
  (erl-receive ()
      ((['ok which] ;; kommer inte handa da de inte skickas fran noden, men valdigt latt att implementera
	(erlang-compile-server-message erlang-compile-server-verbose (format "%s is ok." which))
	(erlang-compile-server-eunit-receive-loop))
       (['e [what line where]]
	(add-to-list 'ecs-eunit-list (tuple line what where))
	(erlang-compile-server-eunit-receive-loop))
       (['klar]
	(erlang-compile-server-message erlang-compile-server-verbose "Done testing eunit.")))
    (when ecs-current-buffer (erlang-compile-server-print-eunit-errors ecs-current-buffer))))

(defun erlang-compile-server-print-eunit-errors (buffer)
  (save-excursion
    (set-buffer buffer)
    (erlang-compile-server-print-errors-and-warnings ecs-eunit-list t)))

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
(defvar ecs-eunit-list '())

(defun erlang-compile-server-print-errors-and-warnings (errors &optional eunit)
  (save-excursion
    (let (tooltip-text)
      (dolist (x errors)
	(progn
	  (setq tooltip-text (format "%s: %s @line %s " (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1)))
	  (erlang-compile-server-make-overlay (tuple-elt x 1)
					      (if (not eunit) (if (string= (tuple-elt x 2) "error")
								  'erlang-compile-server-error-line
								'erlang-compile-server-warning-line)
						'erlang-compile-server-eunit-line)
					      tooltip-text
					      (when eunit t))
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

(defface erlang-compile-server-eunit-line
  '((((class color) (background dark)) (:background "green"))
    (((class color) (background light)) (:background "pale green"))
    (t (:bold t)))
  "Face used for marking eunit lines."
  :group 'ecs)

(defun erlang-compile-server-remove-overlays (buffer which)
    (set-buffer buffer)
    (dolist (ol (overlays-in (point-min) (point-max)))
      (when (and (overlayp ol) (overlay-get ol which))
	(delete-overlay ol))))


(defun erlang-compile-server-goto-beginning-of-line (line-no)
  (goto-line line-no)
  (goto-char (line-beginning-position))
  (while (looking-at "[ \t]")
    (forward-char))
  (point))

(defun erlang-compile-server-make-overlay (line-no face tooltip-text which)
  (goto-line line-no)
  (let* ((beg (erlang-compile-server-goto-beginning-of-line line-no))
	 (end (line-end-position)))
  
  (goto-char end)
  (while (and (looking-at "[ \t\r\n]") (> (point) 1))
    (backward-char))
  (setq end (+ 1 (point)))

  (erlang-compile-server-display-overlay beg end face tooltip-text which)
))

(defun erlang-compile-server-display-overlay (beg end face tooltip-text &optional eunit)
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face           face)
;      (overlay-put ov 'mouse-face     tooltip-text)
      (overlay-put ov 'help-echo      tooltip-text)
      (overlay-put ov (if (not eunit) 'erlang-compile-server-overlay
			'erlang-compile-server-eunit-overlay) t)
      (overlay-put ov 'priority (if (not eunit) 100 90))
      ov))

(provide 'erlang-compile-server)