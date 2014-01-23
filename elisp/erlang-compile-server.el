;;;-------------------------------------------------------------
;;; File    : erlang-compile-server.el
;;; Author  : Sebastian Weddmark Olsson
;;;           github.com/sebastiw
;;; Purpose : Used with distel to mark compilation/xref/dialyzer
;;;           & eunit errors and warnings while writing, in the
;;;           current buffer.
;;; 
;;; Created : June 2012 as an internship at Klarna AB
;;; Comment : Please let me know if you find any bugs or you
;;;           want some feature or something
;;;------------------------------------------------------------
(require 'distel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defvars and initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customization
(defvar erl-ecs-backends '(compiler xref dialyzer eunit)
  "These backends will be activated.")

(defcustom erl-ecs-enable-eunit t
  "If non-nil also checks the eunit tests in the module-file."
  :type '(boolean)
  :group 'erl-ecs)

(defcustom erl-ecs-enable-xref t
  "If non-nil also checks for exported functions that isn't used externally through xref."
  :type '(boolean)
  :group 'erl-ecs)

(defcustom erl-ecs-enable-dialyzer t
  "If non-nil also checks Dialyzer for type warnings."
  :type '(boolean)
  :group 'erl-ecs)

(defcustom erl-ecs-check-on-save t
  "Checks errors and warnings on save."
  :type '(boolean)
  :group 'erl-ecs)

(defcustom erl-ecs-compile-if-ok nil
  "Compiles the module if it doesn't have any errors or warnings."
  :type '(boolean)
  :group 'erl-ecs)

(defcustom erl-ecs-verbose nil
  "Writes output to *Messages* buffer."
  :type '(boolean)
  :group 'erl-ecs)

(defcustom erl-ecs-cut-at-mouseover 100
  "The distance between the newlines for the message when hovering over at an error."
  :type '(number)
  :group 'erl-ecs)
(defcustom erl-ecs-cut-at-distance 200
  "When inserting a newline, this is the distance between the insertions."
  :type '(number)
  :group 'erl-ecs)

(defcustom erl-ecs-check-on-interval nil
  "Checks errors and warnings on given intervals."
  :type '(boolean)
  :group 'erl-ecs)

(defcustom erl-ecs-interval 120
  "Seconds between checks if `erl-ecs-check-on-interval' is set."
  :type '(integer)
  :group 'erl-ecs)
(defvar erl-ecs-timer '()
  "Holds the timer as soon as it is started.")

(defvar erl-ecs-compile-options '()
  "A list of compile options that should be run when testing and compiling.
For more info, check out the variable `erlang-compile-extra-opts'.")

(defvar erl-ecs-compile-includes '()
  "A list of compile include paths that should be run when testing and compiling.")

;;; Error lists
(defvar erl-ecs-error-list '())
(defvar erl-ecs-eunit-list '())
(defvar erl-ecs-xref-list '())
(defvar erl-ecs-dialyzer-list '())
(defvar erl-ecs-user-specified-errors '()
"Must be a list of tuples with lineno, type and an error tuple.
Erlang example: [{35, warning, {err_type, \"There is a cat in the ceiling.\"}}],
Elisp : (list (tuple 35 'warning (tuple err_type \"There is a cat in the ceiling.\"))).")

(defvar erl-ecs-lineno-list '()
  "Short list, specifies which linenumber the errors appear on and what type of error.")

(defvar erl-ecs-before-eval-hooks '())
(defvar erl-ecs-after-eval-hooks '())

;; Faces for highlighting
(defface erl-ecs-error-line
  '((((class color) (background dark)) (:background "Firebrick"))
    (((class color) (background light)) (:background "LightPink1"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'erl-ecs)

(defface erl-ecs-warning-line
  '((((class color) (background dark)) (:background "dark blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'erl-ecs)

(defface erl-ecs-lesser-line
  '((((class color) (background dark)) (:background "dark olive green"))
    (((class color) (background light)) (:background "pale green"))
    (t (:bold t)))
  "Face used for marking lesser warning lines."
  :group 'erl-ecs)

(defface erl-ecs-user-specified-line
  '((((class color) (background dark)) (:background "orange red"))
    (((class color) (background light)) (:background "yellow"))
    (t (:bold t)))
  "Face used for marking lesser warning lines."
  :group 'erl-ecs)

(defvar erl-node-isup nil
  "To track if the node is up or not.")

;; Check that module is loaded else load it
(add-hook 'erl-nodeup-hook 'erl-ecs-check-backend)
(add-hook 'erl-nodedown-hook 'erl-ecs-nodedown)

(defun erl-ecs-check-backend (node _fsm)
  "Reloads 'erlang_compile_server' module to `node'."
  (setq erl-node-isup t)
  (unless distel-inhibit-backend-check
    (progn (erl-ecs-message "ECS: reloading 'erlang_compile_server' onto %s" node)
	   (erl-spawn
	     (erl-send `[rex ,node]
		       `[,erl-self [call
				    code load_file (erlang_compile_server)
				    ,(erl-group-leader)]])
	     (erl-receive (node)
		 ((['rex ['error _]]
		   (&erl-load-backend node))
		  (_ t)))))))

(defun erl-ecs-nodedown (node)
  "Track if the node goes down."
  (setq erl-node-isup))

(defun erl-ecs-setup ()
  (add-hook 'erlang-mode-hook 'erl-ecs-mode-hook)

  ;; for now, just set the enables on each backend
  (unless (member 'xref erl-ecs-backends) (setq erl-ecs-enable-xref nil))
  (unless (member 'eunit erl-ecs-backends) (setq erl-ecs-enable-eunit nil))
  (unless (member 'dialyzer erl-ecs-backends) (setq erl-ecs-enable-dialyzer nil))

  (erl-ecs-message "ECS loaded.")

  (when erl-ecs-check-on-interval (erl-ecs-start-interval)))

(defun erl-ecs-mode-hook ()
  (erl-ecs-mode t)
  (add-hook 'after-save-hook 'erl-ecs-on-save t t))

(defun erl-ecs-on-save ()
  "Check for warnings on save."
  (when erl-ecs-check-on-save (erl-ecs-evaluate)))

(define-minor-mode erl-ecs-mode
  "Extends distel with error evaluation.

Add the following lines to your .emacs file (after distel is initialized):
\(require 'erlang-compile-server)

And then set one or more of the following variables (defaults):
`erl-ecs-backends' (compiler xref dialyzer eunit)

`erl-ecs-check-on-save' (t) - check on save
`erl-ecs-compile-if-ok' (nil) - if no compile fails, compile
`erl-ecs-verbose' (nil) - prints alot of messages

`erl-ecs-check-on-interval' (nil) (not supported yet)
`erl-ecs-interval' (120) (not supported yet)

`erl-ecs-compile-options' (nil) - to specify what extra compile options to be runned
`erl-ecs-compile-includes' (nil) - to specify what compile include paths to be used
`erl-ecs-user-specified-errors' (nil) - specify own errors, must be a list of tuples of lineno, type,and error tuple

For custom colors define the faces:
`erl-ecs-error-line', `erl-ecs-warning-line', `erl-ecs-lesser-line' and `erl-ecs-user-specified-line'

Bindings:
\\[erl-ecs-evaluate] - check for errors/warnings/testfails etc
\\[erl-ecs-next-error] - goto next error
\\[erl-ecs-prev-error] - goto previous error"
  :lighter " ECS"
  :keymap '(("\C-c\C-dq" undefined))
  (if erl-ecs-mode
      (progn (setq erl-ecs-temp-output erl-popup-on-output
		   erl-popup-on-output)
	     (ad-activate-regexp "erl-ecs-.*"))
    (setq erl-popup-on-output erl-ecs-temp-output)
    (ad-deactivate-regexp "erl-ecs-.*")))

(defadvice next-line (after erl-ecs-next-line)
  "Moves point to the next line using `next-line' and then prints the error if there is one."
  (when erl-ecs-mode (erl-ecs-show-error-on-line)))
(defadvice previous-line (after erl-ecs-previous-line)
  "Moves point to previous line and prints error if there is one."
  (when erl-ecs-mode (erl-ecs-show-error-on-line)))
(defadvice forward-paragraph (after erl-ecs-next-paragraph)
  "Next paragraph, then check and print if there is errors."
  (when erl-ecs-mode (erl-ecs-show-error-on-line)))
(defadvice backward-paragraph (after erl-ecs-previous-paragraph)
  "Previous paragraph, check if there is errors."
  (when erl-ecs-mode (erl-ecs-show-error-on-line)))

(defconst erl-ecs-key-binding
  '(("\C-c\C-dq" erl-ecs-evaluate)
    ("\C-c\C-n" erl-ecs-next-error)
    ("\C-c\C-p" erl-ecs-prev-error))
  "Erlang compile server key binding")

;; rebinds the keys
(dolist (k erl-ecs-key-binding) (define-key erl-ecs-mode-map (car k) (cadr k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Main             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erl-ecs-temp-output nil
  "Take care of the original value of the variable `erl-popup-on-output'. This is to keep eunit from popup and leave unwanted output.")
(defvar erl-ecs-have-already-run-once nil
  "If the evaluation function have already been run one time, and you are still not connected, it should stop evaluate.")

(defconst erl-ecs-xref-string "Xref")
(defconst erl-ecs-dialyzer-string "Dialyzer")
(defconst erl-ecs-eunit-string "Eunit")
(defconst erl-ecs-default-string "Erlang")
(defconst erl-ecs-user-specified-string "User specified")

(defun erl-ecs-evaluate ()
  "Checks for errors in current buffer. If this function have been executed and the node wasn't up, you need to connect with distels ping command \\[erl-ping]."
  (interactive)

  (when (and
	 erl-ecs-mode
	 (or (not erl-ecs-have-already-run-once) erl-node-isup))
    (setq erl-ecs-current-buffer (current-buffer)
	  erl-ecs-have-already-run-once t)

      (erl-ecs-message "ECS: Evaluating...")
      (setq erl-ecs-lineno-list '())

      (run-hooks 'erl-ecs-before-eval-hooks)

      ;; seems to only work when recompiled full _plt
      (if erl-ecs-enable-dialyzer (erl-ecs-check-dialyzer)
	(erl-ecs-remove-overlays erl-ecs-dialyzer-string))

      (erl-ecs-check-compile)

      ;; seems to only work when recompiled file
      (if erl-ecs-enable-xref (erl-ecs-check-xref)
	(erl-ecs-remove-overlays erl-ecs-xref-string))

      (if erl-ecs-enable-eunit (erl-ecs-check-eunit)
	(erl-ecs-remove-overlays erl-ecs-eunit-string))

      (if erl-ecs-user-specified-errors
	  (erl-ecs-print-errors 'user-specified-error erl-ecs-user-specified-errors erl-ecs-user-specified-string 'erl-ecs-user-specified-line)
	(erl-ecs-remove-overlays erl-ecs-user-specified-string))

      (run-hooks 'erl-ecs-after-eval-hooks)))

(defun erl-ecs-check-compile (&optional compile-options)
  "Checks for compilation errors and warnings.

Optional parameter `compile-options' should be a list of compile options.
Extra compile options could also be specified by setting the `erl-ecs-compile-options'-variable."
  (erl-ecs-message "ECS: Checking erlang faults.")

  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(incstring (erl-ecs-get-includes))
	(options (or compile-options erl-ecs-compile-options)))

    (erl-ecs-remove-overlays erl-ecs-default-string)

      (if (buffer-modified-p)
	  (let ((bfr-str (buffer-string)))
	    (erl-spawn
	      (erl-send-rpc node 'erlang_compile_server 'get_warnings_from_string (list bfr-str incstring options))
	      (erl-ecs-receive-compile)))
	(erl-spawn
	  (erl-send-rpc node 'erlang_compile_server 'get_warnings (list path incstring options))
	  (erl-ecs-receive-compile)))))

(defun erl-ecs-receive-compile ()
      (erl-receive ()
	  ;; no errors
	  ((['rex ['ok]]
	    (erl-ecs-message "ECS erlang: No faults.")
	    (setq erl-ecs-error-list '())
	    (erl-ecs-delete-items 'compile erl-ecs-lineno-list)
	    (erl-ecs-if-no-compile-faults))
	   
	   ;; errors
	   (['rex ['e errors]]
	    (erl-ecs-message "ECS erlang: faults found.")
	    (setq erl-ecs-error-list errors)
	    (erl-ecs-print-errors 'compile erl-ecs-error-list))
	   
	   (else
	    (erl-ecs-message "ECS erlang unexpected end: %s" else)))))

(defun erl-ecs-if-no-compile-faults ()
  "Compile if compile-if-ok is set."
  (let ((incstring (erl-ecs-get-includes))
	(inclist '())
	tempopts)
    (set-buffer erl-ecs-current-buffer)

    (when (and (not (buffer-modified-p))
	       erl-ecs-compile-if-ok)
      
      (dolist (i incstring inclist) (setq inclist (cons (cons 'i i) inclist)))

      (progn (erl-ecs-message "ECS: Compiling.")
	     (setq tempopts erlang-compile-extra-opts)
	     (setq erlang-compile-extra-opts inclist)
	     (erlang-compile)
	     (setq erlang-compile-extra-opts tempopts)))))

(defun erl-ecs-check-dialyzer ()
  "Checks type and function warnings."
  (erl-ecs-message "ECS: Checking Dialyzer.")

  (let ((path (buffer-file-name))
	(node (erl-target-node)))

    (erl-ecs-remove-overlays erl-ecs-dialyzer-string)

    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'check_dialyzer (list path))
      (erl-receive ()
	  ;; no dialyzer warnings
	  ((['rex ['ok]]
	    (erl-ecs-message "ECS Dialyzer: No warnings.")
	    (setq erl-ecs-dialyzer-list '())
	    (erl-ecs-delete-items 'dialyzer erl-ecs-lineno-list))

	   ;; dialyzer warnings
	   (['rex ['w warnings]]
	    (erl-ecs-message "ECS Dialyzer err at %s." warnings)
	    (setq erl-ecs-dialyzer-list warnings)
	    (erl-ecs-print-errors 'dialyzer erl-ecs-dialyzer-list erl-ecs-dialyzer-string))

	   (else))))))

(defun erl-ecs-check-xref ()
  "Checks for exported function that is not used outside the module."

  (erl-ecs-message "ECS: Checking XREF.")

  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(expline (erl-ecs-find-exportline)))

    (erl-ecs-remove-overlays erl-ecs-xref-string)

    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'xref (list path))
      (erl-receive (expline)
	  ;; no xref warnings
	  ((['rex ['ok]]
	    (erl-ecs-message "ECS XREF: No warnings.")
	    (setq erl-ecs-xref-list '())
	    (erl-ecs-delete-items 'xref erl-ecs-lineno-list))
	   
	   ;; xref warnings
	   (['rex ['w warnings]]
	    (let ((a (list (tuple expline 'warning (tuple 'exported_unused_function warnings)))))
	      (erl-ecs-message "ECS XREF err at %s." a)
	      (setq erl-ecs-xref-list a)
	      (erl-ecs-print-errors 'xref erl-ecs-xref-list erl-ecs-xref-string)))

	   (else))))))

(defun erl-ecs-check-eunit ()
  "Checks eunit tests."

  (erl-ecs-message "ECS: Checking EUNIT.")

  ;; reset eunit errors
  (setq erl-ecs-eunit-list '())
  (erl-ecs-delete-items 'eunit erl-ecs-lineno-list)
  (erl-ecs-remove-overlays erl-ecs-eunit-string)
  
  (let ((node (erl-target-node))
	(path (buffer-name)))
    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'check_eunit (list path erl-self))
      (erl-ecs-eunit-receive))))

(defun erl-ecs-eunit-receive ()
  (erl-receive ()
      ((['ok which]
	(erl-ecs-message "ECS EUNIT ok at %s." which)
	(erl-ecs-eunit-receive))

       (['e error]
	(erl-ecs-message "ECS EUNIT err at %s." error)
	(add-to-list 'erl-ecs-eunit-list error)
	(erl-ecs-eunit-receive))

       (['klar]))

    (erl-ecs-print-errors 'eunit erl-ecs-eunit-list erl-ecs-eunit-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helpers           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-ecs-start-interval ()
  "Defines a timer that runs (erl-ecs-evaluate) every `erl-ecs-interval' second."
  (interactive)
  (erl-ecs-stop-interval)
  (setq erl-ecs-timer (run-at-time (format "%s sec" erl-ecs-interval) erl-ecs-interval 'erl-ecs-evaluate)))

(defun erl-ecs-stop-interval ()
  "Stops the timer."
  (interactive)
  (when erl-ecs-timer (cancel-timer erl-ecs-timer))
  (setq erl-ecs-timer))

(defun erl-ecs-get-includes ()
  "Find the includefiles for an erlang module."
  (save-excursion
    (set-buffer erl-ecs-current-buffer)
    (let ((inc-regexp (concat "^-include\\(_lib\\)?(\"\\([^\)]*\\)"))
	  (include-list erl-ecs-compile-includes)
	  (pt (point-min)))
      (while (string-match inc-regexp (buffer-string) pt)
	(add-to-list 'include-list (file-name-directory (substring (match-string 2) 1)))
	(setq pt (match-end 0)))
      include-list)))

(defun erl-ecs-find-exportline ()
  "Find out which line the exports are made."
  (save-excursion
    (set-buffer erl-ecs-current-buffer)
    (goto-char (or (string-match "^-export\\|^-compile" (buffer-string))
		   (point-min)))
    (line-number-at-pos (forward-char))))

(defun erl-ecs-remove-overlays (which)
  "Removes all overlays with the name `WHICH'"
  (interactive)
  (set-buffer erl-ecs-current-buffer)
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (and (overlayp ol)
	       (eql (overlay-get ol 'erl-ecs-type) which))
      (delete-overlay ol))))

(defun erl-ecs-goto-beg-of-line (line-no)
  "Return the beginning of the line without whitespaces."
  (save-excursion
    (goto-line line-no)
    (goto-char (line-beginning-position))
    (while (looking-at "[ \t]")
      (forward-char))
    (point)))

(defun erl-ecs-goto-end-of-line (line-no)
  "Return the end of the line without whitespaces"
  (save-excursion
    (goto-line line-no)
    (goto-char (line-end-position))
    (while (and (looking-at "[ \t\r\n]") (> (point) 1))
      (backward-char))
    (+ 1 (point))))

(defun erl-ecs-print-errors (tag errors &optional lesser lesser-face)
  "Makes the overlays."
  (set-buffer erl-ecs-current-buffer)
  (let ((err-list '()))
    (dolist (x errors err-list)
	(setq err-list (cons (vector (tuple-elt x 1) tag) err-list))

	(erl-ecs-display-overlay
	 (erl-ecs-goto-beg-of-line (tuple-elt x 1)) ; beginning
	 (erl-ecs-goto-end-of-line (tuple-elt x 1)) ; end
	 (if (not lesser) ; face
	     (if (string= (tuple-elt x 2) "error")
		 'erl-ecs-error-line
	       'erl-ecs-warning-line)
	   (if lesser-face lesser-face 'erl-ecs-lesser-line))
	 (format "%s %s @line %s: %s "
		 (if lesser lesser erl-ecs-default-string)
		 (tuple-elt x 2)
		 (tuple-elt x 1)
		 (tuple-elt x 3)) ; help-echo
	 lesser) ;; lesser-type
	err-list)
    (setq erl-ecs-lineno-list (append erl-ecs-lineno-list err-list))))

(defun erl-ecs-display-overlay (beg end face tooltip-text &optional lesser)
  "Display the overlays."
  (let ((ov (make-overlay beg end nil t t)))
    (overlay-put ov 'erl-ecs t)
    (overlay-put ov 'face face)
    (overlay-put ov 'help-echo (erl-ecs-add-newline tooltip-text erl-ecs-cut-at-mouseover))
    (overlay-put ov 'erl-ecs-type (if lesser lesser
			    erl-ecs-default-string))
    (overlay-put ov 'priority (if lesser 90 100))
    ov))

(defun erl-ecs-add-newline (string distance)
  "Adds a newline at every `distance' chars in `string'."
    (if (> (length string) distance)
	(concat (substring string 0 distance)
		"\n" (erl-ecs-add-newline (substring string distance) distance))
      string))

(defun erl-ecs-goto-error (&optional prev pos)
  "Leave point at the next error and print the message. If `prev' is set goto the previous error. If `pos' is set, goto that line."
  (let* ((delta (line-number-at-pos (if prev (point-min) (point-max))))
	(pt (line-number-at-pos pos))
	(min-max delta)
	(comparison delta))
    (dolist (it erl-ecs-lineno-list delta)
      ;; set the first/last err
      (when (or (and prev
		     (> (elt it 0) min-max))
		(and (not prev)
		     (< (elt it 0) min-max)))
	(setq min-max (elt it 0)))
      ;; set the next/prev error
      (when (or (and (not prev)
		     (> (elt it 0) pt)
		     (< (elt it 0) delta))
		(and prev
		     (< (elt it 0) pt)
		     (> (elt it 0) delta)))
	(setq delta (elt it 0))))
    (if (= delta comparison) min-max delta)))

(defun erl-ecs-next-error ()
  "Moves point to the next error and prints the error."
  (interactive)
  (let* ((max (line-number-at-pos (point-max)))
	 (next-err-line (erl-ecs-goto-error)))
	  (when erl-ecs-lineno-list
	    (goto-char
	     (erl-ecs-goto-beg-of-line
	      next-err-line)))
    (erl-ecs-show-error-on-line)))

(defun erl-ecs-prev-error ()
  "Moves point to the previous error and prints the error."
  (interactive)
  (let* ((min (line-number-at-pos (point-min)))
	 (prev-err-line (erl-ecs-goto-error t)))
	  (when erl-ecs-lineno-list
	    (goto-char
	     (erl-ecs-goto-beg-of-line
		prev-err-line)))
    (erl-ecs-show-error-on-line)))

(defun erl-ecs-show-error-on-line (&optional line)
  "Prints the errors for a certain line. If `line' is nil use the current line."
  (interactive)
  (let (str)
    (dolist (ov (overlays-at (point)) str)
      (let ((help-echo (overlay-get ov 'help-echo)))
	(when (overlay-get ov 'erl-ecs-type)
	  (setq str (concat
		     (replace-regexp-in-string "\n" ""
		      (substring help-echo 0
		       (when (> (length help-echo) erl-ecs-cut-at-distance)
			 erl-ecs-cut-at-distance)))
		     (when (> (length help-echo) erl-ecs-cut-at-distance) "...")
		     (when str (concat "\n" str)))))))
    (when str (message str))))

(defun erl-ecs-message (msg &rest r)
  "Prints message if `erl-ecs-verbose' is non-nil."
  (when erl-ecs-verbose (message msg r)))

(defun erl-ecs-delete-items (tag erl-ecs-list)
 "Deletes all items that has a value matching `TAG' from a list"
 (let ((new-list '()))
   (dolist (it erl-ecs-list new-list)
       (unless (equal tag (elt it 1)) (setq new-list (cons it new-list))))
   (setq erl-ecs-lineno-list new-list)))

(defun erl-ecs-check-backends ()
  "See which backends should be used."
  (interactive)
  (unless (member 'xref erl-ecs-backends)
    (erl-ecs-message "xref not set")
    (setq erl-ecs-enable-xref nil))
  (unless (member 'eunit erl-ecs-backends)
    (erl-ecs-message "eunit not set")
    (setq erl-ecs-enable-eunit nil))
  (unless (member 'dialyzer erl-ecs-backends)
    (erl-ecs-message "dialyzer not set")
    (setq erl-ecs-enable-dialyzer nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            TODO            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Last             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'erl-ecs-before-eval-hooks 'erl-ecs-check-backends)

(erl-ecs-setup)

(provide 'erlang-compile-server)
