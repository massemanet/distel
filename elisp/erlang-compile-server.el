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

(defvar erl-ecs-enable-eunit t
"If non-nil also checks the eunit tests in the module-file.")

(defvar erl-ecs-enable-xref t
"If non-nil also checks for exported functions that isn't used externally through xref.")

(defvar erl-ecs-enable-dialyzer t
"If non-nil also checks Dialyzer for type warnings.")

(defvar erl-ecs-check-on-save t
"Checks errors and warnings on save.")

(defvar erl-ecs-compile-if-ok nil
"Compiles the module if it doesn't have any errors or warnings.")

(defvar erl-ecs-verbose nil
"Writes output to *Messages* buffer.")

(defvar erl-ecs-check-on-interval nil
"Checks errors and warnings on given intervals.")

(defvar erl-ecs-interval 120
"Seconds between checks if `erl-ecs-check-on-interval' is set.")

(defvar erl-ecs-compile-options '()
"A list of compile options that should be run when testing and compiling.
For more info, check out the variable `erlang-compile-extra-opts'.")

;;; Error lists
(defvar erl-ecs-error-list '())
(defvar erl-ecs-eunit-list '())
(defvar erl-ecs-xref-list '())
(defvar erl-ecs-dialyzer-list '())
(defvar erl-ecs-user-specified-errors '()
"Must be a list of tuples with lineno, type and an error tuple.
Erlang example: [{35, warning, {err_type, \"There is a cat in the ceiling.\"}}],
Elisp : (list (tuple 35 'warning (tuple err_type \"There is a cat in the ceiling.\"))).")

(defvar erl-ecs-lineno-list '())
(defvar erl-ecs-error-index 0)

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

(defvar erl-node-isup nil)

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
      (setq erl-ecs-temp-output erl-popup-on-output
	    erl-popup-on-output)
    (setq erl-popup-on-output erl-ecs-temp-output)))

(defconst erl-ecs-key-binding
  '(("\C-c\C-dq" erl-ecs-evaluate)
    ("\C-c\C-n" erl-ecs-next-error)
    ("\C-c\C-p" erl-ecs-prev-error)
    ("\C-n" erl-ecs-next-line)
    ("\C-p" erl-ecs-prev-line))
  "Erlang compile server key binding")

(dolist (k erl-ecs-key-binding) (define-key erl-ecs-mode-map (car k) (cadr k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Main             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar erl-ecs-temp-output nil)
(defvar erl-ecs-have-already-run-once nil)

(defun erl-ecs-evaluate ()
  "Checks for errors in current buffer. If this function have been executed and the node wasn't up, you need to connect with distels ping command \\[erl-ping]."
  (interactive)

  (when (or (not erl-ecs-have-already-run-once) erl-node-isup)
    (setq erl-ecs-current-buffer (current-buffer)
	  erl-ecs-have-already-run-once t)

      (erl-ecs-message "ECS: Evaluating...")
      (setq erl-ecs-lineno-list '())

      (run-hooks 'erl-ecs-before-eval-hooks)

      ;; seems to only work when recompiled full _plt
      (if erl-ecs-enable-dialyzer (erl-ecs-check-dialyzer)
	(erl-ecs-remove-overlays 'erl-ecs-dialyzer-overlay))

      (erl-ecs-check-compile)

      ;; seems to only work when recompiled file
      (if erl-ecs-enable-xref (erl-ecs-check-xref)
	(erl-ecs-remove-overlays 'erl-ecs-xref-overlay))

      (if erl-ecs-enable-eunit (erl-ecs-check-eunit)
	(erl-ecs-remove-overlays 'erl-ecs-eunit-overlay))

      (if erl-ecs-user-specified-errors
	  (erl-ecs-print-errors 'user-specified-error erl-ecs-user-specified-errors 'erl-ecs-user-spec-overlay 'erl-ecs-user-specified-line)
	(erl-ecs-remove-overlays 'erl-ecs-user-spec-overlay))

      (run-hooks 'erl-ecs-after-eval-hooks))
)

(defun erl-ecs-check-compile (&optional compile-options)
  "Checks for compilation errors and warnings.

Optional parameter `compile-options' should be a list of compile options.
Extra compile options could also be specified by setting the `erl-ecs-compile-options'-variable."
  (erl-ecs-message "ECS: Checking erlang faults.")

  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(incstring (erl-ecs-get-includes))
	(options (or compile-options erl-ecs-compile-options)))

    (erl-ecs-remove-overlays 'erl-ecs-overlay)

    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'get_warnings (list path incstring options))
      
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
	    (erl-ecs-message "ECS erlang unexpected end: %s" else)))))))

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

    (erl-ecs-remove-overlays 'erl-ecs-dialyzer-overlay)

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
	    (erl-ecs-print-errors 'dialyzer erl-ecs-dialyzer-list 'erl-ecs-dialyzer-overlay))

	   (else))))))

(defun erl-ecs-check-xref ()
  "Checks for exported function that is not used outside the module."

  (erl-ecs-message "ECS: Checking XREF.")

  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(expline (erl-ecs-find-exportline)))

    (erl-ecs-remove-overlays 'erl-ecs-xref-overlay)

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
	      (erl-ecs-print-errors 'xref erl-ecs-xref-list 'erl-ecs-xref-overlay)))

	   (else))))))

(defun erl-ecs-check-eunit ()
  "Checks eunit tests."

  (erl-ecs-message "ECS: Checking EUNIT.")

  ;; reset eunit errors
  (setq erl-ecs-eunit-list '())
  (erl-ecs-delete-items 'eunit erl-ecs-lineno-list)
  (erl-ecs-remove-overlays 'erl-ecs-eunit-overlay)
  
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

    (erl-ecs-print-errors 'eunit erl-ecs-eunit-list 'erl-ecs-eunit-overlay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helpers           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-ecs-get-includes ()
  "Find the includefiles for an erlang module."
  (save-excursion
    (set-buffer erl-ecs-current-buffer)
    (let ((inc-regexp (concat "^-include\\(_lib\\)?(\"\\([^\)]*\\)"))
	  (include-list '())
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
	       (overlay-get ol which))
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
      (progn
	(setq err-list (cons (vector (tuple-elt x 1) tag) err-list))

	(erl-ecs-display-overlay
	 (erl-ecs-goto-beg-of-line (tuple-elt x 1))
	 (erl-ecs-goto-end-of-line (tuple-elt x 1))
	 (if (not lesser)
	     (if (string= (tuple-elt x 2) "error")
		 'erl-ecs-error-line
	       'erl-ecs-warning-line)
	   (if lesser-face lesser-face 'erl-ecs-lesser-line))
	 (format "%s: %s @line %s " (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1))
	 lesser)) err-list)
    (setq erl-ecs-lineno-list (append erl-ecs-lineno-list err-list))))

(defun erl-ecs-display-overlay (beg end face tooltip-text &optional lesser)
  "Display the overlays."
  (let ((ov (make-overlay beg end nil t t)))
    (overlay-put ov 'face face)
    (overlay-put ov 'help-echo tooltip-text)
    (overlay-put ov (if lesser
			lesser
			'erl-ecs-overlay) t)
    (overlay-put ov 'priority (if (not lesser) 100 90))
    ov))

(defun erl-ecs-goto-error (&optional prev pos)
  (let* ((delta (line-number-at-pos (if prev (point-min) (point-max))))
	(pt (line-number-at-pos pos))
	(skip-counter 0)
	(min-max delta)
	(comparison delta))
    (dolist (it erl-ecs-lineno-list delta)
      ;; set the first/last err
      (when (or (and prev
		     (> (elt it 0) min-max))
		(and (not prev)
		     (< (elt it 0) min-max)))
	(setq min-max (elt it 0)))
      ;; set how many errors on the given line
      (when (= (elt it 0) pt) (setq skip-counter (+ 1 skip-counter)))
      ;; set the next/prev error
      (when (or (and (not prev)
		     (> (elt it 0) pt)
		     (< (elt it 0) delta))
		(and prev
		     (< (elt it 0) pt)
		     (> (elt it 0) delta)))
	(setq delta (elt it 0))))
      (cons (if (= delta comparison) min-max delta) skip-counter)))

(defun erl-ecs-next-error ()
  "Moves point to the next error and prints the error."
  (interactive)
  (let* ((max (line-number-at-pos (point-max)))
	 (next-pair (erl-ecs-goto-error))
	 (next-err-line (car next-pair))
	 (total-errs-on-line (cdr next-pair)))

    (if (>= erl-ecs-error-index total-errs-on-line)
	(progn
	  (setq erl-ecs-error-index 1)
	  (goto-char
	   (erl-ecs-goto-beg-of-line
	      next-err-line)))
      (setq erl-ecs-error-index (+ 1 erl-ecs-error-index)))
    
    (erl-ecs-show-error-on-line)))

(defun erl-ecs-prev-error ()
  "Moves point to the previous error and prints the error."
  (interactive)
  (let* ((min (line-number-at-pos (point-min)))
	(prev-pair (erl-ecs-goto-error t))
	(prev-err-line (car prev-pair))
	(total-errs-on-line (cdr prev-pair)))

    (if (>= erl-ecs-error-index total-errs-on-line)
	(progn
	  (setq erl-ecs-error-index 1)
	  (goto-char
	   (erl-ecs-goto-beg-of-line
	    (if (= prev-err-line min)
		(erl-ecs-goto-error t (point-max))
	      prev-err-line))))
      (setq erl-ecs-error-index (+ erl-ecs-error-index 1)))

    (erl-ecs-show-error-on-line)))

(defun erl-ecs-next-line ()
  "Moves point to the next line using `next-line' and then prints the error if there is one."
  (interactive)
  (next-line)
  (erl-ecs-show-error-on-line))

(defun erl-ecs-prev-line ()
  (interactive)
  (previous-line)
  (erl-ecs-show-error-on-line))

(defun erl-ecs-show-error-on-line (&optional line)
  "Prints the error for a certain line. If more than one error, prints the one given by `erl-ecs-error-index'."
  (interactive)
  (let* ((line (or line (line-number-at-pos)))
	 (tag (erl-ecs-get-tag line))
	 (ret (cond ((equal tag 'compile)
		     (erl-ecs-get-item-match line erl-ecs-error-list 1))
		    ((equal tag 'eunit)
		     (erl-ecs-get-item-match line erl-ecs-eunit-list 1))
		    ((equal tag 'xref)
		     (erl-ecs-get-item-match line erl-ecs-xref-list 1))
		    ((equal tag 'dialyzer)
		     (erl-ecs-get-item-match line erl-ecs-dialyzer-list 1))
		    ((equal tag 'user-specified-error)
		     (erl-ecs-get-item-match line erl-ecs-user-specified-errors 1)))))
    (when ret (message "%s %s" tag (erl-ecs-format-output ret)))))

(defun erl-ecs-get-tag (line)
  (let ((skip 0)
	tag)
    (dolist (it erl-ecs-lineno-list tag)
      (when (equal (elt it 0) line)
	(setq skip (+ 1 skip))
	(when (= skip erl-ecs-error-index)
	  (setq tag (elt it 1)))))))

(defun erl-ecs-get-item-match (match lista vectpos)
  (let (item)
    (dolist (it lista item) (when (equal (tuple-elt it vectpos) match) (setq item (tuple-elt it 3))))))

(defun erl-ecs-format-output (msg)
  (replace-regexp-in-string "\n" "<newline>" (format "%s: %s" (elt msg 0) (elt msg 1))))

(defun erl-ecs-message (msg &rest r)
  (when erl-ecs-verbose (message msg r)))

(defun erl-ecs-delete-items (tag erl-ecs-list)
 "Deletes all items that has a value matching `TAG' from a list"
 (let ((new-list '()))
   (dolist (it erl-ecs-list new-list)
       (unless (equal tag (elt it 1)) (setq new-list (cons it new-list))))
   (setq erl-ecs-lineno-list new-list)))

(defun erl-ecs-check-backends ()
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

(defun erl-ecs-start-interval ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Last             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'erl-ecs-before-eval-hooks 'erl-ecs-check-backends)

(erl-ecs-setup)

(provide 'erlang-compile-server)
