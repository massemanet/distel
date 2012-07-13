(require 'distel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defvars and initialization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customization
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

;;; Error lists
(defvar erl-ecs-error-list '())
(defvar erl-ecs-eunit-list '())
(defvar erl-ecs-xref-list '())
(defvar erl-ecs-dialyzer-list '())

(defvar erl-ecs-lineno-list '())

;; Faces for highlighting
(defface erl-ecs-error-line
  '((((class color) (background dark)) (:background "Firebrick"))
    (((class color) (background light)) (:background "LightPink1"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'ecs)

(defface erl-ecs-warning-line
  '((((class color) (background dark)) (:background "dark blue"))
    (((class color) (background light)) (:background "light blue"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'ecs)

(defface erl-ecs-lesser-line
  '((((class color) (background dark)) (:background "green"))
    (((class color) (background light)) (:background "pale green"))
    (t (:bold t)))
  "Face used for marking lesser warning lines."
  :group 'ecs)

;; Check that module is loaded else load it
(add-hook 'erl-nodeup-hook 'ecs-check-backend)

(defun erl-ecs-check-backend (node _fsm)
  "Reloads 'erlang_compile_server' module to `node'."
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

(defun erl-ecs-setup ()
  (add-hook 'erlang-mode-hook 'erl-ecs-mode-hook)

  (erl-ecs-message "ECS loaded.")

  (add-to-list 'minor-mode-alist
	       '(erl-ecs-mode
		 " ECS"))

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
erl-ecs-enable-eunit (t)
erl-ecs-enable-xref (t)
erl-ecs-enable-dialyzer (t)

erl-ecs-check-on-save (t)
erl-ecs-compile-if-ok (nil)
erl-ecs-verbose (nil)

erl-ecs-check-on-interval (nil) (not supported yet)
erl-ecs-interval (120) (not supported yet)

\\[erl-ecs-evaluate] - check for errors/warnings/testfails etc
\\[erl-ecs-next-error] - goto next error
\\[erl-ecs-prev-error] - goto previous error"
  nil
  nil
  '(("\C-x\C-a" 'undefined)))

(defconst erl-ecs-key-binding
  '(("\C-C\C-dq" erl-ecs-evaluate)
    ("\C-c\C-n" erl-ecs-next-error)
    ("\C-c\C-p" erl-ecs-prev-error))
  "Erlang compile server key binding")

(dolist (k erl-ecs-key-binding) (define-key erl-ecs-mode-map (car k) (cadr k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           Main             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-ecs-evaluate ()
  (interactive)
  (erl-ecs-message "ECS: Evaluating...")
  (setq erl-ecs-lineno-list '())

  ;; seems to only work when recompiled full _plt
  (when erl-ecs-enable-dialyzer (erl-ecs-check-dialyzer))
  (erl-ecs-check-compile)

  ;; seems to only work when recompiled file
  (when erl-ecs-enable-xref (erl-ecs-check-xref))
  (when erl-ecs-enable-eunit (erl-ecs-check-eunit)))

(defun erl-ecs-check-compile ()
  "Checks for compilation errors and warnings."
  (interactive)
  (setq erl-ecs-current-buffer (current-buffer))
  (erl-ecs-message "ECS: Checking erlang faults.")

  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(incstring (erl-ecs-get-includes)))
      
    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'get_warnings (list path incstring))
      
      (erl-receive ()
	  ;; no errors
	  ((['rex ['ok]]
	    (erl-ecs-message "ECS erlang: No faults.")
	    (erl-ecs-delete-items 'compile erl-ecs-lineno-list)
	    (setq erl-ecs-error-list '())
	    (erl-ecs-remove-overlays 'erl-ecs-overlay)
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
	tempopts)
    (set-buffer erl-ecs-current-buffer)

    (when (and (not (buffer-modified-p))
	       erl-ecs-compile-if-ok)
      
      (progn (erl-ecs-message "ECS: Compiling.")
	     (setq tempopts erlang-compile-extra-opts)
	     (setq erlang-compile-extra-opts incstring)
	     (erlang-compile)
	     (setq erlang-compile-extra-opts tempopts)))))

(defun erl-ecs-check-dialyzer ()
  "Checks type and function warnings."
  (interactive)
  (erl-ecs-message "ECS: Checking Dialyzer.")

  (let ((path (buffer-file-name))
	(node (erl-target-node)))
    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'check_dialyzer (list path))
      (erl-receive ()
	  ;; no dialyzer warnings
	  ((['rex ['ok]]
	    (erl-ecs-message "ECS Dialyzer: No warnings.")
	    (setq erl-ecs-dialyzer-list '())
	    (erl-ecs-delete-items 'dialyzer erl-ecs-lineno-list)
	    (erl-ecs-remove-overlays 'ecs-dialyzer-overlay))

	   ;; dialyzer warnings
	   (['rex ['w warnings]]
	    (erl-ecs-message "ECS Dialyzer: Warnings found.")
	    (setq erl-ecs-dialyzer-list warnings)
	    (erl-ecs-print-errors 'dialyzer erl-ecs-dialyzer-list 'ecs-dialyzer-overlay))

	   (else))))))

(defun erl-ecs-check-xref ()
  "Checks for exported function that is not used outside the module."
  (interactive)
  (erl-ecs-message "ECS: Checking XREF.")

  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(expline (erl-ecs-find-exportline)))

    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'xref (list path))
      (erl-receive (expline)
	  ;; no xref warnings
	  ((['rex ['ok]]
	    (erl-ecs-message "ECS XREF: No warnings.")
	    (setq erl-ecs-xref-list '())
	    (erl-ecs-delete-items 'xref erl-ecs-lineno-list)
	    (erl-ecs-remove-overlays 'ecs-xref-overlay))
	   
	   ;; xref warnings
	   (['rex ['w warnings]]
	    (erl-ecs-message "ECS XREF: Warnings found.")
	    (setq erl-ecs-xref-list (list (tuple expline 'warning (tuple 'exported_unused_function warnings))))
	    (erl-ecs-print-errors 'xref erl-ecs-xref-list 'ecs-xref-overlay))

	   (else))))))

(defun erl-ecs-check-eunit ()
  "Checks eunit tests."
  (interactive)

  (erl-ecs-message "ECS: Checking EUNIT.")

  ;; reset eunit errors
  (setq erl-ecs-eunit-list '())
  (erl-ecs-delete-items 'eunit erl-ecs-lineno-list)
  (erl-ecs-remove-overlays 'ecs-eunit-overlay)
  
  (let ((node (erl-target-node))
	(path (buffer-name)))
    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'check_eunit (list path erl-self))
      (erl-ecs-eunit-receive))))

(defun erl-ecs-eunit-receive ()
  (erl-receive ()
      ((['ok which]
	;; kommer inte handa da de inte skickas fran noden,
	;; men valdigt latt att implementera i framtiden
	(erl-ecs-eunit-receive))

       (['e error]
	(add-to-list 'ecs-eunit-list error)
	(erl-ecs-eunit-receive))

       (['klar]))

    (erl-ecs-print-errors 'eunit erl-ecs-eunit-list 'ecs-eunit-overlay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Helpers           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-ecs-get-includes ()
  "Find the includefiles for an erlang module."
  (save-excursion
    (set-buffer erl-ecs-current-buffer)
    (goto-char (point-min))
    (let ((inc-regexp (concat "^-include\\(_lib\\)?(\"\\([^\)]*\\)"))
	  (include-list '())
	  (pt (point-min)))
      (while (string-match inc-regexp (buffer-string) pt)
	(add-to-list 'include-list (substring (match-string 2) 1))
	(setq pt (match-end 0)))
      include-list)))

(defun erl-ecs-find-exportline ()
  "Find the exportline."
  (save-excursion
    (set-buffer erl-ecs-current-buffer)
    (goto-char (or (string-match "^-export\\|^-compile" (buffer-string))
		   (point-min)))
    (line-number-at-pos (forward-char))))

(defun erl-ecs-remove-overlays (which)
  "Removes all overlays with the name \\[which]"
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
  (dolist (x errors)
    (progn
      ;; bug here, on every other check; adds items to list strange
      (setq erl-ecs-lineno-list (cons (list (tuple-elt x 1) tag) erl-ecs-lineno-list))

      (erl-ecs-display-overlay
       (erl-ecs-goto-beg-of-line (tuple-elt x 1))
       (erl-ecs-goto-end-of-line (tuple-elt x 1))
       (if (not lesser)
	   (if (string= (tuple-elt x 2) "error")
	       'erl-ecs-error-line
	     'erl-ecs-warning-line)
	 (if lesser-face lesser-face 'erl-ecs-warning-line))
       (format "%s: %s @line %s " (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1))
       lesser))))

(defun erl-ecs-display-overlay (beg end face tooltip-text &optional lesser)
  "Display the overlays."
  (let ((ov (make-overlay beg end nil t t)))
    (overlay-put ov 'face face)
    (overlay-put ov 'help-echo tooltip-text)
    (overlay-put ov (if (not lesser)
			'erl-ecs-overlay
		      lesser) t)
    (overlay-put ov 'priority (if (not lesser) 100 90))
    ov))


(defun erl-ecs-goto-error (&optional prev pos)
  (let ((delta (line-number-at-pos (if prev (point-min) (point-max))))
	(pt (line-number-at-pos pos)))
    (dolist (it erl-ecs-lineno-list delta) (when (or (and (not prev)
						    (> (car it) pt)
						    (< (car it) delta))
					       (and prev
						    (< (car it) pt)
						    (> (car it) delta)))
				       (setq delta (car it))))))

(defun erl-ecs-next-error ()
  (interactive)
  (let ((max (line-number-at-pos (point-max)))
	(next (erl-ecs-goto-error)))

    (goto-char
     (erl-ecs-goto-beg-of-line
      (if (= next max)
	  (erl-ecs-goto-error nil (point-min))
	next)))))

(defun erl-ecs-prev-error ()
  (interactive)
  (let ((min (line-number-at-pos (point-min)))
	(prev (erl-ecs-goto-error t)))

    (goto-char
     (erl-ecs-goto-beg-of-line
      (if (= prev min)
	  (erl-ecs-goto-error t (point-max))
	prev)))))

(defun erl-ecs-message (msg &rest r)
  (when erl-ecs-verbose (message msg r)))

(defun erl-ecs-delete-items (tag erl-ecs-list)
 "Deletes all items that has a value matching 'tag' from a list"
 (let ((new-list '()))
   (dolist (it erl-ecs-list new-list)
       (unless (equal tag (cdr it)) (setq new-list (nconc new-list it))))
   (setq erl-ecs-lineno-list new-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            TODO            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-ecs-start-interval ())

(erl-ecs-setup)

(provide 'erlang-compile-server)
