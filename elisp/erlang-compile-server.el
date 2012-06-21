;;;; variables for customization
; erlang-compile-server-check-on-save default t
; erlang-compile-server-compile-if-ok default t
; keybinding for save C-x C-a ?
; keybinding for toggle mode ?

(require 'distel)

;;;;;;;
;;;;;;;; Erlang compile server
;;;;;;;


;; Check that module is loaded else load it
(add-hook 'erl-nodeup-hook 'esc-check-backend)

(defun esc-check-backend (node _fsm)
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
  (or (boundp 'erlang-compile-server-check-on-save)
      (setq erlang-compile-server-check-on-save t))
  (or (boundp 'erlang-compile-server-compile-if-ok)
      (setq erlang-compile-server-compile-if-ok t))
  (if erlang-compile-server-check-on-save
      (add-hook 'after-save-hook 'erlang-compile-server-check-file-ending))
  (add-hook 'erlang-mode-hook 'erlang-compile-server-mode-hook)
  (add-to-list 'minor-mode-alist
	       '(erlang-compile-server-mode
		 " ESC"))
)

(defun erlang-compile-server-mode-hook ()
  (erlang-compile-server-mode t))

(define-minor-mode erlang-compile-server-mode
  "Erlang compile server"
  nil
  nil
  nil)

;; On save, check for errors etc
(defun erlang-compile-server-check-file-ending()
   (let ((path (buffer-file-name)))
   (if (string= (substring path (- 0 (length ".erl"))) ".erl") (erlang-compile-server-check-for-errors-and-warnings)))
)

;;; Main function
(defun erlang-compile-server-check-for-errors-and-warnings()
  (interactive)
  (let ((node (erl-target-node))
	(path (buffer-file-name))
	(buffer (current-buffer)))
;	(compile-opts (get-compile-options)))
    (erl-spawn
      (erl-send-rpc node 'erlang_compile_server 'try_to_compile (list path)) ; n m f a
      (erl-receive (buffer)
	  ((['rex ['ok]] ; no errors
	    (erlang-compile-server-remove-overlays buffer)
	    (message "Ok. %s" (if erlang-compile-server-compile-if-ok " Compiling." ""))
	    (if erlang-compile-server-compile-if-ok (erlang-compile)))
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

(defun erlang-compile-server-get-compile-options () ;; deprecated
  (let ((code-dir-opts ; outputfolder
	 (apply #'append
		(mapcar (lambda (dir) (tuple 'pa dir))
			(list (concat (erlang-compile-server-get-app-dir) "ebin")))))
	(inc-dir-opts ; include dirs
	 (apply #'append
		(mapcar (lambda (dir) (tuple 'i dir))
			(list (concat (erlang-compile-server-get-app-dir) "include"))))))
    (list code-dir-opts inc-dir-opts)))

(defun erlang-compile-server-erlang-get-app-dir () ;; deprecated
  (let ((src-path (file-name-directory (buffer-file-name))))
    (file-name-directory (directory-file-name src-path))))

(defun erlang-compile-server-print-errors-and-warnings(errors warnings)
  (save-excursion
    (let ((err (if errors (cadr (tuple-to-list (tuple-elt errors 1))) nil))
	  (war (if warnings (cadr (tuple-to-list (tuple-elt warnings 1))) nil))
	  tooltip-text
	  x)
      (while err ; errors
	(setq x (pop err)
	      tooltip-text (format "%s error: %s @line %s " (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1)))
	(erlang-compile-server-make-overlay (tuple-elt x 1) 'error-line tooltip-text)
	(message "%s" (propertize tooltip-text 'face 'erlang-compile-server-error-line)))
      (while war ; warnings
	(setq x (pop war)
	      tooltip-text (format "%s warning: %s @line %s" (tuple-elt x 2) (tuple-elt x 3) (tuple-elt x 1)))
	(erlang-compile-server-make-overlay (tuple-elt x 1) 'warning-line tooltip-text)
	(message "%s" (propertize tooltip-text 'face 'erlang-compile-server-warning-line))))
))

;;; Overlays

;; Faces for highlighting
(defface erlang-compile-server-error-line
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'flymake)

(defface erlang-compile-server-warning-line
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'flymake)

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