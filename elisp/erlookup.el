;;; erlookup.el

;;; Commentary:
;;
;; This module contains a naive, inefficient and buggy way to lookup macro and
;; record definitions. Sort of like M-. for functions.
;;
;; NOTE: This is a work in progress and stuff change all the time. There are
;; also a couple of known bugs. Since it makes use of simple and dumb regexps
;; for finding stuff it's easy finding definitions like '-define(foo, bar)' but
;; a bit trickier finding other cases with lists of definitions, etc.

;; To use this you need to specify a list of lookup roots:

;; (setq erlookup-roots '("~/projects/foo/lib"
;;                        "~/path/to/otp/headers"))

;; TODO: 'inline' lookups of macros to e.g. jump to record definitions when
;; standing on '#?name_of_record', and jump to function definitions when
;; standing on '?name_of_function(Foo, Bar)'.
;; TODO: Make everything less side-effecty.

(require 'thingatpt)

;;; Path related things
(defvar erlookup-roots nil
  "List of paths from which header files will try to be located.")

(defvar erl-include-pattern "-include\\(_lib(\\|(\\)\""
  "Regexp for matching '-include' and '-include_lib' entries in a file.")

(defun erl-find-includes ()
  "Collects included paths from a file and returns them in a list."
  (let ((origin (point))
        (paths nil)
        (searching t))
    (goto-char (point-min))
    (while searching
      (if (re-search-forward erl-include-pattern nil t)
          (push (thing-at-point 'filename) paths)
        (setq searching nil)))
    (goto-char origin)
    (reverse paths)))

;; TODO: For some reason `remove-duplicates' didn't work. It was easier to
;; write one than finding out what went wrong with the original.
(defun remove-dup-paths (dup-paths)
  "Removes duplicate paths."
  (let ((paths nil))
    (dolist (dp dup-paths)
      (unless (member dp paths)
        (push dp paths)))
    paths))


;; utilities

(defun last-from-path (path delim)
  "Returns the last part of a string after being split at `delim'."
  (first (last (split-string path delim))))

(defun buffer-name-from-path (path)
  "Returns the last part of a path, e.g. 'toto/tata/titi.erl' ->
titi.erl"
  (last-from-path path "/"))

(defun find-file-paths (path roots)
  "Brokenly transforms a list of paths to paths that can be used
by `find-file'."
  (let ((paths))
    (loop for r in roots
          do (if (string-equal ".." (first (split-string path "/")))
                 (push (expand-file-name (substring-no-properties path)) paths)
               (push (concat r "/" path) paths)))
    (push (concat "./" path) paths)
    paths))

(defun try-open-file (path)
  (setq find-paths (find-file-paths path erlookup-roots))
  (dolist (find-path find-paths)
    (when (file-exists-p find-path)
      (find-file find-path))))

;;; lookup related things

;; TODO: need to look for "pattern arg" before looking for pattern "(.*, arg"
(defun erl-find-pattern-in-file (pattern arg)
  "Goto the definition of ARG in the current buffer and return
symbol."
  (let ((origin (point))
        (symbol nil))
    (goto-char (point-min))
    ;; (if (re-search-forward (concat pattern arg "\\(,\\|(\\)") nil t)
    (set (make-local-variable 'case-fold-search) nil)
    (if (re-search-forward
         (concat pattern "\\(" arg "\\(,\\|(\\)\\|.*\\?" arg "\\)") nil t)
        (progn t (beginning-of-line) (search-forward "(") ;;(backward-word)
               (setq symbol (cons (thing-at-point 'symbol) (copy-marker (point-marker))))))
    (goto-char origin)
    symbol))


;; FIXME: This 50 loc monster just feels wrong
(defun erl-find-source-pattern (pattern arg &optional include-paths)
  (unless include-paths
    (ring-insert-at-beginning erl-find-history-ring
                              (copy-marker (point-marker))))
  (let ((origin (point))
        (paths (if include-paths include-paths (erl-find-includes)))
        (extra-paths nil)
        (already-open nil)
        (already-tried nil)
        (symbol (erl-find-pattern-in-file pattern arg)))

    ;; check open buffers first
    (dolist (path paths)
      (unless symbol
        (setq buffer-path (buffer-name-from-path path))
        
        (when (get-buffer buffer-path)
          (set-buffer buffer-path)
          (setq extra-paths (remove-duplicates (append (erl-find-includes) extra-paths)))
          (push buffer-path already-tried)
          (push buffer-path already-open))
        
        (when (setq symbol (erl-find-pattern-in-file pattern arg))
          (switch-to-buffer buffer-path)
          (goto-char (cdr symbol)))))

    ;; slowly read from disk to find stuff
    (dolist (path paths)
      (unless symbol
        (setq buffer-path (buffer-name-from-path path))
        (setq find-paths (find-file-paths path erlookup-roots))
        
        (unless (member buffer-path already-tried)
          
          (dolist (find-path find-paths)
            (when (file-exists-p find-path)
              (find-file find-path)
              (set-buffer buffer-path)
              (setq extra-paths (append (erl-find-includes) extra-paths))
              (push buffer-path already-tried)
              
              (when (setq symbol (erl-find-pattern-in-file pattern arg))
                (switch-to-buffer buffer-path)
                (goto-char (cdr symbol)))
              
              (unless (member buffer-path already-open)
                (unless symbol
                  (kill-this-buffer))))))))
    
    (if (and (not symbol) extra-paths)
        (progn
          (erl-find-source-pattern pattern arg extra-paths))
      
      (if symbol
          (goto-char (cdr symbol))
        (goto-char origin)
        (message "Can't find definition of: %s" arg)
        (erl-find-source-unwind)))))


(defvar erl-record-regex "-record("
  "regex to search for when looking up records")

(defvar erl-macro-regex "-define("
  "regex to search for when looking up records")

(defun erl-is-pattern ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (cond
     ;; ((looking-back "\\#?") erl-inline-record-regex)
     ((looking-back (concat erl-include-pattern ".*")) 'open-header)
     ((looking-back "\\#") erl-record-regex)
     ((looking-back "\\?") erl-macro-regex)
     ;; ((and (looking-back "\\?") (looking-forward "(")) erl-inline-function-regex)
     (t nil))))

(defun meta-erl-find-source-under-point ()
  "When trying to find a function definition checks to see if we
  are standing on a macro instead."
  (interactive)
  (if erlookup-roots
      (let ((pattern (erl-is-pattern)))
        (cond ((equal pattern 'open-header)
               (progn
                 (ring-insert-at-beginning erl-find-history-ring
                                           (copy-marker (point-marker)))
                 (try-open-file (save-excursion (end-of-thing 'filename) (thing-at-point 'filename)))))
               ((stringp pattern)
               (erl-find-source-pattern pattern (thing-at-point 'symbol)))
              (t
               (erl-find-source-under-point))))
    (erl-find-source-under-point)))

(provide 'erlookup)
