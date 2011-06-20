;;; erlookup.el

;;; Commentary:
;;
;; This module contains a naive, inefficient and buggy way to lookup
;; macro and record definitions. Sort of like M-. for functions.
;;
;; NOTE: This is a work in progress and stuff change all the time. There
;; are also a couple of known bugs. Since it makes use of simple and
;; dumb regexps for finding stuff it's easy finding definitions like
;; '-define(foo, bar)' but a bit trickier finding other cases with lists
;; of definitions, etc.

;; NOTE: We now try to ask a running distel node what include paths were
;; used when compiling the current module. For this to work you will
;; need to compile the module with +debug_info.

;; The original `erlookup-roots' variable can still be set, but it is
;; not needed if you have a distel node running. If it is set the paths
;; will still be used.

;; If you have `erlookup-roots' set you will also not need a running
;; node for the lookup to work.

;; `erlookup-roots' are set like this:

;; (setq erlookup-roots '("~/projects/foo/lib"
;;                        "~/path/to/otp/headers"))

;; This is all a bit hackish for now, but it seems to work. Distel will
;; however complain about the node being down if you do lookups
;; "offline".

;; NOTE: This offline lookup functionality might be ripped out to a
;; separate elisp package altogether sooner or later. It would be a
;; better fit in a new erlang-mode utilizing the a cedet parser for
;; Erlang.

;; TODO: 'inline' lookups of macros to e.g. jump to record definitions
;; when standing on '#?name_of_record', and jump to function definitions
;; when standing on '?name_of_function(Foo, Bar)'.

;; TODO: Make everything less side-effecty.

(require 'thingatpt)
(require 'erlang)
(require 'distel)

;;; Path related things
(defvar erlookup-roots nil
  "List of manually added paths from which header files will try
  to be located.")

(defvar erl-include-pattern "-include\\(_lib(\\|(\\)\""
  "Regexp for matching '-include' and '-include_lib' entries in a file.")

(defun erl-extract-include-paths-from-buffer (buffer)
  "Collects included paths from a file and returns them in a list."
  (let ((paths nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward erl-include-pattern nil t)
        (push (thing-at-point 'filename) paths)))
    (nreverse paths)))

;; Yes, this is horribly horrible horribleness, but for now I can't
;; figure out a nicer way of doing it.
(defvar erlookup-roots-distel nil
  "Scary global variable containing list of paths from which
  header files will try to be located. Should only be set from
  `erl-find-include-paths'.")

(defun erl-find-include-paths ()
  (erl-find-include-paths-distel)
  (cond ((and erlookup-roots erlookup-roots-distel)
         (append erlookup-roots erlookup-roots-distel))
        (erlookup-roots-distel erlookup-roots-distel)
        (t erlookup-roots)))

(defun erl-find-include-paths-distel ()
  (let ((module (erlang-get-module))
        (node (or erl-nodename-cache (erl-target-node))))
    (erl-spawn
      (erl-send-rpc node 'distel 'find_includes (list (intern module)))
      (erl-receive ()
          ((['rex ['ok paths]]
            (setq erlookup-roots-distel paths))
           (['rex ['error reason]]
            (ring-remove erl-find-history-ring)
            (message "Error: %s" reason)))))))

(defun erl-find-variable-binding ()
  (ring-insert-at-beginning erl-find-history-ring (copy-marker (point-marker)))
  (let ((sym (thing-at-point 'symbol)))
    (if (erlang-in-arglist-p)
        (message "To be continued")
      (erl-search-local-variable-binding sym))))

(defun erl-search-local-variable-binding (sym)
  (let ((origin nil))
    (beginning-of-thing 'symbol)
    (setq origin (point))
    (erlang-beginning-of-clause)
    (set (make-local-variable 'case-fold-search) nil)
    (re-search-forward sym)
    (beginning-of-thing 'symbol)
    (when (eq origin (point))
      (erl-find-source-unwind)
      (message "Already standing on first occurance of: %s" sym))))


;; utilities

(defun compose-include-file-paths (path roots)
  "Brokenly transforms a list of paths to paths that can be used by `find-file'."
  (let ((paths))
    (loop for r in roots
          do (if (string-equal ".." (first (split-string path "/")))
                 (push (expand-file-name (substring-no-properties path)) paths)
               (push (concat (file-name-as-directory r) path) paths)))
    (push (concat "./" path) paths)
    paths))


(defun erlang-at-variable-p ()
  "Possibly the ugliest hack ever :)

Rely on syntax highlighting of erlang-mode to determine whether
we are standing on a variable"
  (if (eq 'font-lock-variable-name-face (get-text-property (point) 'face))
      t nil))

(defvar erl-function-definition-regex
  (concat "^" erlang-atom-regexp "\\s *(")
  "Regex for finding function definitions")

(defun erlang-in-arglist-p ()
  (if (erlang-stop-when-inside-argument-list)
      (save-excursion
        (re-search-backward "(")
        (forward-char)
        (if (looking-back erl-function-definition-regex)
            t nil))
    nil))

(defun erlang-on-function-definition-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at erl-function-definition-regex)))


;;; lookup related things

(defun erl-find-pattern-in-buffer (buffer pattern arg)
  "Goto the definition of ARG in the current buffer and return symbol."
  (let ((origin (point))
        (symbol nil))
    (goto-char (point-min))
    (set (make-local-variable 'case-fold-search) nil)
    (if (re-search-forward
         (concat pattern "\\s *" arg "\\s *\\(,\\|(\\)") nil t)
        (progn t (beginning-of-line) (search-forward "(") ;;(backward-word)
               (setq symbol (cons (thing-at-point 'symbol)
                                  (copy-marker (point-marker))))))
    symbol))


(defun erl-open-header-file-under-point ()
  (ring-insert-at-beginning erl-find-history-ring (copy-marker (point-marker)))
  (try-open-file (save-excursion
                   (end-of-thing 'filename) (thing-at-point 'filename))))

(defun try-open-file (path)
  (let ((find-paths (compose-include-file-paths path (erl-find-include-paths))))
    (dolist (find-path find-paths)
      (when (file-exists-p find-path)
        (find-file find-path)))))


(defun erl-find-source-pattern-under-point (pattern)
  (erl-find-source-pattern pattern (thing-at-point 'symbol)))

(defun erl-find-source-pattern-in-open-buffers (pattern arg paths)
  (let ((symbol) (buffer-name) (extra-paths) (tried) (open))
    (dolist (path paths)
      (unless symbol
        (setq buffer-name (file-name-nondirectory path))
        (when (get-buffer buffer-name)
          (setq extra-paths (remove-duplicates (append (erl-extract-include-paths-from-buffer buffer-name) extra-paths)))
          (push buffer-name tried)
          (push buffer-name open)
          (setq symbol (erl-find-pattern-in-buffer buffer-name pattern arg)))))
    (list symbol buffer-name extra-paths tried open)))

(defun erl-find-source-pattern-in-files-on-disk (pattern arg paths open tried)
  (let ((symbol) (buffer-name) (extra-paths) (tried) (open))
    (dolist (path paths)
      (unless symbol
        (setq buffer-name (file-name-nondirectory path))
        (setq find-paths (compose-include-file-paths path (erl-find-include-paths)))
        (unless (member buffer-name tried)
          (dolist (find-path find-paths)
            (when (file-exists-p find-path)
              (find-file find-path)
              (setq extra-paths (append (erl-extract-include-paths-from-buffer buffer-name) extra-paths))
              (push buffer-name tried)
              (setq symbol (erl-find-pattern-in-buffer buffer-name pattern arg))
              (unless (member buffer-name open)
                (unless symbol
                  (kill-this-buffer))))))))
    (list symbol buffer-name extra-paths tried open)))

(defun erl-find-source-pattern (pattern arg &optional include-paths)
  (unless include-paths
    (ring-insert-at-beginning erl-find-history-ring
                              (copy-marker (point-marker))))
  (let ((origin (point))
        (paths (if include-paths include-paths (erl-extract-include-paths-from-buffer (file-name-nondirectory buffer-file-name))))
        (extra-paths nil)
        (open nil)
        (tried nil)
        (buffer-name nil)
        (find-paths nil)
        (symbol (erl-find-pattern-in-buffer (file-name-nondirectory buffer-file-name) pattern arg))
        (open-buffers-pass nil)
        (disk-buffers-pass nil))

    ;; check open buffers first.
    
    ;; unless we've found what we're looking for, check open buffers to
    ;; see if the header file we want to search in for our symbol is
    ;; already open, if it can be found we record it as open so we don't
    ;; close it later on, if we find the symbol we jump to it.
    (unless symbol
      (when (setq open-buffers-pass (erl-find-source-pattern-in-open-buffers pattern arg paths))
        (setq symbol (nth 0 open-buffers-pass))
        (setq buffer-name (nth 1 open-buffers-pass))
        (setq extra-paths (nconc (nth 2 open-buffers-pass)))
        (setq open (nconc (nth 3 open-buffers-pass)))
        (setq tried (nconc (nth 4 open-buffers-pass)))))

    ;; slowly read from disk to find stuff

    ;; if we didn't find the symbol in the open buffers, we try to open
    ;; the header files from disk and search through them, we won't open
    ;; files recorded in already-tried and we won't close buffers
    ;; recorded in already-open.
    (unless symbol
      (when (setq disk-buffers-pass (erl-find-source-pattern-in-files-on-disk pattern arg paths open tried))
        (setq symbol (nth 0 disk-buffers-pass))
        (setq buffer-name (nth 1 disk-buffers-pass))
        (setq extra-paths (nconc (nth 2 disk-buffers-pass)))
        (setq open (nconc (nth 3 disk-buffers-pass)))
        (setq tried (nconc (nth 4 disk-buffers-pass)))))

    ;; do a recursive call if we found some extra include paths while
    ;; searching through header files.
    (unless (and symbol buffer-name)
      (if extra-paths
          (erl-find-source-pattern pattern arg extra-paths)
        (message "Can't find definition for: %s" arg)
        (erl-find-source-unwind)))

    (when (and symbol buffer-name)
      (switch-to-buffer buffer-name)
      (goto-char (cdr symbol)))))


(defvar erl-record-regex "-record("
  "regex to search for when looking up records")

(defvar erl-macro-regex "-define("
  "regex to search for when looking up macros")

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

(defun erl-find-source-under-point ()
  "When trying to find a function definition checks to see if we
  are standing on a macro instead."
  (interactive)
  (let ((pattern (erl-is-pattern)))
    (cond ((equal pattern 'open-header)
           (erl-open-header-file-under-point))
          ((stringp pattern)
           (erl-find-source-pattern-under-point pattern))
          ((erlang-at-variable-p)
           (erl-find-variable-binding))
          ((erlang-on-function-definition-p)
           (erl-who-calls (erl-target-node)))
          (t
           (erl-find-function-under-point)))))

(provide 'erlookup)
