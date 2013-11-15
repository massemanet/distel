(require 'thingatpt)
(require 'erlang)
(require 'distel)

(defvar erl-include-or-include-lib-pattern "[ \t]*-include\\(_lib\\)?[ \t]*([ \t]*\""
  "Regexp for matching '-include' and '-include_lib' entries in a file.")

(defvar erl-include-lib-pattern "[ \t]*-include_lib[ \t]*([ \t]*\"\\(\\(.*?\\)/\\(.*?\\)\\)\"[ \t]*)"
  "Regexp for matching '-include_lib' entries in a file.")

(defvar erl-include-pattern "[ \t]*-include[ \t]*([ \t]*\"\\(.*?\\)\"[ \t]*)"
  "Regexp for matching '-include' entries in a file.")


(defun erl-open-header-file-under-point()
  (interactive)
  (ring-insert-at-beginning erl-find-history-ring (copy-marker (point-marker)))
  (save-excursion
    (beginning-of-line)
    (if (looking-at erl-include-lib-pattern)
        (find-header-file-by-include-lib-under-point)
      (find-header-file-by-include-under-point))))

(defun find-header-file-by-include-lib-under-point()
  "open the header file by `-include_lib(header-file)'. "
  (save-excursion
    (beginning-of-line)
    (when (looking-at erl-include-lib-pattern)
      (let* ((include-lib-header (match-string 1))
             (node (or erl-nodename-cache (erl-target-node))))
        (erl-spawn
          (erl-send-rpc node 'distel 'find_header_file (list include-lib-header))
          (erl-receive ()
              ((['rex header-file]
                (when (file-exists-p header-file) (find-file header-file)))
               (['rex ['error reason]]
                (ring-remove erl-find-history-ring)
                (message "Error: %s" reason)))))))))

(defun find-header-file-by-include-under-point()
  "point is on '-include(header-file)' ,open the header file "
  (save-excursion
    (beginning-of-line)
    (when (looking-at erl-include-pattern)
      (let ((header-file (erl-locate-header-file (match-string 1))))
        (when header-file (find-file header-file)))
      ))
  )
(defun erl-guess-app-dir()
  "guess app-root dir."
  (let (app-root)
    (cond
     ((setq app-root (locate-dominating-file default-directory "Emakefile"))
      (expand-file-name app-root))
     ((setq app-root (locate-dominating-file default-directory "rebar"))
      (expand-file-name app-root))
     ((setq app-root (locate-dominating-file default-directory "Makefile"))
      (expand-file-name app-root))
     ((setq app-root (locate-dominating-file default-directory "makefile"))
      (expand-file-name app-root))
     (t (file-name-directory
         (directory-file-name (file-name-directory (buffer-file-name))))))))

;; http://www.erlang.org/doc/reference_manual/macros.html
;; (erl-locate-header-file "head.hrl")
;; (erl-locate-header-file "$INCLUDE/bab.hrl")
(defun erl-locate-header-file (extracted-header-file-path)
  "find header file of type -include(),return the absolute path of header file ."
  (cond
   ;; absolute path or file in current path -include("my_records.hrl"). -include("/home/user/proj/my_records.hrl").
   ((file-exists-p extracted-header-file-path)
    (expand-file-name extracted-header-file-path))

   ((string-match "^\\$\\(.+?\\)\\b\\(.*\\)" extracted-header-file-path) ;-include("$PROJ_ROOT/my_records.hrl"). with $VAR
    (let ((env (match-string  1 extracted-header-file-path))
          (other-part-of-header-file (match-string 2 extracted-header-file-path))
          (header-file))
      (setq header-file (concat (or (getenv env) env) other-part-of-header-file ))
      (when (file-exists-p header-file) header-file)))

   (t                                   ; in project-root/include ,or project/deps/app-name/include
    (let* ((project-root (erl-guess-app-dir))
           (header-in-default-include-path (expand-file-name (concat  "include/" extracted-header-file-path) project-root))
           (default-deps-path (expand-file-name "deps/" project-root))
           tmp-include-file)
      (if (file-exists-p header-in-default-include-path)
          header-in-default-include-path
        (when (file-exists-p default-deps-path)
          (catch 'found
            (dolist (dir-in-deps-dirs (directory-files default-deps-path t))
              (setq tmp-include-file (expand-file-name (concat "include/" extracted-header-file-path) dir-in-deps-dirs))
              (when (file-exists-p tmp-include-file)
                (throw 'found tmp-include-file))))
          )
        )
      )
    )
   )
  )
(defun erl-extract-include-paths-from-buffer-recursively(buffer)
  "Collects included paths from a file and returns them in a list,
only '-include(' ,no 'include_lib('"
  (let (stack extracted-stack cur-file file-visiting-p cur-buf)
    (push (buffer-file-name buffer) stack)
    stack
    (while (> (length stack) 0)
      (setq cur-file (pop stack))
      (unless (member cur-file  extracted-stack)
        (push cur-file extracted-stack)
        (setq file-visiting-p (find-buffer-visiting cur-file))
        (setq cur-buf  (or file-visiting-p (find-file-noselect cur-file)))
        (setq stack (append stack (erl-extract-include-paths-from-buffer cur-buf)))
        (unless file-visiting-p (kill-buffer cur-buf))))
    extracted-stack))

(defun erl-extract-include-paths-from-buffer (buffer)
  "Collects included paths from a file and returns them in a list,
only '-include(' ,no 'include_lib('"
  (let ((project-root (erl-guess-app-dir))
        paths path tmp-include-file)
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward erl-include-pattern nil t)
          (setq path (match-string 1))
          (cond
           ((file-exists-p (expand-file-name path ))
            (push (expand-file-name path ) paths))
           ((file-exists-p (expand-file-name (concat  "../include/" path)))
            (push (expand-file-name (concat  "../include/" path)) paths))
           ((file-exists-p (expand-file-name (concat  "include/" path) project-root))
            (push  (expand-file-name (concat  "include/" path) project-root) paths))
           ))))
    paths
    ))

 ;; (erl-extract-include-lib-paths-from-buffer (current-buffer) (lambda (header-files) (print header-files) ))
(defun erl-extract-include-lib-paths-from-buffer (buffer hook)
  "Collects included paths from a file,only '-include_lib(' ,no
'include(' if include_lib path exists ,the `hook' would be
called,the `hook' is a function accept one parameter,the
parameters is a list of found header files"
  (let ((node (or erl-nodename-cache (erl-target-node)))
        (paths nil))
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward erl-include-lib-pattern nil t)
          (add-to-list 'paths (match-string 1) t)))
      )

    (erl-spawn
      (erl-send-rpc node 'distel 'find_header_files (list paths))
      (erl-receive (hook)
          ((['rex header-files ]
            (apply hook (list header-files)))
           (['rex ['error reason]]
            (ring-remove erl-find-history-ring)
            (message "Error: %s" reason)))))
    )
  )

(defun erl-macro-regex ()
  (let ((macro (thing-at-point 'symbol)))
     (format "-define(\\s *%s\\(,\\|(\\)" macro)))

(defun erl-record-regex ()
  (let ((record (thing-at-point 'symbol)))
     (format "-record(\\s *%s\\(,\\|(\\)" record)))

(defun erl-is-pattern ()
  (save-excursion
    (beginning-of-thing 'symbol)
    (cond
     ((save-excursion (progn (beginning-of-line)(looking-at erl-include-or-include-lib-pattern))) 'open-header)
     ((looking-back "\\#") (erl-record-regex))
     ((looking-back "\\?") (erl-macro-regex))
     (t t))))
;;; find variable
(defun erl-find-variable-binding ()
  (ring-insert-at-beginning erl-find-history-ring (copy-marker (point-marker)))
  (let ((sym (thing-at-point 'symbol)))
    (if (erlang-in-arglist-p)
        (message "To be continued")
      (erl-search-local-variable-binding sym))))
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

(defun erl-search-local-variable-binding (sym)
  (let ((origin nil)
        (sympos nil))
    (beginning-of-thing 'symbol)
    (setq origin (point))
    (erlang-beginning-of-clause)
    (setq sympos (erl-search-variable sym))
    (if (eq origin sympos)
        (progn (erl-find-source-unwind)
               (message "Already standing on first occurance of: %s" sym))
      (goto-char sympos))))

(defun erl-search-variable (sym)
  (set (make-local-variable 'case-fold-search) nil)
  (re-search-forward sym)
  (backward-char)
  (if (equal (thing-at-point 'symbol) sym)
      (progn (beginning-of-thing 'symbol)
             (point))
    (erl-search-variable sym)))


(defun erlang-at-variable-p ()
  "Possibly the ugliest hack ever :)

Rely on syntax highlighting of erlang-mode to determine whether
we are standing on a variable"
  (if (eq 'font-lock-variable-name-face (get-text-property (point) 'face))
      t nil))
;;;

(defun erl-find-source-under-point ()
  "When trying to find a function definition checks to see if we
  are standing on a macro ,a header file a record."
  (interactive)
  (let ((pattern (erl-is-pattern)))
    (cond ((equal pattern 'open-header) ;find header-files
           (erl-open-header-file-under-point))
          ((stringp pattern)            ;find macro or record
           (erl-find-source-pattern-under-point pattern))
          ((erlang-at-variable-p)
           (erl-find-variable-binding))
          ;; ((erlang-on-function-definition-p)
          ;;  (erl-who-calls (erl-target-node)))
          (t                            ;find function
           (erl-find-function-under-point)))))

(defun erl-find-source-pattern-in-file(pattern  header-file)
  "return ((ok position-of-matched-pattern-in-headerfile) header-file-already-opened-in-emacs-p)
or   (nil header-file-already-opened-in-emacs-p)"
  (let* ((buf-exists (find-buffer-visiting header-file))
         (buf buf-exists))
    (unless buf-exists
      (setq buf(find-file-noselect header-file t)))
    (list (erl-find-pattern-in-buffer buf pattern) buf-exists)))

;; FIXME: if a header file include another header file ,it failed
(defun erl-find-source-pattern-under-point(pattern)
  "pattern can be a '#str','?str',means finding a record or macro  "
  (ring-insert-at-beginning erl-find-history-ring (copy-marker (point-marker)))
  (let (buf  buf-exists found tmp-result)
    (setq found (catch 'found
                  (dolist (header-file (erl-extract-include-paths-from-buffer-recursively (current-buffer)))
                    (setq tmp-result (erl-find-source-pattern-in-file pattern header-file))
                    (if (equal (caar tmp-result) 'ok)
                        (throw 'found `(,header-file ,(nth 1 (car tmp-result)))) ;return (header-file new-position)
                      (unless (nth 1 tmp-result) ;if buffer opened by function find-file-noselect ,now make sure it is closed
                        (kill-buffer (find-buffer-visiting header-file)))
                      ))nil))
    (when found                         ;found = (header-file newposition)
      (with-current-buffer (switch-to-buffer (find-file (car found)))
        (goto-char (nth 1 found))))

    (unless found
      (erl-extract-include-lib-paths-from-buffer
       (current-buffer)
       (lambda (header-files)
         (setq found
               (catch 'found
                 (dolist (header-file header-files )
                   (setq tmp-result (erl-find-source-pattern-in-file  pattern header-file))
                   (if (equal (caar tmp-result) 'ok)
                       (throw 'found `(,header-file ,(nth 1 (car tmp-result))))
                     (unless (nth 1 tmp-result)
                       (kill-buffer (find-buffer-visiting header-file)))))))
         (when found
           (with-current-buffer (switch-to-buffer (find-file (car found)))
             (goto-char (nth 1 found))))
         (unless found
           (ring-remove erl-find-history-ring)))))))

(defun erl-find-pattern-in-buffer (buffer pattern)
  "find pattern in buffer  return '(ok newposition) ,or nil"
  (save-excursion
    (with-current-buffer buffer
      (goto-char (point-min))
      (set (make-local-variable 'case-fold-search) nil)
      (if (re-search-forward pattern nil t)
          (list 'ok (point)) nil))))

(provide 'erlookup)
