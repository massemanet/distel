(defvar erl-include-or-include-lib-pattern "-include\\(_lib(\\|(\\)\""
  "Regexp for matching '-include' and '-include_lib' entries in a file.")

(defvar erl-include-lib-pattern "-include_lib(\"\\(\\(.*?\\)/\\(.*?\\)\\)\"[ \t]*)"
  "Regexp for matching '-include_lib' entries in a file.")

(defvar erl-include-pattern "-include(\"\\(.*?\\)\"[ \t]*)"
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
      (let* ((application (match-string 2))
             (sub-path  (match-string 3)))
        (let ((node (or erl-nodename-cache (erl-target-node))))
          (erl-spawn
            (erl-send-rpc node 'distel 'find_header_file (list (intern application) (intern sub-path)))
            (erl-receive ()
                ((['rex header-file]
                  (when (file-exists-p header-file) (find-file header-file)))
                 (['rex ['error reason]]
                  (ring-remove erl-find-history-ring)
                  (message "Error: %s" reason))))))
        )
      )
    )
  )
(defun find-header-file-by-include-under-point()
  "point is on '-include(header-file)' ,open the header file "
  (save-excursion
    (beginning-of-line)
    (when (looking-at erl-include-pattern)
      (let ((header-file (erl-locate-header-file (match-string 1))))
        (when header-file (find-file header-file)))
      ))
  )

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
    (let* ((current-src-file (buffer-file-name (current-buffer)))
           (project-root (expand-file-name "../.." current-src-file))
           (header-in-default-include-path (expand-file-name (concat  "include/" extracted-header-file-path) project-root))
           (default-deps-path (expand-file-name "deps/" project-root))
           tmp-include-file
           )
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

(defun erl-extract-include-paths-from-buffer (buffer)
  "Collects included paths from a file and returns them in a list,
only '-include(' ,no 'include_lib('"
  (let ((paths nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward erl-include-pattern nil t)
        (push (match-string 1) paths)))
    (nreverse paths)))

 ;; (erl-extract-include-lib-paths-from-buffer (current-buffer) (lambda (header-files) (print header-files) ))
(defun erl-extract-include-lib-paths-from-buffer (buffer hook)
  "Collects included paths from a file,only '-include_lib(' ,no
'include(' if include_lib path exists ,the `hook' would be
called,the `hook' is a function accept one parameter,the
parameters is a list of found header files"
  (let ((node (or erl-nodename-cache (erl-target-node)))
        (paths nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward erl-include-lib-pattern nil t)
        (add-to-list 'paths (match-string 1) t)))
    (erl-spawn
      (erl-send-rpc node 'distel 'find_header_files (list paths hook))
      (erl-receive ()
          ((['rex (header-files hook)]
            (apply hook (list header-files))
            )
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

(defun erl-find-source-under-point ()
  "When trying to find a function definition checks to see if we
  are standing on a macro ,a header file a record."
  (interactive)
  (let ((pattern (erl-is-pattern)))
    (cond ((equal pattern 'open-header)
           (erl-open-header-file-under-point))
          ((stringp pattern)
           (erl-find-source-pattern-under-point pattern))
          (t
           (erl-find-function-under-point)))))

(defun erl-find-source-pattern-in-file(pattern  header-file)
  (let* ((buf-exists (find-buffer-visiting header-file))
         (buf buf-exists))
    (unless buf-exists
      (setq buf(find-file-noselect header-file t)))
    (list (erl-find-pattern-in-buffer buf pattern) buf-exists)))

(defvar erl-erlookup-pattern nil "private variable")

(defun erl-find-source-pattern-under-point(pattern)
  (setq erl-erlookup-pattern pattern)
  (let (buf  buf-exists found tmp-result)
    (setq found (catch 'found
                  (dolist (header-file (erl-extract-include-paths-from-buffer (current-buffer)))
                    (setq tmp-result (erl-find-source-pattern-in-file pattern header-file))
                    (if (equal (caar tmp-result) 'ok)
                        (throw 'found `(,header-file ,(nth 1 (car tmp-result))))
                      (unless (nth 1 tmp-result)
                        (kill-buffer (find-buffer-visiting header-file)))
                      ))nil))
    (when found
      (with-current-buffer (switch-to-buffer (find-file (car found)))
        (goto-char (nth 1 found))))

    (unless found
      (erl-extract-include-lib-paths-from-buffer
       (current-buffer)
       (lambda (header-files)
         (setq found
               (catch 'found
                 (dolist (header-file header-files )
                   (setq tmp-result (erl-find-source-pattern-in-file  erl-erlookup-pattern header-file))
                   (if (equal (caar tmp-result) 'ok)
                       (throw 'found `(,header-file ,(nth 1 (car tmp-result))))
                     (unless (nth 1 tmp-result)
                       (kill-buffer (find-buffer-visiting header-file)))
                     )
                   )  )
               )
         (when found
           (with-current-buffer (switch-to-buffer (find-file (car found)))
             (goto-char (nth 1 found))
             )  )
         )
       )
      )

    )
  )

(defun erl-find-pattern-in-buffer (buffer pattern)
  "Goto the definition of ARG in the current buffer and return '(ok newposition) ,or (fail oldposition)"
  (with-current-buffer buffer
    (let ((origin (point)))
      (goto-char (point-min))
      (set (make-local-variable 'case-fold-search) nil)
      (if (re-search-forward pattern nil t)
          `(ok ,(point)) `(fail ,origin))
      )
    ))
(provide 'erlookup)
