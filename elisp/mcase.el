(eval-when-compile (require 'cl))
(require 'erlext)                       ; for `erl-tag'.

(eval-and-compile
  (defun mcase-parse-clauses (clauses)
    `(list ,@(mapcar #'(lambda (clause)
                         `(list ',(car clause)
                                (lambda () ,@(cdr clause))))
                     clauses))))

(put 'mcase 'lisp-indent-function 1)

;;;###autoload
(defmacro mcase (object &rest clauses)
  "Pattern-matching case expression.
The syntax is like the normal `case':

  (mcase EXPR
    (PATTERN . BODY)
    ...)

The body of the first matching pattern is executed, with pattern
variables bound to their matching values. If no patterns match, an
error is signaled.

See `mcase-let' for a description of pattern syntax."
  `(mcase* ,object ,(mcase-parse-clauses clauses)))

(define-obsolete-function-alias 'mlet 'mcase-let "2014-01-12")

(put 'mcase-let 'lisp-indent-function 2)

;;;###autoload
(defmacro mcase-let (pattern object &rest body)
  "Match PATTERN with OBJECT, and execute BODY with all bindings.
The pattern syntax is:

Trivial: t, nil, 42
  Testing with `equal'
Pattern variable: x, my-variable
  Variable that the pattern should bind. If the same variable
  appears several times in a pattern, then all of its bindings must
  match.
  Within the body of a successful pattern match, lisp variables are
  bound for all pattern variables.
Constant: 'symbol, '(1 2 3), ...
  Quoted constant, matched with `equal'.
Bound variable: ,var
  Pre-bound Lisp variable, matched by value.
Wild card: _ (underscore)
  Matches anything, with no binding.
Sequence: (pat1 ...), [pat1 ...]
  Matches the \"shape\" of the pattern, as well as each individual
  subpattern."
  (let ((var (make-symbol "var")))
    `(let ((,var ,object))      ; so that we just eval `object' once
       (mcase ,var
         (,pattern ,@body)
         (_        (signal 'erl-exit-signal
                           (list (tuple 'badmatch ',pattern ,var))))))))

(defun mcase* (object clauses)
  (let ((clause (mcase-choose object clauses)))
    (if clause
        (funcall clause)
      (signal 'erl-exit-signal '(case-clause)))))

(defun mcase-choose (object clauses)
  (if (null clauses)
      nil
    (let* ((clause  (car clauses))
           (pattern (car clause))
           (action  (cadr clause))
           (result  (mcase-match pattern object)))
      (if (eq result 'fail)
          (mcase-choose object (cdr clauses))
        `(lambda ()
           (let ,(mcase-alist-to-letlist result)
             (funcall ,action)))))))

(defun mcase-alist-to-letlist (alist)
  "Convert an alist into `let' binding syntax, eg: ((A . B)) => ((A 'B))"
  (mapcar (lambda (cell)
            (list (car cell) (list 'quote (cdr cell))))
          alist))

(defun mcase-tail (seq)
  (if (consp seq)
      (cdr seq)
    (let ((new (make-vector (1- (length seq)) nil)))
      (dotimes (i (length new))
        (aset new i (aref seq (1+ i))))
      new)))

(defun mcase-match (pattern object &optional bindings)
  "Match OBJECT with PATTERN, and return an alist of bindings."
  (if (eq bindings 'fail)
      'fail
    (cond ((mcase-wildcard-p pattern)
           bindings)
          ((mcase-constant-p pattern) ; '(x)
           (mcase-constant pattern object bindings))
          ((mcase-bound-var-p pattern) ; ,foo
           (mcase-match-var pattern object bindings))
          ((mcase-unbound-var-p pattern) ; foo
           (mcase-bind-var pattern object bindings))
          ((mcase-trivial-p pattern) ; nil, t, any-symbol
           (if (equal pattern object) bindings 'fail))
          ((consp pattern)
           (if (consp object)
               (mcase-match (cdr pattern) (cdr object)
                            (mcase-match (car pattern) (car object) bindings))
             'fail))
          ((vectorp pattern)
           (if (and (vectorp object)
                    (= (length pattern) (length object)))
               (mcase-match (cl-coerce pattern 'list)
                            (cl-coerce object 'list)
                            bindings)
             'fail))
          (t
           'fail))))

(defun mcase-wildcard-p (pat)
  (eq pat '_))

(defun mcase-trivial-p (pat)
  "Test for patterns which can always be matched literally with `equal'."
  (or (numberp pat)
      (equal pat [])
      (equal pat nil)
      (equal pat t)))

(defun mcase-constant-p (pat)
  "Test for (quoted) constant patterns.
Example: (QUOTE QUOTE)"
  (and (consp pat)
       (= (length pat) 2)
       (eq (car pat) 'quote)))

(defun mcase-constant-value (pat)
  "The value of a constant pattern.  (QUOTE X) => X"
  (cadr pat))

(defun mcase-constant (pat object bindings)
  "Match OBJECT with the constant pattern PAT."
  (if (equal (mcase-constant-value pat) object)
      bindings
    'fail))

(defun mcase-unbound-var-p (obj)
  "Unbound variable is any symbol except nil or t."
  (and (symbolp obj)
       (not (eq obj nil))
       (not (eq obj t))))

(defun mcase-unbound-var-symbol (sym)
  sym)

(defun mcase-bind-var (pat object bindings)
  "Add a binding of pattern variable VAR to OBJECT in BINDINGS."
  (if (eq object erl-tag)
      ;; `erl-tag' cannot bind to a variable; this is to prevent pids
      ;; or ports from matching tuple patterns.
      'fail
    (let* ((var (mcase-unbound-var-symbol pat))
           (binding (assoc var bindings)))
      (cond ((null binding)
             (acons var object bindings))
            ((equal (cdr binding) object)
             bindings)
            (t
             'fail)))))

(defun mcase-match-var (var object bindings)
  "Match the value of the Lisp variable VAR with OBJECT."
  (if (equal (symbol-value (mcase-bound-var-name var)) object)
      bindings
    'fail))

(defun mcase-bound-var-p (obj)
  (and (symbolp obj)
       (eq (elt (symbol-name obj) 0) ?,)))

(defun mcase-bound-var-name (sym)
  (intern (substring (symbol-name sym) 1)))

(defun mcase-alist-keysort (alist)
  (sort alist (lambda (a b)
                (string< (symbol-name (car a))
                         (symbol-name (car b))))))

;;; Test suite

(defun mcase-expect (pattern object expected)
  "Assert that matching PATTERN with OBJECT yields EXPECTED.
EXPECTED is either 'fail or a list of bindings (in any order)."
  (let ((actual (mcase-match pattern object)))
    (if (or (and (eq actual 'fail)
                 (eq actual expected))
            (and (listp expected)
                 (listp actual)
                 (equal (mcase-alist-keysort actual)
                        (mcase-alist-keysort expected))))
        t
      (error "mcase: %S %S => %S, expected %S"
             pattern object actual expected))))

(defun mcase-test ()
  "Test the pattern matcher."
  (interactive)
  (mcase-expect t t ())
  (mcase-expect '(t nil 1) '(t nil 1) ())
  (let ((foo 'foo))
    (mcase-expect '(FOO ,foo 'foo [FOO]) '(foo foo foo [foo])
                   '((FOO . foo))))
  (mcase-expect 1 2 'fail)
  (mcase-expect '(x x) '(1 2) 'fail)
  (mcase-expect '_ '(1 2) 'nil)
  (assert (equal 'yes
                 (mcase '(call 42 lists length ((1 2 3)))
                   (t 'no)
                   (1 'no)
                   ((call Ref 'lists 'length (_))
                    'yes)
                   (_ 'no))))
  (message "Smooth sailing"))

(provide 'mcase)
