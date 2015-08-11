;;; erlext.el --- Encoding and decoding of Erlang external term format

;; Copyleft (]) 2000-2002 Luke Gorrie <luke@bluetail.com>
;; Version: $Id: erlext.el,v 1.1 2004/10/25 19:55:57 lukeg Exp $
;; Keywords: erlang

;;; Commentary:
;;
;; Library for encoding/decoding elisp terms into erlang's external
;; term format.  For format details see erts/emulator/internal_doc/ in
;; the Erlang/OTP sources.
;;
;; Supported mappings from/to erlext to/from elisp:
;;   atom    -> symbol
;;   string  -> string
;;   integer -> integer
;;   bignum  -> math-bignum
;;   float   -> float
;;   list    -> list
;;   tuple   -> (vector ...)
;;   pid     -> (vector ERL-TAG 'pid node id serial creation)
;;   binary  -> string
;;   map     -> hashtable
;; Not mapped/supported yet:
;;   ref, port, function, ...
;;
;; ----------------------------------------------------------------------
;; Revision history:
;;
;; Originally written some time in 2000, borrowing lots of code that I
;; didn't understand from Lennart Staflin's nice elisp CORBA client.
;;
;; May 2001: Added asynchronous networking support for the "shbuf"
;; program that shares emacs buffers on the network via an erlang
;; server.
;;
;; March 2002: Big cleanup for use in distributed erlang. Removed the
;; old networking code.

(eval-when-compile (require 'cl))

;; type tags

(defconst erlext-tag-alist
  '((cached     . 67)
    (newFloat   . 70)
    (smallInt   . 97)
    (int        . 98)
    (float      . 99)                   ;superseded by newFloat
    (atom       . 100)
    (ref        . 101)                  ;superseded by newRef
    (port       . 102)
    (pid        . 103)
    (smallTuple . 104)
    (largeTuple . 105)
    (null       . 106)
    (string     . 107)
    (list       . 108)
    (bin        . 109)
    (smallBig   . 110)
    (largeBig   . 111)
    (newRef     . 114)
    (map        . 116)))

(defconst erlext-max-atom-length 255 "The maximum length of an erlang atom.")
(defconst erlext-protocol-version 131)

(defconst erlext-empty-symbol (intern "")
  "The zero-length lisp symbol.")

(defvar erl-tag (make-symbol "TYPE")
  "Tag placed in the first element of a vector to indicate a non-tuple type.")

;; ------------------------------------------------------------
;; Encoding / decoding interface
;; ------------------------------------------------------------

(defun erlext-binary-to-term (string)
  "Decode and return the elisp representation of `string'."
  (assert (stringp string))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert string)
    (goto-char (point-min))
    (erlext-read-whole-obj)))

(defun erlext-term-to-binary (term)
  "Encode `term' as erlext and return the result as a string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert erlext-protocol-version)
    (erlext-write-obj term)
    (buffer-string)))

;; Tuple datatype: (tuple X Y Z) => [X Y Z]

(defun tuple (&rest elems)
  (apply #'vector elems))

(defun tuple-to-list (x)
  (assert (tuplep x))
  (mapcar #'identity x))

(defun tuplep (x)
  (and (vectorp x)
       (or (zerop (length x))
           (not (eq (elt x 0) erl-tag)))))

(defun tuple-arity (tuple)
  (1- (length tuple)))

(defmacro tuple-elt (tuple index)
  "Return element INDEX from TUPLE. Index starts from 1."
  ;; Defined as a macro just so that we get the setf of `elt' for free
  `(elt ,tuple (1- ,index)))

;; ------------------------------------------------------------
;; Encoding
;; ------------------------------------------------------------

(defun erlext-write-obj (obj)
  (cond ((listp obj)                    ; lists at top since (symbolp '()) => t
         (if (and (memq (car obj) '(bigneg bigpos))
                  (ignore-errors (cl-every #'integerp (cdr obj))))
             ;; math-bignum
             (erlext-write-bignum obj)
           (erlext-write-list obj)))
        ((stringp obj)
         (erlext-write-string obj))
        ((symbolp obj)
         (erlext-write-atom obj))
        ((vectorp obj)
         (if (tuplep obj)
             (erlext-write-tuple (tuple-to-list obj))
           (let* ((list (mapcar #'identity obj))
                  (type (cadr list))
                  (elts (cddr list)))
             (ecase type
               ((erl-pid)
                (apply #'erlext-write-pid elts))
               ((erl-port)
                (apply #'erlext-write-port elts))
               ((erl-ref)
                (apply #'erlext-write-ref elts))
               ((erl-new-ref)
                (apply #'erlext-write-new-ref elts))
               ((erl-binary)
                (erlext-write-binary (car elts)))))))
        ((hash-table-p obj)
         (erlext-write-map obj))
        ((integerp obj)
         (erlext-write-int obj))
        ((floatp obj)
         (erlext-write-float obj))
        (t
         (error "erlext can't marshal %S" obj))))

(defun erlext-write1 (n)
  (assert (integerp n))
  (insert n))
(defun erlext-write2 (n)
  (assert (integerp n))
  (insert (logand (ash n -8) 255)
          (logand n 255)))
(defun erlext-write4 (n)
  (assert (integerp n))
  (insert (logand (ash n -24) 255)
          (logand (ash n -16) 255)
          (logand (ash n -8) 255)
          (logand n 255)))
(defun erlext-writen (bytes)
  (assert (stringp bytes))
  (insert bytes))
(defun erlext-insert4 (n offset)
  (goto-char offset)
  (erlext-write4 n)
  (goto-char (point-max)))

(defun erlext-write-atom (atom)
  (assert (symbolp atom))
  (let* ((string (symbol-name atom))
         (len    (length string)))
    (assert (<= len erlext-max-atom-length))
    (erlext-write1 (erlext-get-code 'atom))
    (erlext-write2 (length string))
    (erlext-writen string)))

;;; Test for erlext-write-int
;; (cl-labels ((test (x expected)
;;               (with-temp-buffer
;;                 (set-buffer-multibyte nil)
;;                 (erlext-write-int x)
;;                 (goto-char (point-min))
;;                 (equal (cl-loop repeat (length expected)
;;                                 collect (prog1 (get-byte) (forward-char 1)))
;;                        expected))))
;;   (cl-mapcar #'test
;;              (list -953877499181397206
;;                    953877499181397206
;;                    most-positive-fixnum ; 2305843009213693951
;;                    most-negative-fixnum ; -2305843009213693952
;;                    )
;;              ;; Results
;;              '((110 8 1 214 244 241 25 136 218 60 13)
;;                (110 8 0 214 244 241 25 136 218 60 13)
;;                (110 8 0 255 255 255 255 255 255 255 31)
;;                (110 8 1 0 0 0 0 0 0 0 32))))
;; => (t t t t)
(defun erlext-write-int (n)
  (assert (integerp n))
  (cond ((= n (logand n 255))
         (erlext-write1 (erlext-get-code 'smallInt))
         (erlext-write1 n))
        ((or (< (log most-positive-fixnum 2) 32)
             (= n (logand n #xffffffff)))
         (erlext-write1 (erlext-get-code 'int))
         (erlext-write4 n))
        (t
         ;; Could use (erlext-write-bignum (math-bignum n)) but may
         ;; fail for `most-negative-fixnum'.
         ;; See http://debbugs.gnu.org/17556
         ;;
         ;; Let's not depend on calc.el unless necessary
         ;;
         (erlext-write1 (erlext-get-code 'smallBig))
         ;; Note: (= (abs most-negative-fixnum) most-negative-fixnum)
         (erlext-write1 (1+ (ceiling (log (abs (ash n -8)) 256))))
         (erlext-write1 (if (< n 0) 1 0))
         (while (not (zerop n))
           (erlext-write1 (logand (abs n) 255))
           (setq n (ash (abs n) -8))))))

(autoload 'math-lessp   "calc-ext")
(autoload 'math-idivmod "calc")
(autoload 'math-abs     "calc-arith")
(autoload 'math-zerop   "calc-misc")

(defun erlext-write-bignum (bignum)
  (let ((sign (if (math-lessp bignum 0) 1 0))
        (bytes (loop for (x . mod) = (math-idivmod (math-abs bignum) 256)
                     then (math-idivmod x 256)
                     collect mod into result
                     do (and (math-zerop x) (return result)))))
    (cond
     ((null (nthcdr 255 bytes))
      (erlext-write1 (erlext-get-code 'smallBig))
      (erlext-write1 (length bytes)))
     (t
      (erlext-write1 (erlext-get-code 'largeBig))
      (erlext-write4 (length bytes))))
    (erlext-write1 sign)
    (mapc #'erlext-write1 bytes)))

;; Test erlext-encode-ieee-double
;;
;; (cl-mapcar (lambda (x y) (equal (erlext-encode-ieee-double x) y))
;;            '(1e+INF -1e+INF -12.2 1.0000000000000004)
;;            '((127 240 0 0 0 0 0 0)
;;              (255 240 0 0 0 0 0 0)
;;              (192 40 102 102 102 102 102 102)
;;              (63 240 0 0 0 0 0 2)))
;; => (t t t t)
(defun erlext-encode-ieee-double (n)
  ;; Ref: http://en.wikipedia.org/wiki/Double-precision_floating-point_format
  (cl-labels ((fill-mantissa (vec frac)
             (loop for i from 12 to 63
                   for tmp = (- frac (expt 0.5 (- i 11)))
                   when (>= tmp 0)
                   do (setf (aref vec i) (prog1 1 (setq frac tmp)))))
           (bytes (bits)
             (loop for i from 0 to 63
                   for b = 7 then (1- b)
                   sum (* (aref bits i) (expt 2 b)) into byte
                   when (zerop b)
                   collect (prog1 byte (setf byte 0 b 8)))))
    (let* ((result (make-vector 64 0))
           (bias 1023)
           (E (frexp n))
           (S (pop E)))
      ;; Sign
      (when (< S 0)
        (setf (aref result 0) 1))
      ;; Exponent & Mantissa
      (cond ((isnan n) (cl-fill result 1 :start 1 :end 64))
            ((member S '(1.0e+INF -1.0e+INF))
             (cl-fill result 1 :start 1 :end 12)
             (cl-fill result 0 :start 12 :end 64))
            ((zerop E)                  ; subnormals
             (cl-fill result 0 :start 1 :end 12)
             (fill-mantissa result S))
            ;; Move factor 2 to S so that S >= 1.0
            (t (loop for x = (+ (1- E) bias) then (ash x -1)
                     for i from 11 downto 1
                     do (setf (aref result i) (logand x 1)))
               (fill-mantissa result (1- (* 2 (abs S))))))
      (bytes result))))

(defun erlext-write-float (n)
  (cond
   ((fboundp 'frexp)
    ;; Function `frexp' was introduced in emacs 24.1;
    ;; Erlang R11B-4 and later is able to decode this representation.
    (erlext-write1 (erlext-get-code 'newFloat))
    (mapc #'erlext-write1 (erlext-encode-ieee-double n)))
   (t
    (erlext-write1 (erlext-get-code 'float))
    (let ((f (format "%.20e" n)))
      (erlext-writen (concat f (make-string
                                (max 0 (- 31 (length f))) 0)))))))

(defun erlext-write-list (lst)
  (assert (listp lst))
  (if (null lst)
      (erlext-write-null)
    (progn (erlext-write-list-head (length lst))
           (mapc 'erlext-write-obj lst)
           (erlext-write-null))))
(defun erlext-write-string (str)
  (assert (stringp str))
  (erlext-write1 (erlext-get-code 'string))
  (erlext-write2 (length str))
  (erlext-writen str))
(defun erlext-write-binary (str)
  (assert (stringp str))
  (erlext-write1 (erlext-get-code 'bin))
  (erlext-write4 (length str))
  (erlext-writen str))
(defun erlext-write-null ()
  (erlext-write1 (erlext-get-code 'null)))
(defun erlext-write-list-head (arity)
  (assert (> arity 0))
  (erlext-write1 (erlext-get-code 'list))
  (erlext-write4 arity))

(defun erlext-write-tuple (elts)
  (assert (listp elts))
  (let ((arity (length elts)))
    (if (< arity 256)
        (progn (erlext-write1 (erlext-get-code 'smallTuple))
               (erlext-write1 arity))
      (progn (erlext-write1 (erlext-get-code 'largeTuple))
             (erlext-write4 arity))))
  (mapc 'erlext-write-obj elts))
(defun erlext-write-pid (node id serial creation)
  (erlext-write1 (erlext-get-code 'pid))
  (erlext-write-obj node)
  (erlext-write4 id)
  (erlext-write4 serial)
  (erlext-write1 creation))
(defun erlext-write-port (node id creation)
  (erlext-write1 (erlext-get-code 'port))
  (erlext-write-obj node)
  (erlext-write4 id)
  (erlext-write1 creation))
(defun erlext-write-ref (node id creation)
  (erlext-write1 (erlext-get-code 'ref))
  (erlext-write-obj node)
  (erlext-write4 id)
  (erlext-write1 creation))
(defun erlext-write-new-ref (node creation id)
  (erlext-write1 (erlext-get-code 'newRef))
  (erlext-write2 (/ (length id) 4))
  (erlext-write-obj node)
  (erlext-write1 creation)
  (erlext-writen id))

(defun erlext-write-map (obj)
  (erlext-write1 (erlext-get-code 'map))
  (erlext-write4 (hash-table-count obj))
  (maphash (lambda (k v)
             (erlext-write-obj k)
             (erlext-write-obj v))
           obj))

;; ------------------------------------------------------------
;; Decoding
;; ------------------------------------------------------------

(eval-and-compile
  (if (fboundp 'char-int)
      ;; convert character to string
      (defsubst erlext-read1 ()
        (prog1 (char-int (following-char))
          (forward-char 1)))
    (defsubst erlext-read1 ()
      (prog1 (following-char)
        (forward-char 1)))))

(defun erlext-read-whole-obj ()
  (let ((version (erlext-read1)))
    (assert (= version erlext-protocol-version))
    (erlext-read-obj)))

(defun erlext-read-obj ()
  (let ((tag (erlext-get-tag (erlext-read1))))
    (case tag
      ((smallInt)   (erlext-read1))
      ((int)        (erlext-read4))
      ((newFloat)   (erlext-read-ieee-double))
      ((float)      (erlext-read-float))
      ((atom)       (erlext-read-atom))
      ((smallTuple) (erlext-read-small-tuple))
      ((largeTuple) (erlext-read-large-tuple))
      ((list)       (erlext-read-list))
      ((string)     (erlext-read-string))
      ((bin)        (erlext-read-binary))
      ((null)       nil)
      ((nil)        nil)
      ((pid)        (vector erl-tag
                            'erl-pid
                            (erlext-read-obj) ; node
                            (erlext-read4)    ; id
                            (erlext-read4)    ; serial
                            (erlext-read1)))  ; creation
      ((port)       (vector erl-tag
                            'erl-port
                            (erlext-read-obj) ; node
                            (erlext-read4)    ; id
                            (erlext-read1)))  ; creation
      ((ref)        (vector erl-tag
                            'erl-ref
                            (erlext-read-obj) ; node
                            (erlext-read4)    ; id
                            (erlext-read1)))  ; creation
      ((newRef)     (erlext-read-newref))
      ((smallBig)   (erlext-read-small-bignum))
      ((largeBig)   (erlext-read-large-bignum))
      ((map)        (erlext-read-map))
      (t
       (error "Unknown tag: %S" tag)))))

(defun erlext-read (size)
  (case size
    ((1) (erlext-read1))
    ((2) (erlext-read2))
    ((4) (erlext-read4))))
;; read1 moved above so that it can be inlined
(defun erlext-read2 ()
  (logior (ash (erlext-read1) 8)
          (erlext-read1)))
(defun erlext-read4 ()
  (logior (ash (erlext-read1) 24)
          (ash (erlext-read1) 16)
          (ash (erlext-read1) 8)
          (erlext-read1)))

(defun erlext-readn (n)
  (assert (integerp n))
  (let ((start (point))
        (end   (+ (point) n)))
    (prog1 (let ((string (buffer-substring start end)))
             (if (featurep 'xemacs)
                 string
               (string-as-unibyte string))) ; fixme: should be
                                            ; string-make-unibyte?
                                            ; Why is it necessary
                                            ; anyhow?
      (goto-char end))))

;; Test:
;; (cl-labels ((test (x)
;;               (with-temp-buffer
;;                 (set-buffer-multibyte nil)
;;                 (apply #'insert (erlext-encode-ieee-double x))
;;                 (goto-char (point-min))
;;                 (equal (erlext-read-ieee-double) x))))
;;   (mapcar #'test '(1.0e+INF -1.0e+INF -12.2 1.0000000000000004)))
;; => (t t t t)
(defun erlext-read-ieee-double ()
  (cl-labels ((to-bits (byte)
             (nreverse (loop repeat 8
                             collect (prog1 (logand byte 1)
                                       (setq byte (ash byte -1)))))))
    (let* ((bias 1023)
           (bits (apply #'vector (loop repeat 8
                                       append (to-bits (erlext-read1)))))
           (sign (if (zerop (aref bits 0)) 1 -1))
           (exponent (loop for i from 11 downto 1
                           sum (if (zerop (aref bits i))
                                   0
                                 (ash 1 (- 11 i)))))
           (fraction (loop for i from 12 to 63
                           sum (* (aref bits i)
                                  (expt 0.5 (- i 11))))))
      (case exponent
        (#x7ff (if (zerop fraction)
                   (* sign 1.0e+INF)
                 0.0e+NaN))
        (0 (* sign (expt 2 (1- bias)) fraction))
        (t (* sign (expt 2 (- exponent bias)) (1+ fraction)))))))

(defun erlext-read-float ()
  (string-to-number (erlext-readn 31)))

(defun erlext-read-atom ()
  (let ((length (erlext-read2)))
    (intern (erlext-readn length))))
(defun erlext-read-small-tuple ()
  (erlext-read-tuple (erlext-read1)))
(defun erlext-read-large-tuple ()
  (erlext-read-tuple (erlext-read4)))
(defun erlext-read-list ()
  (let ((arity (erlext-read4)))
    (prog1 (loop for x from 1 to arity
                 collect (erlext-read-obj))
      ;; This seems fishy, I find nil's at the end of lists, not
      ;; included as elements, and no mention of how it works in the
      ;; erl_ext_dist.txt
      (assert (eq (erlext-get-code 'null) (erlext-read1))))))
(defun erlext-read-tuple (arity)
  (apply #'vector (loop for x from 1 to arity
                        collect (erlext-read-obj))))

(defun erlext-read-string ()
  (erlext-readn (erlext-read2)))

(defun erlext-read-binary ()
  (erlext-readn (erlext-read4)))

(defun erlext-read-newref ()
  (let* ((len (erlext-read2))
         (node (erlext-read-obj))
         (creation (erlext-read1))
         (id (erlext-readn (* 4 len))))
    (vector erl-tag 'erl-new-ref node creation id)))

(autoload 'math-mul "calc")
(autoload 'math-add "calc")

(defun erlext-read--bignum (n sign)
  ;; Helper routine for `erlext-read-small-bignum' and
  ;; `erlext-read-large-bignum'.
  (loop with result = 0
        repeat n
        for b = 1 then (math-mul b 256)
        do (setq result (math-add result (math-mul b (erlext-read1))))
        finally (return (math-mul (if (zerop sign) 1 -1) result))))

(defun erlext-read-small-bignum ()
  (erlext-read--bignum (erlext-read1) (erlext-read1)))

(defun erlext-read-large-bignum ()
  (erlext-read--bignum (erlext-read4) (erlext-read1)))

(defun erlext-read-map ()
  (let ((table (make-hash-table :test #'equal)))
    (loop repeat (erlext-read4)
          do (puthash (erlext-read-obj) (erlext-read-obj) table))
    table))

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

(defun erlext-get-tag (number)
  (car (rassq number erlext-tag-alist)))
(defun erlext-get-code (tag)
  (cdr (assq tag erlext-tag-alist)))

;; ------------------------------------------------------------
;; Testing
;; ------------------------------------------------------------

(defvar erlext-test-cases
  `(1 foo "bar" [bar baz] [,erl-tag erl-pid someone@somehost 0 0 0] (1 foo ())
      [,erl-tag erl-port someone@somehost 0 0]
      (([1 2]) ([1 2]))))

(defun erlext-test ()
  "Test each term in `erlext-test-cases' by encoding it and decoding
it and making sure that it's unchanged."
  (interactive)
  (mapc #'erlext-test-case erlext-test-cases)
  (message "Smooth sailing"))

(defun erlext-test-case (term)
  (condition-case x
      (assert (equal term (erlext-binary-to-term (erlext-term-to-binary term))))
    (error (error "test failed for %S: %S" term (error-message-string x)))))

(provide 'erlext)
