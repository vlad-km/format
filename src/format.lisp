;;; -*- mode:lisp;  coding:utf-8 -*-

#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                                                               2023,  @vlad-km
            /     \                   
            )     (                   
           /       \                  Electron >= electron@21.2.2
           \       /                  JSCL >= version 0.8.2  
      jgs   \__ __/
               ))
              //
             ((
              \)
|#

;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Functions to implement FORMAT and FORMATTER for CMU Common Lisp.
;;;
;;; Written by William Lott, with lots of stuff stolen from the previous
;;; version by David Adam and later rewritten by Bill Maddox.
;;;

(defparameter +digits+ "0123456789")
(defconstant +format-directive-limit+ (1+ (char-code #\~)))
(defvar *format-directive-expanders* (make-array +format-directive-limit+
                                                 :initial-element nil))
(defvar *format-directive-interpreters* (make-array +format-directive-limit+
                                                    :initial-element nil))
(defvar *default-format-error-control-string* nil)
(defvar *default-format-error-offset* nil)


;;;; Specials used to communicate information.

;;; *UP-UP-AND-OUT-ALLOWED* -- internal.
;;; Used both by the expansion stuff and the interpreter stuff.  When it is
;;; non-NIL, up-up-and-out (~:^) is allowed.  Otherwise, ~:^ isn't allowed.
(defvar *up-up-and-out-allowed* nil)

;;; *LOGICAL-BLOCK-POPPER* -- internal.
;;; Used by the interpreter stuff.  When it non-NIL, its a function that will
;;; invoke PPRINT-POP in the right lexical environemnt.
(defvar *logical-block-popper* nil)

;;; *EXPANDER-NEXT-ARG-MACRO* -- internal.
;;; Used by the expander stuff.  This is bindable so that ~<...~:>
;;; can change it.
(defvar *expander-next-arg-macro* 'expander-next-arg)

;;; *ONLY-SIMPLE-ARGS* -- internal.
;;; Used by the expander stuff.  Initially starts as T, and gets set to NIL
;;; if someone needs to do something strange with the arg list (like use
;;; the rest, or something).
(defvar *only-simple-args*)

;;; *ORIG-ARGS-AVAILABLE* -- internal.
;;; Used by the expander stuff.  We do an initial pass with this as NIL.
;;; If someone doesn't like this, they (throw 'need-orig-args nil) and we try
;;; again with it bound to T.  If this is T, we don't try to do anything
;;; fancy with args.
(defvar *orig-args-available* nil)

;;; *SIMPLE-ARGS* -- internal.
;;; Used by the expander stuff.  List of (symbol . offset) for simple args.
(defvar *simple-args*)


(defun %print-format-error (condition stream)
  (das!format stream
	           "~&Error in format:~& ~a ~&args: ~a~&ctrl: ~a~&"
	           (format-error-complaint condition)
	           (format-error-arguments condition)
	           (format-error-control-string condition)))


;;; stub for SVREF
(jscl::fset 'svref (fdefinition 'aref))


#+nil (defvar *default-format-error-control-string* nil)
#+nil (defvar *default-format-error-offset* nil)

(define-condition format-error (error)
  ((complaint :reader format-error-complaint
              :initarg :complaint)
   (arguments :reader format-error-arguments
              :initarg :arguments
              :initform nil)
   (control-string :reader format-error-control-string
		               :initarg :control-string
		               :initform *default-format-error-control-string*) 
   (offset :reader format-error-offset
           :initarg :offset
	         :initform *default-format-error-offset*)
   (print-banner :reader format-error-print-banner
                 :initarg :print-banner
		             :initform t))
  (:report (lambda (condition stream)(%print-format-error condition stream))))

(defstruct (format-directive :named (:type vector))
  (string t :type string)
  (start  0 :type integer)
  (end    0 :type integer)
  (character #\Space :type character)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
        (end (length string)))
    (flet ((get-char ()
             (if (= posn end)
                 (error 'format-error
                        :complaint "String ended before directive was found."
                        :control-string string
                        :offset start)
                 (char string posn))))
      (loop
        (let ((char (get-char)))
          (cond ((and (not colonp) (not atsignp)
                      (or (char<= #\0 char #\9) (char= char #\+) (char= char #\-)))
                 (multiple-value-bind (param new-posn)
                     (parse-integer string :start posn :junk-allowed t)
                   (push (cons posn param) params)
                   (setf posn new-posn)
                   (case (get-char)
                     (#\,)
                     ((#\: #\@)
                      (decf posn))
                     (t (return)))))
                ((and (not colonp) (not atsignp) (or (char= char #\v) (char= char #\V)))
                 (push (cons posn :arg) params)
                 (incf posn)
                 (case (get-char)
                   (#\,)
                   ((#\: #\@)
                    (decf posn))
                   (t (return))))
                ((and (not colonp) (not atsignp)(char= char #\#))
                 (push (cons posn :remaining) params)
                 (incf posn)
                 (case (get-char)
                   (#\,)
                   ((#\: #\@)
                    (decf posn))
                   (t (return))))
                ((and (not colonp) (not atsignp) (char= char #\'))
                 (incf posn)
                 (push (cons posn (get-char)) params)
                 (incf posn)
                 (unless (char= (get-char) #\,) (decf posn)))
                ((and (not colonp) (not atsignp) (char= char #\,))
                 (push (cons posn nil) params))
                ((char= char #\:)
                 (if colonp
                     (error 'format-error
                            :complaint "Too many colons supplied."
                            :control-string string
                            :offset posn)
                     (setf colonp t)))
                ((char= char #\@)
                 (if atsignp
                     (error 'format-error
                            :complaint "Too many at-signs supplied."
                            :control-string string
                            :offset posn)
                     (setf atsignp t)))
                (t  (return))))
        (incf posn))
      (let ((char (get-char)))
        (when (char= char #\/)
          (let ((closing-slash (position #\/ string :start (1+ posn))))
            (if closing-slash
                (setf posn closing-slash)
                (error 'format-error
                       :complaint "No matching closing slash."
                       :control-string string
                       :offset posn))))
        (make-format-directive :string string :start start :end (1+ posn)
                               :character (char-upcase char)
                               :colonp colonp :atsignp atsignp
                               :params (nreverse params))))))

(defun tokenize-control-string (string)
  (let ((index 0)
        (end (length string))
        (result nil))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
        (when (> next-directive index)
          (push (subseq string index next-directive) result))
        (when (= next-directive end)
          (return))
        (let ((directive (parse-directive string next-directive)))
          (push directive result)
          (setq index (format-directive-end directive)))))
    (nreverse result)))


(defun %set-format-directive-expander (char fn)
  (setf (aref *format-directive-expanders* (char-code (char-upcase char))) fn)
  char)

(defun %set-format-directive-interpreter (char fn)
  (setf (aref *format-directive-interpreters* (char-code (char-upcase char)))
        fn)
  char)

(defun find-directive (directives kind stop-at-semi)
  (if directives
      (let ((next (car directives)))
        (if (format-directive-p next)
            (let ((char (format-directive-character next)))
              (if (or (char= kind char)(and stop-at-semi (char= char #\;)))
                  (car directives)
                  (find-directive
                   (cdr (flet ((after (char)
                                 (member (find-directive (cdr directives) char nil)
                                         directives)))
                          (case char
                            (#\( (after #\)))
                            (#\< (after #\>))
                            (#\[ (after #\]))
                            (#\{ (after #\}))
                            (t directives))))
                   kind stop-at-semi)))
            (find-directive (cdr directives) kind stop-at-semi)))))

(defmacro once-only (specs &body body)
  (labels ((frob (specs body)
             (if (null specs)
                 `(progn ,@body)
                 (let ((spec (first specs)))
                   (cond ((atom spec)
                          (setf spec (list spec spec)))
                         ((/= (length spec) 2)
                          (error "Malformed Once-Only binding spec: ~S." spec)))
                   (let ((name (first spec))
                         (exp-temp (gensym)))
                     `(let ((,exp-temp ,(second spec))
                            (,name (gensym "OO-")))
                        `(let ((,,name ,,exp-temp))
                           ,,(frob (rest specs) body))))))))
    (frob specs body)))

(defmacro interpret-bind-defaults (specs params &body body)
  (once-only ((params params))
    (jscl::with-collector (bindings)
      (dolist (spec specs)
        (destructuring-bind (var default)
            spec
          (collect-bindings `(,var (let* ((param-and-offset (pop ,params))
                                          (offset (car param-and-offset))
                                          (param (cdr param-and-offset)))
                                     (case param
                                       (:arg (or (next-arg offset) ,default))
                                       (:remaining (length args))
                                       ((nil) ,default)
                                       (t param)))))))
      `(let* ,bindings
         (when ,params
           (error 'format-error
                  :complaint
                  "Too many parameters, expected no more than ~D"
                  :arguments (list ,(length specs))
                  :offset (caar ,params)))
         ,@body))))

;;; This macro is used to extract the next argument from the current arg list.
;;; This is the version used by format directive interpreters.
(defmacro next-arg (&optional offset)
  `(progn
     (when (null args)
       (error 'format-error :complaint "No more arguments."
	            ,@(when offset  `(:offset ,offset))))
     ;;(when *logical-block-popper* (funcall *logical-block-popper*))
     (pop args)))

;;; formater

(defmacro formatter (control-string)
  `#',(%formatter control-string))

(defun %formatter (control-string)
  (block nil
    (catch 'need-orig-args
      (let* ((*simple-args* nil)
	           (*only-simple-args* t)
	           (guts (expand-control-string control-string))
	           (args nil))
	      (dolist (arg *simple-args*)
	        (push `(,(car arg)
		              (error 'format-error :complaint "Required argument missing"
		                                   :control-string ,control-string :offset ,(cdr arg)))
		            args))
	      (return `(lambda (stream &optional ,@args &rest args) ,guts args))))
    (let ((*orig-args-available* t)
	        (*only-simple-args* nil))
      `(lambda (stream &rest orig-args)
	       (let ((args orig-args))
	         ,(expand-control-string control-string)
	         args)))))

(defun expand-control-string (string)
  (let* ((string strin)
	       (*default-format-error-control-string* string)
	       (directives (tokenize-control-string string)))
    `(block nil
       ,@(expand-directive-list directives))))

(defun expand-directive-list (directives)
  (let ((results nil)
	      (remaining-directives directives))
    (loop
      (unless remaining-directives (return))
      (multiple-value-bind (form new-directives)
	        (expand-directive (car remaining-directives)(cdr remaining-directives))
	      (when form (push form results))
	      (setf remaining-directives new-directives)))
    (reverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
	           (aref *format-directive-expanders*
		               (char-code (format-directive-character directive))))
	         (*default-format-error-offset*
	           (1- (format-directive-end directive))))
       (if expander
	         (funcall expander directive more-directives)
	         (error 'format-error
		              :complaint "Unknown directive."))))
    (string (values `(write-string ,directive stream)
	                  more-directives))))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(,*expander-next-arg-macro*
        ,*default-format-error-control-string*
	      ,(or offset *default-format-error-offset*))
      (let ((symbol (gensym "FORMAT-ARG-")))
	      (push (cons symbol (or offset *default-format-error-offset*))
	            *simple-args*)
	      symbol)))

;;; note: wtf?
(defun need-hairy-args ()
  (when *only-simple-args*
    ))

(defmacro expander-next-arg (string offset)
  `(if args
       (pop args)
       (error 'format-error :complaint "No more arguments." :control-string ,string
	                          :offset ,offset)))

(defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error :complaint "No more arguments."
	                          :control-string ,string :offset ,offset))
     (pprint-pop)
     (pop args)))

;;; todo: ~:C directive
(defmacro def-complex-format-directive (char lambda-list &body body)
  (let ((defun-name (intern (jscl:!format nil "~:C-FORMAT-DIRECTIVE-EXPANDER" char)))
	      (directive (gensym))
	      (directives (if lambda-list (car (last lambda-list)) (gensym))))
    `(progn
       (defun ,defun-name (,directive ,directives)
	       ,@(if lambda-list
	             `((let ,(mapcar
                        #'(lambda (var)
				                    `(,var
				                      (,(intern (concatenate 'string "FORMAT-DIRECTIVE-" (symbol-name var))
					                              (symbol-package 'foo))
				                       ,directive)))
			                  (butlast lambda-list))
		               ,@body))
	             `(,@body)))
       (%set-format-directive-expander ,char #',defun-name))))

(defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (gensym))
	      (declarations nil)
	      (body-without-decls body))
    (loop
      (let ((form (car body-without-decls)))
	      (unless (and (consp form) (eq (car form) 'declare))
	        (return))
	      (push (pop body-without-decls) declarations)))
    (setf declarations (reverse declarations))
    `(def-complex-format-directive ,char (,@lambda-list ,directives)
       ,@declarations
       (values (progn ,@body-without-decls)
	             ,directives))))

(defmacro expand-bind-defaults (specs params &body body)
  (once-only ((params params))
    (if specs
	      (collect ((expander-bindings) (runtime-bindings))
		      (dolist (spec specs)
		        (destructuring-bind (var default) spec
		          (let ((symbol (gensym)))
		            (expander-bindings
			           `(,var ',symbol))
		            (runtime-bindings
			           `(list ',symbol
			                  (let* ((param-and-offset (pop ,params))
				                       (offset (car param-and-offset))
				                       (param (cdr param-and-offset)))
				                  (case param
				                    (:arg `(or ,(expand-next-arg offset)
					                             ,,default))
				                    (:remaining
				                     (setf *only-simple-args* nil)
				                     '(length args))
				                    ((nil) ,default)
				                    (t param))))))))
		      `(let ,(expander-bindings)
		         `(let ,(list ,@(runtime-bindings))
		            ,@(if ,params
			                (error 'format-error
				                     :complaint
				                     (intl:gettext "Too many parameters, expected no more than ~D")
				                     :arguments (list ,(length specs))
				                     :offset (caar ,params)))
		            ,,@body)))
	      `(progn
	         (when ,params
	           (error 'format-error
		                :complaint (intl:gettext "Too many parameters, expected no more than 0")
		                :offset (caar ,params)))
	         ,@body))))

;;;

(defmacro def-complex-format-interpreter (char lambda-list &body body)
  (let ((defun-name
          (intern
           (concatenate 'string (string char) "-FORMAT-DIRECTIVE-INTERPRETER")))
        (directive (gensym))
        (directives (if lambda-list (car (last lambda-list)) (gensym))))
    `(progn
       (defun ,defun-name (stream ,directive ,directives orig-args args)
         ,@(if lambda-list
               `((let ,(mapcar
                        #'(lambda (var)
                            `(,var
                              (,(intern (concatenate 'string "FORMAT-DIRECTIVE-" (symbol-name var))
                                        (symbol-package 'foo))
                               ,directive)))
			                  (butlast lambda-list))
		               (values (progn ,@body) args)))
               `(,@body)))
       (%set-format-directive-interpreter ,char #',defun-name))))

(defmacro def-format-interpreter (char lambda-list &body body)
  (let ((directives (gensym)))
    `(def-complex-format-interpreter ,char (,@lambda-list ,directives)
       ,@body
       ,directives)))


;;;; Simple outputting noise.
(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (unless padleft (write-string string stream))
  (dotimes (i minpad) (write-char padchar stream))
  (and mincol minpad colinc
       (do ((chars (+ (length string) (max 0 minpad)) (+ chars colinc)))
           ((>= chars mincol))
         (dotimes (i colinc) (write-char padchar stream))))
  (when padleft
    (write-string string stream)))

(defun format-princ (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp)) (princ-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

(defun format-prin1 (stream arg colonp atsignp mincol colinc minpad padchar)
  (format-write-field stream
                      (if (or arg (not colonp))
                          (prin1-to-string arg)
                          "()")
                      mincol colinc minpad padchar atsignp))

;;; FORMAT-PRINT-NUMBER does most of the work for the numeric printing
;;; directives.  The parameters are interpreted as defined for ~D.
(defun format-print-integer (stream number print-commas-p print-sign-p
			                       radix mincol padchar commachar commainterval)
  (let ((*print-base* radix)
	      (*print-radix* nil))
    (if (integerp number)
	      (let* ((text (princ-to-string (abs number)))
	             (commaed (if print-commas-p
			                      (format-add-commas text commachar commainterval)
			                      text))
	             (signed (cond ((minusp number)
			                        (concatenate 'string "-" commaed))
			                       (print-sign-p
			                        (concatenate 'string "+" commaed))
			                       (t commaed))))
          ;; colinc = 1, minpad = 0, padleft = t
	        (format-write-field stream signed mincol 1 0 padchar t))
	      (princ number stream))))

(defun format-add-commas (string commachar commainterval)
  (let ((length (length string)))
    (multiple-value-bind (commas extra) (truncate (1- length) commainterval)
      (let ((new-string (make-string (+ length commas)))
            (first-comma (1+ extra)))
        (replace new-string string :end1 first-comma :end2 first-comma)
        (do ((src first-comma (+ src commainterval))
             (dst first-comma (+ dst commainterval 1)))
            ((= src length))
          (setf (char new-string dst) commachar)
          (replace new-string string :start1 (1+ dst)
                                     :start2 src
                                     :end2 (+ src commainterval)))
        new-string))))

(defmacro interpret-format-integer (base)
  `(if (or colonp atsignp params)
       (interpret-bind-defaults
	      ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
	      params
	      (format-print-integer stream (next-arg) colonp atsignp ,base mincol
			                        padchar commachar commainterval))
       (write (next-arg) :stream stream :base ,base :radix nil :escape nil)))

(def-format-interpreter #\A (colonp atsignp params)
  (if params
      (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                (padchar #\space))
                               params
                               (format-princ stream (next-arg) colonp atsignp
                                             mincol colinc minpad padchar))
      (princ (if colonp (or (next-arg) "()") (next-arg)) stream)))

(def-format-interpreter #\S (colonp atsignp params)
  (cond (params
         (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                   (padchar #\space))
                                  params
                                  (format-prin1 stream (next-arg) colonp atsignp
                                                mincol colinc minpad padchar)))
        (colonp
         (let ((arg (next-arg)))
           (if arg (prin1 arg stream) (princ "()" stream))))
        (t
         (prin1 (next-arg) stream))))

;;; todo: wrong case
(defun format-print-named-character (char stream)
  (let* ((name (char-name char)))
    (cond (name
	         (write-string name stream))
	        ((<= 0 (char-code char) 31)
           ;; Print control characters as "^"<char>
	         (write-char #\^ stream)
	         (write-char (code-char (+ 64 (char-code char))) stream))
	        (t
	         (write-char char stream)))))

(def-format-interpreter #\C (colonp atsignp params)
  (interpret-bind-defaults
   () params
   (if colonp
       (format-print-named-character (next-arg) stream)
       (if atsignp
           (prin1 (next-arg) stream)
           (write-char (next-arg) stream)))))

;;; bug:
(def-format-interpreter #\W (colonp atsignp params)
  (interpret-bind-defaults
   ()
   params
   (let ((*print-pretty* (or colonp *print-pretty*))
	       (*print-level* (and atsignp *print-level*))
	       (*print-length* (and atsignp *print-length*)))
     (output-object (next-arg) stream))))

(def-format-interpreter #\D (colonp atsignp params)
  (interpret-format-integer 10))

(def-format-interpreter #\B (colonp atsignp params)
  (interpret-format-integer 2))

(def-format-interpreter #\O (colonp atsignp params)
  (interpret-format-integer 8))

(def-format-interpreter #\X (colonp atsignp params)
  (interpret-format-integer 16))

(def-format-interpreter #\R (colonp atsignp params)
  (if params
      (interpret-bind-defaults
	     ((base nil) (mincol 0) (padchar #\space) (commachar #\,)
        (commainterval 3))
	     params
	     (if base
	         (format-print-integer stream (next-arg) colonp atsignp base mincol
				                         padchar commachar commainterval)
	         (format-print-cardinal stream (next-arg))))
      (if atsignp
	        (if colonp
	            (format-print-old-roman stream (next-arg))
	            (format-print-roman stream (next-arg)))
	        (if colonp
	            (format-print-ordinal stream (next-arg))
	            (format-print-cardinal stream (next-arg))))))

;;; bug: ? see rep.txt
(def-format-interpreter #\P (colonp atsignp params)
  (interpret-bind-defaults
   () params
   (let ((arg (if colonp
		              (if (eq orig-args args)
		                  (error "~~P - No previous argument.")
		                  (do ((arg-ptr orig-args (cdr arg-ptr)))
			                    ((eq (cdr arg-ptr) args)
			                     (car arg-ptr))))
		              (next-arg))))
     (if atsignp
	       (write-string (if (eql arg 1) "y" "ies") stream)
	       (unless (eql arg 1) (write-char #\s stream))))))

;;; Print Roman numerals
(defun format-print-old-roman (stream n)
  (unless (< 0 n 5000)
    (error "Number too large to print in old Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn
				                        (write-char cur-char stream)
				                        (- i cur-val))))
		                ((< i cur-val) i))))
      ((zerop start))))

(defun format-print-roman (stream n)
  (unless (< 0 n 4000)
    (error "Number too large to print in Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn
				                        (write-char cur-char stream)
				                        (- i cur-val))))
		                ((< i cur-val)
		                 (cond ((<= (- cur-val cur-sub-val) i)
			                      (write-char cur-sub-char stream)
			                      (write-char cur-char stream)
			                      (- i (- cur-val cur-sub-val)))
			                     (t i))))))
	    ((zerop start))))

(defconstant cardinal-ones
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defconstant cardinal-tens
  #(nil nil "twenty" "thirty" "forty"
	"fifty" "sixty" "seventy" "eighty" "ninety"))

(defconstant cardinal-teens
  #("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

;; See http://en.wikipedia.org/wiki/Names_of_large_numbers and also
;; http://home.hetnet.nl/~vanadovv/BignumbyN.html.  This list comes
;; from the latter link.
;;
;; Leading spaces are required to get everything printed out
;; correctly.
(defconstant cardinal-periods
  #("" " thousand" " million" " billion" " trillion" " quadrillion"
    " quintillion" " sextillion" " septillion" " octillion" " nonillion"))

(defconstant ordinal-ones
  #(nil "first" "second" "third" "fourth"
	"fifth" "sixth" "seventh" "eighth" "ninth")
  "Table of ordinal ones-place digits in English")

(defconstant ordinal-tens 
  #(nil "tenth" "twentieth" "thirtieth" "fortieth"
	"fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth")
  "Table of ordinal tens-place digits in English")

(defun format-print-small-cardinal (stream n)
  (multiple-value-bind 
        (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref cardinal-ones hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem)
	      (write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones)
			    (truncate rem 10)
        (cond ((< 1 tens)
	             (write-string (aref cardinal-tens tens) stream)
	             (when (plusp ones)
		             (write-char #\- stream)
		             (write-string (aref cardinal-ones ones) stream)))
	            ((= tens 1)
	             (write-string (aref cardinal-teens ones) stream))
	            ((plusp ones)
	             (write-string (aref cardinal-ones ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
	       (write-string "negative " stream)
	       (format-print-cardinal-aux stream (- n) 0 n))
	      ((zerop n)
	       (write-string "zero" stream))
	      (t
	       (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (< period (length cardinal-periods))
      (error "Number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)(write-char #\space stream))
      (format-print-small-cardinal stream here)
      (write-string (aref cardinal-periods period) stream))))

(defun format-print-ordinal (stream n)
  (when (minusp n)(write-string "negative " stream))
  (let ((number (abs n)))
    (multiple-value-bind
	        (top bot) (truncate number 100)
      (unless (zerop top)
	      (format-print-cardinal stream (- number bot)))
      (when (and (plusp top) (plusp bot))
	      (write-char #\space stream))
      (multiple-value-bind
	          (tens ones) (truncate bot 10)
	      (cond ((= bot 12) (write-string "twelfth" stream))
	            ((= tens 1)
	             (write-string (aref cardinal-teens ones) stream) ;;;RAD
	             (write-string "th" stream))
	            ((and (zerop tens) (plusp ones))
	             (write-string (aref ordinal-ones ones) stream))
	            ((and (zerop ones)(plusp tens))
	             (write-string (aref ordinal-tens tens) stream))
	            ((plusp bot)
	             (write-string (aref cardinal-tens tens) stream)
	             (write-char #\- stream)
	             (write-string (aref ordinal-ones ones) stream))
	            ((plusp number)
	             (write-string "th" stream))
	            (t
	             (write-string "zeroth" stream)))))))

;;; 22.3.3 FORMAT Floating-Point
;;; Tilde F
;;; 
;;; general form    ~w,d,k,overflowchar,padcharF.
;;; monkey style implementation (very restricted) only for print fixed point format
;;;    parameters
;;;                     ~w,d,k,o,p
;;;                      w - width of the field,  default 0
;;;                      d - number of digits after decimal point, default 1
;;;                      k - always ignore
;;;                      o - always ignore
;;;                      p - padchar #\space
;;;   modifiers      @ add "+" if positive value
;;;

(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

(defun format-fixed (stream number w d k ovf pad atsign)
  (if (numberp number)
      (if (floatp number)
	        (format-fixed-aux stream number w d k ovf pad atsign)
          (format-write-field stream (decimal-string number) w 1 0 #\space t) )
      (format-princ stream number nil nil w 1 0 pad)))

;;; w - mincol d - fixed 
;;; (format-fixed-aux    t      123.56789 4 nil nil #\space nil)
(defun format-fixed-aux (stream number w d k ovf pad atsign)
  ;;(print (list :number number :w w :d d :k k :ovf ovf :pad pad :atsign atsign))
  (let* ((negative (if (minusp number) t nil))
         (string (if d ((jscl::oget (abs number) "toFixed") d)
                     ((jscl::oget (abs number) "toString"))))         
         (pad-length (fmt/compute-padding string negative atsign w)))
    (fmt/print-padding pad-length pad stream)
    (cond (negative (write-char #\- stream))
          (atsign (write-char #\+ stream)))
    (write-string string stream)))

(defun string-pad-start (string length &optional (pad " "))
  ((jscl::oget (jscl::lisp-to-js string) "padStart") length pad))

(defun string-pad-end (string length &optional (pad " "))
  ((jscl::oget (jscl::lisp-to-js string) "padStart") length pad))

(defun fmt/compute-padding (string negative at-signp mincol)
  (let* ((comma-length  0)
         (total-length (+ (length string) comma-length (if (or negative at-signp) 1 0)))
         (pad-length (max 0 (- (if mincol mincol 0) total-length))))
    pad-length))

#+nil
(defun fmt/print-padding (how-many char stream)
  (dotimes (repeat how-many) (write-char char stream))
  (values))

(defun fmt/print-padding (how-many char stream)
  (write-string (make-string how-many :initial-element char) stream)
  (values))

(def-format-interpreter #\F (colonp atsignp params)
  (when colonp
    (error "~~F - Cannot specify the colon modifier with this directive."))
  (interpret-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
			                     params
                           (format-fixed stream (next-arg) w d k ovf pad atsignp)))

(def-format-interpreter #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	         :complaint  "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
                           (dotimes (i count)
                             (terpri stream))))

(def-format-interpreter #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	         :complaint "Cannot specify either colon or atsign for this directive ~~&."))
  (interpret-bind-defaults ((count 1)) params
                           (when (plusp count)
                             (fresh-line stream)
                             (dotimes (i (1- count))
	                             (terpri stream)))))

;;; todo: (dotimes (write-char)) -> (write-string (make-string))
(def-format-interpreter #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	         :complaint  "Cannot specify either colon or atsign for this directive ~~."))
  (interpret-bind-defaults ((count 1)) params
                           (dotimes (i count)
                             (write-char #\~ stream))))

;;; todo: ????
(def-complex-format-interpreter #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	         :complaint  "Cannot specify both colon and atsign for this directive."))
  (interpret-bind-defaults () params
                           (when atsignp
                             (write-char #\newline stream)))
  (if (and (not colonp) directives (stringp (car directives)))
      (cons (string-left-trim '(#\space #\newline #\tab) (car directives)) (cdr directives))
      directives))

;;; todo: add relative & absolute
(def-format-interpreter #\T
    (colonp atsignp params)
  (if colonp
      (interpret-bind-defaults ((n 1) (m 1)) params
	                             (pprint-tab (if atsignp :section-relative :section) n m stream))
      (if atsignp
	        (interpret-bind-defaults ((colrel 1) (colinc 1)) params
	                                 (format-relative-tab stream colrel colinc))
	        (interpret-bind-defaults ((colnum 1) (colinc 1)) params
	                                 (format-absolute-tab stream colnum colinc)))))


(def-format-interpreter #\* (colonp atsignp params)
  (if atsignp
      (if colonp
          (error 'format-error :complaint "Cannot specify both colon and at-sign.")
          (interpret-bind-defaults
           ((posn 0))
           params
           (if (<= 0 posn (length orig-args))
               (setf args (nthcdr posn orig-args))
               (error  'format-error
                       :complaint "Index ~D out of bounds.  Should have been between 0 and ~D."
                       :arguments (posn (length orig-args)) ))))
      (if colonp
          (interpret-bind-defaults
           ((n 1))
           params
           (do ((cur-posn 0 (1+ cur-posn))
                (arg-ptr orig-args (cdr arg-ptr)))
               ((eq arg-ptr args)
                (let ((new-posn (- cur-posn n)))
                  (if (<= 0 new-posn (length orig-args))
                      (setf args (nthcdr new-posn orig-args))
                      (error 'format-error
                             :complaint "Index ~D out of bounds.  Should have been  between 0 and ~D."
                             :arguments (new-posn (length orig-args)))   )))))
          (interpret-bind-defaults ((n 1)) params
                                   (dotimes (i n)
                                     (next-arg))))))

;;; todo: bind->case ???
;;; bug:
(def-format-interpreter #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error :complaint "Cannot specify the colon modifier."))
  (interpret-bind-defaults
   () params
   (handler-bind
	     ((format-error
	        #'(lambda (condition)
	            (error 'format-error :complaint "~A~%while processing indirect format string:"
		                               :arguments (list condition)
		                               :print-banner nil
		                               :control-string string
		                               :offset (1- end)))))
     (if atsignp
	       (setf args (%format stream (next-arg) orig-args args))
	       (%format stream (next-arg) (next-arg))))))


;;; todo: redesig ?
(def-format-interpreter #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-function-name string start end)))
    (collect ((args))
      (dolist (param-and-offset params)
	(let ((param (cdr param-and-offset)))
	  (case param
	    (:arg (args (next-arg)))
	    (:remaining (args (length args)))
	    (t (args param)))))
      (apply (fdefinition symbol) stream (next-arg) colonp atsignp (args)))))

(defun extract-user-function-name (string start end)
  (let ((slash (position #\/ string :start start :end (1- end)
			 :from-end t)))
    (unless slash
      (error 'format-error
	     :complaint "Malformed ~~/ directive."))
    (let* ((name (string-upcase (let ((foo string))
				  ;; Hack alert: This is to keep the compiler
				  ;; quiet about deleting code inside the
				  ;; subseq expansion.
				  (subseq foo (1+ slash) (1- end)))))
	   (first-colon (position #\: name))
	   (second-colon (if first-colon (position #\: name :start (1+ first-colon))))
	   (package-name (if first-colon
			     (subseq name 0 first-colon)
			     "COMMON-LISP-USER"))
	   (package (find-package package-name)))
      (unless package
	(error 'format-error
	       :complaint "No package named ~S"
	       :arguments (list package-name)))
      (intern (cond
                ((and second-colon (= second-colon (1+ first-colon)))
                 (subseq name (1+ second-colon)))
                (first-colon
                 (subseq name (1+ first-colon)))
                (t name))
              package))))


;;; Coditionals

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
        (last-semi-with-colon-p nil)
        (remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
        (unless close-or-semi
          (error 'format-error
                 :complaint "No corresponding close bracket."))
        (let ((posn (position close-or-semi remaining)))
          (push (subseq remaining 0 posn) sublists)
          (setf remaining (nthcdr (1+ posn) remaining))
          (when (char= (format-directive-character close-or-semi) #\])
            (return))
          (setf last-semi-with-colon-p
                (format-directive-colonp close-or-semi)))))
    (values sublists last-semi-with-colon-p remaining)))


(def-complex-format-interpreter #\[ (colonp atsignp params directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (setf args
          (if atsignp
              (if colonp
                  (error "'[' - Cannot specify both the colon and at-sign modifiers.")
                  (if (cdr sublists)
                      (error "'[' - Can only specify one section")
                      (interpret-bind-defaults
                       () params
                       (let ((prev-args args)
                             (arg (next-arg)))
                         (if arg
                             (interpret-directive-list stream (car sublists) orig-args prev-args)
                             args)))))
              (if colonp
                  (if (= (length sublists) 2)
                      (interpret-bind-defaults
                       () params
                       (if (next-arg)
                           (interpret-directive-list stream (car sublists) orig-args args)
                           (interpret-directive-list stream (cadr sublists) orig-args args)))
                      (error "'[' - Must specify exactly two sections."))
                  (interpret-bind-defaults
                   ((index (next-arg)))
                   params
                   (let* ((default (and last-semi-with-colon-p (pop sublists)))
                          (last (1- (length sublists)))
                          (sublist
			                      (if (<= 0 index last)
                                (nth (- last index) sublists)
                                default)))
                     (interpret-directive-list stream sublist orig-args args))))))
    remaining))


(def-complex-format-interpreter #\; ()
  (error "~~; not contained within either ~~[...~~] or ~~<...~~>."))

(def-complex-format-interpreter #\] ()
  (error "No corresponding open bracket `]`."))


(defvar *outside-args*)

(def-format-interpreter #\^ (colonp atsignp params)
  (when atsignp
    (error "~~^ - Cannot specify the at-sign modifier."))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error  "Attempt to use ~~:^ outside a ~~:{...~~} construct."))
  ;; This is messy because, as I understand it, and as tested by
  ;; ansi-tests, a NIL parameter is the same as not given.  Thus for 2
  ;; args, if the second is nil, we have to pretend that only 1 was
  ;; given.  Similarly for 3 args.
  ;; Also, ansi-tests interprets CLHS 22.3.9.2 such that "equal"
  ;; parameter means equal (or at least eql).
  ;; FIXME: This needs to be done in a better way!
  (when (case (length params)
          (0 (if colonp
                 (null *outside-args*)
                 (null args)))
          (1 (interpret-bind-defaults ((count nil)) params
                                      (if count (eql count 0)
                                          (if colonp (null *outside-args*)
                                              (null args)))))
          (2 (interpret-bind-defaults ((arg1 nil) (arg2 nil)) params
                                      (if arg2 (eql arg1 arg2)
                                          ;; Should we duplicate the previous case here?
                                          (eql arg1 0))))
          (t (interpret-bind-defaults ((arg1 nil) (arg2 nil) (arg3 nil)) params
                                      (if arg3 (<= arg1 arg2 arg3)
                                          (if arg2 (eql arg1 arg2)
                                              ;; Duplicate the case of 1 arg?
                                              (eql arg1 0))))))
    (throw (if colonp 'up-up-and-out 'up-and-out)
	    args)))


;;;

(def-complex-format-interpreter #\{ (colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error "~~{ - No corresponding close brace."))
    (interpret-bind-defaults
     ((max-count nil))
     params
     (let* ((closed-with-colon (format-directive-colonp close))
            (posn (position close directives))
            (insides (if (zerop posn)
                         (next-arg)
                         (subseq directives 0 posn)))
            (*up-up-and-out-allowed* colonp))
       (labels
           ((do-guts (orig-args args)
              (if (zerop posn)
                  (handler-bind
                      ((format-error
                         #'(lambda (condition)
                             (error 'format-error
                                    :complaint "~A~%while processing indirect format string:"
                                    :arguments (list condition)
                                    :print-banner nil
                                    :control-string string
                                    :offset (1- end)))))
                    (%format stream insides orig-args args))
                  (interpret-directive-list stream insides orig-args args)))
            (bind-args (orig-args args)
              (if colonp
                  (let* ((arg (next-arg))
                         (*logical-block-popper* nil)
                         (*outside-args* args))
                    (catch 'up-and-out (do-guts arg arg))
                    args)
                  (do-guts orig-args args)))
            (do-loop (orig-args args)
              (catch (if colonp 'up-up-and-out 'up-and-out)
                (loop
                  (when (and (not closed-with-colon) (null args))
                    (return))
                  (when (and max-count (minusp (decf max-count)))
                    (return))
                  (setf args (bind-args orig-args args))
                  (when (and closed-with-colon (null args))
                    (return)))
                args)))
         ;;
         (if atsignp
             (setf args (do-loop orig-args args))
             (let ((arg (next-arg))
                   (*logical-block-popper* nil))
               (do-loop arg arg)))
         (nthcdr (1+ posn) directives))))))

(def-complex-format-interpreter #\} ()
  (error  "No corresponding open brace `}`."))


;;; path for jscl::stream.lisp
;;; with-output-to-string (var &optional string-form &key element-type) declaration* form*
;;; without &key implementation
(defmacro das!with-output-to-string ((var &optional string-form) &body body)
  (cond (string-form
         (let ((buf (gensym)))
           `(let ((,var (make-string-output-stream))
                  (,buf ,string-form))
              (funcall (jscl::stream-write-fn ,var) ,buf)
              ,@body
              #+nil(jscl::stream-data ,var)
              (get-output-stream-string ,var))))
        (t
         `(let ((,var (make-string-output-stream)))
            ,@body
            (get-output-stream-string ,var)))
        ))

(defun das!format (destination control-string &rest format-arguments)
  "Provides various facilities for formatting output.
  CONTROL-STRING contains a string to be output, possibly with embedded
  directives, which are flagged with the escape character \"~\".  Directives
  generally expand into additional text to be output, usually consuming one
  or more of the FORMAT-ARGUMENTS in the process.  A few useful directives
  are:
        ~A or ~nA     Prints one argument as if by PRINC
        ~S or ~nS     Prints one argument as if by PRIN1
        ~D or ~nD     Prints one argument as a decimal integer
        ~%            Does a TERPRI
        ~&            Does a FRESH-LINE

         where n is the width of the field in which the object is printed.
  
  DESTINATION controls where the result will go.  If DESTINATION is T, then
  the output is sent to the standard output stream.  If it is NIL, then the
  output is returned in a string as the value of the call.  Otherwise,
  DESTINATION must be a stream to which the output will be sent.

  Example:   (FORMAT NIL \"The answer is ~D.\" 10) => \"The answer is 10.\"

  FORMAT has many additional capabilities not described here.  Consult
  Section 22.3 (Formatted Output) of the ANSI Common Lisp standard for
  details."
  (etypecase destination
    (null
     (with-output-to-string (stream)
       (%format stream control-string format-arguments)))
    (string
     (das!with-output-to-string (stream destination)
       (%format stream control-string format-arguments)))
    ((member t)
     (%format *standard-output* control-string format-arguments)
     nil)
    (stream
     (%format destination control-string format-arguments)
     nil)))

;;; todo: %format->%format-aux ??
(defun %format (stream string orig-args &optional (args orig-args))
  (check-type string string)
  (catch 'up-and-out
    (handler-case
        (let* ((*default-format-error-control-string* string)
               (*logical-block-popper* nil))
          (interpret-directive-list stream
                                    (tokenize-control-string string)
                                    orig-args
                                    args))
      (error (c)
        (typecase c
          (format-error (%print-format-error c *standard-output*))
          (error (jscl::display-condition c))
          (t (describe c)))))))

(defun interpret-directive-list (stream directives orig-args args)
  (if directives
      (let ((directive (car directives)))
        (etypecase directive
          (string
           (write-string directive stream)
           (interpret-directive-list stream (cdr directives) orig-args args))
          (format-directive
           (multiple-value-bind (new-directives new-args)
               (let ((function (aref *format-directive-interpreters*
                                     (char-code (format-directive-character directive))))
                     (*default-format-error-offset* (1- (format-directive-end directive))))
                 (unless function (error 'format-error :complaint "Unknown format directive."
                                                       :control-string (list (format-directive-string directive))))
                 (multiple-value-bind (new-directives new-args)
                     (funcall function stream directive (cdr directives) orig-args args)
                   (values new-directives new-args)))
             (interpret-directive-list stream new-directives orig-args new-args)))))
      args))


;;; EOF
