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
(defparameter *format-directive-expanders*
  (make-array +format-directive-limit+ :initial-element nil))
(defparameter *format-directive-interpreters*
  (make-array +format-directive-limit+ :initial-element nil))
(defparameter *default-format-error-control-string* nil)
(defparameter *default-format-error-offset* nil)


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
                 ;; todo: condition
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
                               :params (reverse params))))))

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
    (reverse result)))


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
        (destructuring-bind (var default) spec
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

(def-format-interpreter #\A
    (colonp atsignp params)
  (if params
      (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
                                (padchar #\space))
                               params
                               (format-princ stream (next-arg) colonp atsignp
                                             mincol colinc minpad padchar))
      (princ (if colonp (or (next-arg) "()") (next-arg)) stream)))

(def-format-interpreter #\S
    (colonp atsignp params)
  (cond (params
         (interpret-bind-defaults ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
          params
          (format-prin1 stream (next-arg) colonp atsignp mincol colinc minpad padchar)))
        (colonp
         (let ((arg (next-arg)))
           (if arg (prin1 arg stream) (princ "()" stream))))
        (t
         (prin1 (next-arg) stream))))

(def-format-interpreter #\C
    (colonp atsignp params)
  (interpret-bind-defaults
   ()
   params
   (if colonp
       (format-print-named-character (next-arg) stream)
       (if atsignp
           (prin1 (next-arg) stream)
           (write-char (next-arg) stream)))))

(def-format-interpreter #\W
    (colonp atsignp params)
  (interpret-bind-defaults
   ()
   params
   (let ((*print-pretty* (or colonp *print-pretty*))
	       (*print-level* (and atsignp *print-level*))
	       (*print-length* (and atsignp *print-length*)))
     (output-object (next-arg) stream))))

(def-format-interpreter #\D
    (colonp atsignp params)
  (interpret-format-integer 10))

(def-format-interpreter #\B
    (colonp atsignp params)
  (interpret-format-integer 2))

(def-format-interpreter #\O
    (colonp atsignp params)
  (interpret-format-integer 8))

(def-format-interpreter #\X
    (colonp atsignp params)
  (interpret-format-integer 16))

(def-format-interpreter #\R
    (colonp atsignp params)
  (if params
      (interpret-bind-defaults
	     ((base nil) (mincol 0) (padchar #\space) (commachar #\,)(commainterval 3))
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

(def-format-interpreter #\P
    (colonp atsignp params)
  (interpret-bind-defaults () params
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

;;; floating point
(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

(defun format-fixed (stream number w d k ovf pad atsign)
  (if (numberp number)
      (if (floatp number)
	        (format-fixed-aux stream number w d k ovf pad atsign)
          (format-write-field stream (decimal-string number) w 1 0 #\space t) )
      (format-princ stream number nil nil w 1 0 pad)))

(def-format-interpreter #\F
    (colonp atsignp params)
  (when colonp
    (error "~~F - Cannot specify the colon modifier with this directive."))
  (interpret-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space))
			                     params
                           (format-fixed stream (next-arg) w d k ovf pad atsignp)))


(def-format-interpreter #\%
    (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	         :complaint  "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
                           (dotimes (i count)
                             (terpri stream))))

(def-format-interpreter #\&
    (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	         :complaint "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
                           (when (plusp count)
                             (fresh-line stream)
                             (dotimes (i (1- count))
	                             (terpri stream)))))

(def-format-interpreter #\~
    (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	         :complaint  "Cannot specify either colon or atsign for this directive."))
  (interpret-bind-defaults ((count 1)) params
                           (dotimes (i count)
                             (write-char #\~ stream))))

(def-complex-format-interpreter #\newline
    (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	         :complaint  "Cannot specify both colon and atsign for this directive."))
  (interpret-bind-defaults () params
                           (when atsignp
                             (write-char #\newline stream)))
  (if (and (not colonp) directives (stringp (car directives)))
      (cons (string-left-trim '(#\space #\newline #\tab) (car directives)) (cdr directives))
      directives))

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


(def-format-interpreter #\*
    (colonp atsignp params)
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

(def-format-interpreter #\?
    (colonp atsignp params string end)
  (when colonp (error 'format-error :complaint "Cannot specify the colon modifier."))
  (interpret-bind-defaults
   ()
   params
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


(def-complex-format-interpreter #\[
    (colonp atsignp params directives)
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
                   ((index (next-arg))) params
                   (let* ((default (and last-semi-with-colon-p (pop sublists)))
                          (last (1- (length sublists)))
                          (sublist
			                      (if (<= 0 index last)
                                (nth (- last index) sublists)
                                default)))
                     (interpret-directive-list stream sublist orig-args args))))))
    remaining))


(def-complex-format-interpreter #\;
    ()
  (error "~~; not contained within either ~~[...~~] or ~~<...~~>."))

(def-complex-format-interpreter #\]
    ()
  (error "No corresponding open bracket `]`."))


(defvar *outside-args*)

(def-format-interpreter #\^
    (colonp atsignp params)
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

(def-complex-format-interpreter #\{
		(colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error "~~{ - No corresponding close brace."))
    (interpret-bind-defaults
     ((max-count nil)) params
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

(def-complex-format-interpreter #\}
    ()
  (error  "No corresponding open brace `}`."))



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
    #+nil (string
     (with-output-to-string (stream destination)
       (%format stream control-string format-arguments)))
    ((member t)
     (%format *standard-output* control-string format-arguments)
     nil)
    (stream
     (%format destination control-string format-arguments)
     nil)))


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
      (format-error (c)
        (describe c))
      (error (c) (describe c)))))

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
                 (unless function (error 'format-error :complaint "Unknown format directive."))
                 (multiple-value-bind (new-directives new-args)
                     (funcall function stream directive (cdr directives) orig-args args)
                   (values new-directives new-args)))
             (interpret-directive-list stream new-directives orig-args new-args)))))
      args))


;;; EOF
