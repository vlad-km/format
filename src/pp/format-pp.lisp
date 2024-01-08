;;; -*- mode:lisp; coding:utf-8 -*-
#|

            /\___/\
            )     (
           =\     /=                  if this code is not work, i dont know who wrote this code
             )   (                                                               2024,  @vlad-km
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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fmt-ansii)
    (error "Load  `FORMAT' firstly")))


;;;; Tab and simple pretty-printing noise.

(defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error :complaint "No more arguments."
                           :control-string ,string :offset ,offset))
     (pprint-pop)
     (pop args)))

;;; CLHS 22.3.6.1 for relative tabulations says:
;;;
;;;   ... outputs COLREL spaces and then outputs the smallest
;;;   non-negative number of additional spaces necessary to move the
;;;   cursor to a column that is a multiple of COLINC.... If the
;;;   current output column cannot be determined, however, then colinc
;;;   is ignored, and exactly colrel spaces are output.

(defun output-spaces (stream n)
  (let ((spaces (make-string 100 :initial-element #\space)))
    (loop
      (when (< n (length spaces)) (return))
      (write-string spaces stream)
      (decf n (length spaces)))
    (write-string spaces stream :end n)))

(defun format-relative-tab (stream colrel colinc)
  ;; todo: note: PP:
  (if (pretty-stream-p stream)
      (pprint-tab :line-relative colrel colinc stream)
      (flet ((advance-to-column ()
               ;; todo: note: lisp::charpos
              (let* ((cur (charpos stream))
                    (spaces (if (and cur (plusp colinc))
                              (- (* (ceiling (+ cur colrel) colinc) colinc) cur)
                              colrel)))
               (output-spaces stream spaces))))
        ;; todo: note: stream-dispatch
       (stream-dispatch stream
                               ;; simple-stream
                              (advance-to-column)
                               ;; lisp-stream
                              (advance-to-column)
                               ;; fundamental-stream
                              (let ((cur (stream-line-column stream)))
                                (cond ((and cur (plusp colinc))
                                      (stream-advance-to-column stream
                                                             (+ cur
                                                               (* (floor (+ cur colrel) colinc)
                                                                  colinc))))
                                     (t
                                      (stream-advance-to-column stream (+ cur colrel)))))))))

;; CLHS 22.3.6.1 says:
;;
;;   If the cursor is already at or beyond the column COLNUM, it will
;;   output spaces to move it to COLNUM + k*COLINC for the smallest
;;   positive integer k possible, unless COLINC is zero, in which case
;;   no spaces are output.
(defun format-absolute-tab (stream colnum colinc)
  ;; todo: note: PP:
  (if (pretty-stream-p stream)
      (pprint-tab :line colnum colinc stream)
      (flet ((advance-to-column ()
                                        ; todo: note: lisp::charpos
              (let ((cur (charpos stream)))
               (cond ((null cur)
                     (write-string "  " stream))
                     ((< cur colnum)
                     (output-spaces stream (- colnum cur)))
                     (t
                     (unless (zerop colinc)
                       (output-spaces stream
                                    (- colinc (rem (- cur colnum) colinc)))))))))
        ;; todo: note: lisp::stream-dispatch
       (stream-dispatch stream
                         ;; simple-stream. NOTE: Do we need to do soemthing better for
                         ;; simple streams?
                        (advance-to-column)
                         ;; lisp-stream
                        (advance-to-column)
                         ;; fundamental-stream
                        (let ((cur (stream-line-column stream)))
                          (cond ((null cur)
                                (write-string "  " stream))
                               ((< cur colnum)
                                (stream-advance-to-column stream colnum))
                               (t
                                (unless (zerop colinc)
                                  (let ((k (ceiling (- cur colnum) colinc)))
                                    (stream-advance-to-column stream
                                                          (+ colnum (* k colinc))))))))))))

;;; tilde T

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

(def-format-directive #\T (colonp atsignp params)
  (if colonp
      (expand-bind-defaults ((n 1) (m 1)) params
       `(pprint-tab ,(if atsignp :section-relative :section)
                   ,n ,m stream))
      (if atsignp
         (expand-bind-defaults ((colrel 1) (colinc 1)) params
           `(format-relative-tab stream ,colrel ,colinc))
         (expand-bind-defaults ((colnum 1) (colinc 1)) params
           `(format-absolute-tab stream ,colnum ,colinc)))))

;;; tilde _
(def-format-interpreter #\_ (colonp atsignp params)
  (interpret-bind-defaults () params
    (pprint-newline (if colonp
   (if atsignp
       :mandatory
       :fill)
   (if atsignp
       :miser
       :linear))
      stream)))

(def-format-directive #\_ (colonp atsignp params)
  (expand-bind-defaults () params
    `(pprint-newline ,(if colonp
     (if atsignp
         :mandatory
         :fill)
     (if atsignp
         :miser
         :linear))
       stream)))

;;; tilde #\I
(def-format-interpreter #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
          :complaint "Cannot specify the at-sign modifier."))
  (interpret-bind-defaults ((n 0)) params
                           (pprint-indent (if colonp :current :block) n stream)))

(def-format-directive #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
          :complaint  "Cannot specify the at-sign modifier."))
  (expand-bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n stream)))


;;; Justification

(defparameter *illegal-inside-justification*
  (mapcar (lambda (x) (parse-directive x 0))
         '("~W" "~:W" "~@W" "~:@W"
           "~_" "~:_" "~@_" "~:@_"
           "~:>" "~:@>"
           "~I" "~:I" "~@I" "~:@I"
           "~:T" "~:@T")))

(defun illegal-inside-justification-p (directive)
  (member directive *illegal-inside-justification*
         :test (lambda (x y)
                (and (format-directive-p x)
                     (format-directive-p y)
                     (eql (format-directive-character x) (format-directive-character y))
                     (eql (format-directive-colonp x) (format-directive-colonp y))
                     (eql (format-directive-atsignp x) (format-directive-atsignp y))))))

;;; bug: note: (collect (segments))
#+nil
(defun parse-format-justification (directives)
  (let ((first-semi nil)
       (close nil)
       (remaining directives))
    (collect ((segments))
      (loop
       (let ((close-or-semi (find-directive remaining #\> t)))
         (unless close-or-semi
           (error 'format-error :complaint "No corresponding close bracket."))
         (let ((posn (position close-or-semi remaining)))
           (segments (subseq remaining 0 posn))
           (setf remaining (nthcdr (1+ posn) remaining)))
         (when (char= (format-directive-character close-or-semi) #\>)
           (setf close close-or-semi)
           (return))
         (unless first-semi
           (setf first-semi close-or-semi))))
      (values (segments) first-semi close remaining))))

(defun parse-format-justification (directives)
  (let ((first-semi nil)
       (close nil)
       (remaining directives))
    (jscl::with-collector (segments)
      (loop
       (let ((close-or-semi (find-directive remaining #\> t)))
         (unless close-or-semi
           (error 'format-error :complaint "No corresponding close bracket."))
         (let ((posn (position close-or-semi remaining)))
           (collect-segments (subseq remaining 0 posn))
           (setf remaining (nthcdr (1+ posn) remaining)))
         (when (char= (format-directive-character close-or-semi) #\>)
           (setf close close-or-semi)
           (return))
         (unless first-semi
           (setf first-semi close-or-semi))))
      (values segments first-semi close remaining))))


;; CLHS 22.3.5.2 says fill-style conditional newlines are
;; automatically inserted after each group of blanks except for blanks
;; after a newline directive.
#+nil
(defun add-fill-style-newlines-aux (literal string offset &optional newlinep)
  (let ((end (length literal))
       (posn 0))
    (collect ((results))
      (loop
       (let ((blank (position #\space literal :start posn)))
         (when (null blank)
           (results (subseq literal posn))
           (return))
         (let ((non-blank (or (position #\space literal :start blank
                                                    :test #'char/=)
                            end)))
           (results (subseq literal posn non-blank))
           (unless newlinep
             (results (make-format-directive
                     :string string :character #\_
                     :start (+ offset non-blank) :end (+ offset non-blank)
                     :colonp t :atsignp nil :params nil)))
           (setf posn non-blank))
         (when (= posn end)
           (return))))
      (results))))

(defun add-fill-style-newlines-aux (literal string offset &optional newlinep)
  (let ((end (length literal))
       (posn 0))
    (jscl::with-collector (results)
      (loop
       (let ((blank (position #\space literal :start posn)))
         (when (null blank)
           (collect-results (subseq literal posn))
           (return))
         (let ((non-blank (or (position #\space literal :start blank
                                                    :test #'char/=)
                            end)))
           (results (subseq literal posn non-blank))
           (unless newlinep
             (collect-results (make-format-directive
                             :string string :character #\_
                             :start (+ offset non-blank) :end (+ offset non-blank)
                             :colonp t :atsignp nil :params nil)))
           (setf posn non-blank))
         (when (= posn end)
           (return))))
      results)))



(defun add-fill-style-newlines (list string offset &optional newlinep)
  (if list
      (let ((directive (car list)))
       (if (simple-string-p directive)
           (nconc (add-fill-style-newlines-aux directive string offset newlinep)
                 (add-fill-style-newlines (cdr list)
                                       string
                                       (+ offset (length directive))))
           (cons directive
                (add-fill-style-newlines (cdr list)
                                      string
                                      (format-directive-end directive)
                                      (char= (format-directive-character directive)
                                            #\Newline)))))
      nil))

(defun parse-format-logical-block
    (segments colonp first-semi close params string end)
  (when params
    (error 'format-error
          :complaint  "No parameters can be supplied with ~~<...~~:>."
          :offset (caar params)))
  (multiple-value-bind
        (prefix insides suffix)
      (multiple-value-bind (prefix-default suffix-default)
         (if colonp (values "(" ")") (values "" ""))
       (flet ((extract-string (list prefix-p)
               (let ((directive (find-if #'format-directive-p list)))
                 (if directive
                     (error 'format-error
                           :complaint "Cannot include format directives inside the ~
          ~:[suffix~;prefix~] segment of ~~<...~~:>"
                           :arguments (list prefix-p)
                           :offset (1- (format-directive-end directive)))
                     (apply #'concatenate 'string list)))))
         (case (length segments)
           (0 (values prefix-default nil suffix-default))
           (1 (values prefix-default (car segments) suffix-default))
           (2 (values (extract-string (car segments) t)
                     (cadr segments) suffix-default))
           (3 (values (extract-string (car segments) t)
                     (cadr segments)
                     (extract-string (caddr segments) nil)))
           (t
            (error 'format-error
                  :complaint "Too many segments for ~~<...~~:>.")))))
    (when (format-directive-atsignp close)
      (setf insides
           (add-fill-style-newlines insides
                                 string
                                 (if first-semi
                                    (format-directive-end first-semi)
                                    end))))
    (values prefix
           (and first-semi (format-directive-atsignp first-semi))
           insides
           suffix)))

;;; note: pprint-logical-block !!
(defun interpret-format-logical-block
    (stream orig-args args prefix per-line-p insides suffix atsignp)
  (let ((arg (if atsignp args (next-arg))))
    (if per-line-p
       (pprint-logical-block
           (stream arg :per-line-prefix prefix :suffix suffix)
         (let ((*logical-block-popper* #'(lambda () (pprint-pop))))
           (catch 'up-and-out
             (interpret-directive-list stream insides
                                   (if atsignp orig-args arg)
                                   arg))))
       (pprint-logical-block (stream arg :prefix prefix :suffix suffix)
         (let ((*logical-block-popper* #'(lambda () (pprint-pop))))
           (catch 'up-and-out
             (interpret-directive-list stream insides
                                   (if atsignp orig-args arg)
                                   arg))))))
  (if atsignp nil args))

;;; note: pprint-logical-block
(defun expand-format-logical-block (prefix per-line-p insides suffix atsignp)
  `(let ((arg ,(if atsignp 'args (expand-next-arg))))
     ,@(when atsignp
        (setf *only-simple-args* nil)
        '((setf args nil)))
     (pprint-logical-block
        (stream arg
               ,(if per-line-p :per-line-prefix :prefix) (or ,prefix "")
               :suffix (or ,suffix ""))
       (let ((args arg)
            ,@(unless atsignp
               `((orig-args arg))))
         ;; todo: note: (declare ...)
        (declare (ignorable args ,@(unless atsignp '(orig-args))))
        (block nil
          ,@(let ((*expander-next-arg-macro* 'expander-pprint-next-arg)
                 (*only-simple-args* nil)
                 (*orig-args-available* t))
              (expand-directive-list insides)))))))


(defun format-justification (stream newline-prefix extra-space line-len strings
                          pad-left pad-right mincol colinc minpad padchar)
  (setf strings (reverse strings))
  (let* ((num-gaps (+ (1- (length strings))
                    (if pad-left 1 0)
                    (if pad-right 1 0)))
        (chars (+ (* num-gaps minpad)
                 (loop
                   for string in strings
                   summing (length string))))
        (length (if (> chars mincol)
                   (+ mincol (* (ceiling (- chars mincol) colinc) colinc))
                   mincol))
        (padding (+ (- length chars) (* num-gaps minpad))))
    (when (and newline-prefix
               ;; note: todo: lisp::charpos
              (> (+ (or (charpos stream) 0)
                   length extra-space)
                line-len))
      (write-string newline-prefix stream))
    #||
    (format t "mincol   = ~A~%" mincol) ;
    (format t "minpad   = ~A~%" minpad) ;
    (format t "num-gaps = ~A~%" num-gaps) ;
    (format t "chars    = ~A~%" chars)  ;
    (format t "length   = ~A~%" length) ;
    (format t "padding  = ~A~%" padding) ;
    ||#
    (flet ((do-padding ()
            (let ((pad-len (if (zerop num-gaps) padding
                            (truncate padding num-gaps))))
              (decf padding pad-len)
              (decf num-gaps)
              (dotimes (i pad-len) (write-char padchar stream)))))
      (when (or pad-left
              (and (not pad-right) (null (cdr strings))))
       (do-padding))
      (when strings
       (write-string (car strings) stream)
       (dolist (string (cdr strings))
         (do-padding)
         (write-string string stream)))
      (when pad-right
       (do-padding)))))

;; CLHS 22.3.5.2 says fill-style conditional newlines are
;; automatically inserted after each group of blanks except for blanks
;; after a newline directive.
(defun add-fill-style-newlines (list string offset &optional newlinep)
  (if list
      (let ((directive (car list)))
 (if (simple-string-p directive)
     (nconc (add-fill-style-newlines-aux directive string offset newlinep)
     (add-fill-style-newlines (cdr list)
         string
         (+ offset (length directive))))
     (cons directive
    (add-fill-style-newlines (cdr list)
        string
        (format-directive-end directive)
        (char= (format-directive-character directive)
        #\Newline)))))
      nil))


(defun interpret-format-justification
    (stream orig-args args segments colonp atsignp first-semi params)
  (interpret-bind-defaults
   ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
   params
   (let ((newline-string nil)
        (strings nil)
        (extra-space 0)
        (line-len 0))
     (setf args
          (catch 'up-and-out
            (when (and first-semi (format-directive-colonp first-semi))
             (interpret-bind-defaults
              ((extra 0)
                 ;; note: todo: lisp::line-length
               (len (or (line-length stream) 72)))
              (format-directive-params first-semi)
              (setf newline-string
                   (with-output-to-string (stream)
                     (setf args
                          (interpret-directive-list stream
                                                 (pop segments)
                                                 orig-args
                                                 args))))
              (setf extra-space extra)
              (setf line-len len)))
            (dolist (segment segments)
             (push (with-output-to-string (stream)
                    (setf args
                          (interpret-directive-list stream segment
                                                orig-args args)))
                   strings))
            args))
     (format-justification stream newline-string extra-space line-len strings
                        colonp atsignp mincol colinc minpad padchar)))
  args)

(defun expand-format-justification (segments colonp atsignp first-semi params)
  (let ((newline-segment-p
         (and first-semi
              (format-directive-colonp first-semi))))
    (expand-bind-defaults
       ((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
       params
      `(let ((segments nil)
            ,@(when newline-segment-p
               '((newline-segment nil)
                 (extra-space 0)
                 (line-len 72))))
        (block nil
          ,@(when newline-segment-p
              `((setf newline-segment
                     (with-output-to-string (stream)
                      ,@(expand-directive-list (pop segments))))
               ,(expand-bind-defaults
                    ((extra 0)
                       ;; note: todo: lisp::line-length
                     (line-len '(or (line-length stream) 72)))
                    (format-directive-params first-semi)
                  `(setf extra-space ,extra line-len ,line-len))))
          ,@(mapcar #'(lambda (segment)
                      `(push (with-output-to-string (stream)
                              ,@(expand-directive-list segment))
                            segments))
                   segments))
        (format-justification stream
                            ,@(if newline-segment-p
                                 '(newline-segment extra-space line-len)
                                 '(nil 0 0))
                            segments ,colonp ,atsignp
                            ,mincol ,colinc ,minpad ,padchar)))))

;;; tilde #\<
(def-complex-format-interpreter #\<
  (colonp atsignp params string end directives)
  (multiple-value-bind
        (segments first-semi close remaining)
      (parse-format-justification directives)
    (setf args
         (if (format-directive-colonp close)
             (multiple-value-bind
                  (prefix per-line-p insides suffix)
                (parse-format-logical-block segments colonp first-semi
                                         close params string end)
              (interpret-format-logical-block stream orig-args args
                                          prefix per-line-p insides
                                          suffix atsignp))
             (let ((count (reduce #'+ (mapcar (lambda (x)
                                                 (count-if #'illegal-inside-justification-p x))
                                               segments))))
              (when (> count 0)
                  ;; ANSI specifies that "an error is signalled" in this
                  ;; situation.
                (error 'format-error
                      :complaint "~D illegal directives found inside justification block"
                      :arguments (list count)))
              (interpret-format-justification stream orig-args args
                                          segments colonp atsignp
                                          first-semi params))))
    remaining))

(def-complex-format-directive #\< (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
      (parse-format-justification directives)
    (values
     (if (format-directive-colonp close)
         (multiple-value-bind (prefix per-line-p insides suffix)
             (parse-format-logical-block segments colonp first-semi
                                         close params string end)
           (expand-format-logical-block prefix per-line-p insides suffix atsignp))
         (let ((count (reduce #'+
                              (mapcar (lambda (x)
                                        (count-if #'illegal-inside-justification-p x))
                                      segments))))
           (when (> count 0)
             ;; ANSI specifies that "an error is signalled" in this
             ;; situation.
             (error 'format-error
                    :complaint (list "~D illegal directive found inside justification block"
                                     count)
                    :arguments (list count)))
           (expand-format-justification segments colonp atsignp first-semi params)))
     remaining)))

(def-complex-format-interpreter #\> ()
  (error 'format-error
         :complaint "No corresponding open bracket."))

(def-complex-format-directive #\> ()
  (error 'format-error
         :complaint "No corresponding open bracket."))


;;; Damn, this code, ancient, as a mammoth shit
;;; EOF
