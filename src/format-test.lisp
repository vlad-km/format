;;; format directives test cases


;;; local macro
(defmacro test-case (id &rest set)
  (let ((len (length `,set)))
    `(let ((test-seq-number 0)
           (test-id ',id)
           (case-length ,len)
           (complite 0)
           (errors 0)
           (swf nil))
       ,@set
       (princ (jscl::concat "Test-case " (symbol-name test-id) " " case-length "/" complite "/" errors))
       (terpri) )))

;;; local flatten
(defun unzip (ls)
  (let ((r))
    (labels ((z (x) (if (atom x) (push x r) (dolist (it x) (z it)))))
      (dolist (it ls) (z it))
      (reverse r))))

(defmacro flatten () `(progn (setf swf t) (decf case-length)))


(defun %das (control-string &rest format-arguments)
     (with-output-to-string (stream)
       (%assert-format stream control-string format-arguments)))
;;;
(defun %assert-format (stream string orig-args &optional (args orig-args))
  (let* ((*default-format-error-control-string* string)
         (*logical-block-popper* nil))
        (interpret-directive-list stream
                              (tokenize-control-string string)
                              orig-args
                              args)))


(defmacro assert-eql (pattern  &rest args)
  (let ((control-string (car args))
        (parms (apply #'append (push 'list args) nil))
        (format-args (gensym "FORMAT-ARGS"))
        (format-control (gensym "CONTROL-STRING"))
        (result (gensym "RESULT"))
        (math (gensym "MATH")))
    `(let ((,result)
           (,math)
           (,format-args  (rest ,args))
           (,format-control ,control-string)
           (out (make-string-output-stream)))
       ;;(print (list :control ,format-control :args ,format-args))
       (handler-case
           (progn
             (setq ,result (apply '%das ,format-control (if swf (unzip ,format-args) ,format-args)))
             (setq ,math (string= ,pattern ,result))
             (cond ((equal ,math t)
                    (incf complite)
                    (incf test-seq-number)
                    t)
                   (t
                    (format t  "WARNING: test ~a #~d incomplite~%   control-string ~s~%   result         ~s~%   pattern        ~s~%"
                          test-id test-seq-number ,format-control ,result ,pattern)
                    (incf test-seq-number))))
         (error (msg)
           (setq errors (1+ errors))
           ;;(print (list :msg msg :bind jscl::*handler-bindings*))
           (format t "WARNING: test ~a #~d with error: ~s~%" test-id
                 test-seq-number
                 (progn
                   (typecase msg
                     (format-error (format nil "~a ~a" (format-error-complaint msg)
                                           (format-error-control-string msg)))
                     (simple-error (format nil (simple-condition-format-control msg)
                                           (simple-condition-format-arguments msg)))
                     (type-error (type-error-datum msg))
                     (t (jscl::concat "Unhandled error condition " (class-name (class-of msg)))))
                   ))
           (incf test-seq-number))
         ))))


(defmacro assert-err (&rest args)
  (let ((control-string (car args))
        (parms (apply #'append (push 'list args) nil))
        (format-args (gensym))
        (format-control (gensym)))
    `(let ((result)
           (,format-args  ,parms)
           (,format-control ,control-string)
           (stream (make-string-output-stream)))
       (handler-case
           (progn
             (apply #'%assert-format stream (if swf (unzip ,format-args) ,format-args))
             (warn "Error test ~a #~d incomplite~%   control-string ~s~%"
                   test-id test-seq-number ,format-control)
             (incf test-seq-number)
             nil)
         (error (msg)
           ;;(print (list :assert-err-binds jscl::*handler-bindings*))
           (incf complite)
           (incf test-seq-number)
           t)))))


;;;
;;; test cases
;;;

;;;
(test-case character-char-code-10
           (assert-eql #.(char-name #\Newline) "~:C" #\Newline))

(test-case standard-character
           (let ((ss (loop for code from 32 below 127
	                         for s = (das!format nil "~@c" (code-char code))
	                         for cs = (read-from-string s)
	                         unless (eql code cs)
	                           collect (list code s cs))))
             (if (eql (length ss) 95)
                 (progn (incf complite) t)
                 nil)))
;;;
(test-case newline
           (assert-eql (with-output-to-string (stream)
                         (write-char #\Newline stream))
                       "~%")
           (assert-eql (with-output-to-string (stream)
                         (write-char #\Newline stream))
                       "~1%")
           (assert-eql ""  "~0%")
           (assert-eql (with-output-to-string (stream)
                         (write-char #\Newline stream)
                         (write-char #\Newline stream))
                       "~2%"))
;;;
(test-case other-newline
           (assert-eql "X" "X~V%" 0)
           (assert-eql "X"  "X~#%" nil)
           (assert-eql "X"  "X~#%")
           (assert-eql #.(let ((nl (string #\Newline)))
			                     (jscl::concat "X" nl nl nl))
                       "X~#%" 'a 'b 'c))
;;;
(test-case fresh-line
           (assert-eql  "" "~&")
           (assert-eql  "" "~0&")
           (assert-eql  "" "~1&")
           (assert-eql (with-output-to-string (stream)
                         (write-char #\Newline stream))
                       "~2&")
           (assert-eql  #.(jscl::concat "X" (string #\Newline)) "X~&" nil)
           (assert-eql  #.(jscl::concat "X" (string #\Newline)) "X~%~&" nil))


;;;
(test-case tilde
           (assert-eql (with-output-to-string (stream)
                         (write-char #\~ stream))
                       "~~")
           (assert-eql (with-output-to-string (stream)
                         (write-char #\~ stream))
                       "~1~")
           (assert-eql "" "~0~")
           (assert-eql (with-output-to-string (stream)
                         (write-char #\~ stream)
                         (write-char #\~ stream))
                       "~2~"))

;;;; Directve P
(test-case directive-p
           (assert-eql  ""  "~p" 1)
           (Assert-eql "s"  "~P" 2)
           (assert-eql "s"  "~p" 0)
           (assert-eql "s"  "~P" 1.0)
           (assert-eql "1 cat"      "~D cat~:P" 1)
           (assert-eql "2 cats"     "~D cat~:p" 2)
           (assert-eql "0 cats"     "~D cat~:P" 0)
           (assert-eql "No cats"    "~D cat~:p" "No")
           ;; modifier :@p
           (assert-eql "1 penny"    "~D penn~:@P" 1)
           (assert-eql "2 pennies"  "~D penn~:@p" 2)
           (assert-eql "0 pennies"  "~D penn~@:P" 0)
           (assert-eql "No pennies" "~D penn~@:p" "No")
           ;; modifier @p
           (assert-eql "y"       "~@p" 1) 
           (assert-eql "ies"     "~@P" 2)
           (assert-eql "ies"     "~@p" 0)
           (assert-eql "ies"     "~@P" 1.0))



;;; Directive R
(test-case Radix-English-cardinal-numbers
           (assert-eql "zero"  "~r" 0)
           (assert-eql "one"   "~r" 1)
           (assert-eql "two"   "~r" 2)
           (assert-eql "three" "~r" 3)
           (assert-eql "four"  "~r" 4)
           (assert-eql "five"  "~r" 5)
           (assert-eql "six"   "~r" 6)
           (assert-eql "seven" "~r" 7)
           (assert-eql "eight" "~r" 8)
           (assert-eql "nine"  "~r" 9)
           (assert-eql "ten"   "~r"    10)
           (assert-eql "eleven""~r"    11)
           (assert-eql "twelve""~r"    12)
           (assert-eql "thirteen" "~r" 13)
           (assert-eql "fourteen" "~r" 14)
           (assert-eql "fifteen"  "~r" 15)
           (assert-eql "sixteen"  "~r" 16)
           (assert-eql "seventeen""~r" 17)
           (assert-eql "eighteen" "~r" 18)
           (assert-eql "nineteen" "~r" 19)
           (assert-eql "twenty"        "~r" 20)
           (assert-eql "twenty-one"    "~r" 21)
           (assert-eql "thirty"        "~r" 30)
           (assert-eql "fourty"        "~r" 40)
           (assert-eql "fifty"         "~r" 50)
           (assert-eql "sixty"         "~r" 60)
           (assert-eql "seventy"       "~r" 70)
           (assert-eql "eighty"        "~r" 80)
           (assert-eql "ninety"        "~r" 90)
           (assert-eql "one hundred"               "~r" 100)
           (assert-eql "two hundred four"          "~r" 204)
           (assert-eql "three hundred sixteen"     "~r" 316)
           (assert-eql "four hundred thirty-six"   "~r" 436)
           (assert-eql "two thousand"              "~r" 2000)
           (assert-eql "three thousand five"       "~r" 3005)
           (assert-eql "four thousand twelve"      "~r" 4012)
           (assert-eql "five thousand two hundred" "~r" 5200)
           (assert-eql "eighty thousand"           "~r" 80000)
           (assert-eql "five hundred thousand"     "~r" 500000)
           (assert-eql "two million"               "~r" 2000000)
           (assert-eql "three million six"         "~r" 3000006)
           (assert-eql "four million two thousand" "~r" 4002000))

;;;
(test-case Radix-English-Numbers
           (assert-eql "zeroth"   "~:r" 0)
           (assert-eql "first"    "~:r" 1)
           (assert-eql "second"   "~:r" 2)
           (assert-eql "third"    "~:r" 3)
           (assert-eql "fourth"   "~:r" 4)
           (assert-eql "fifth"    "~:r" 5)
           (assert-eql "sixth"    "~:r" 6)
           (assert-eql "seventh"  "~:r" 7)
           (assert-eql "eighth"   "~:r" 8)
           (assert-eql "ninth"    "~:r" 9)
           (assert-eql "tenth"            "~:r" 10)
           (assert-eql "eleventh"         "~:r" 11)
           (assert-eql "twelvth"          "~:r" 12)
           (assert-eql "thirteenth"       "~:r" 13)
           (assert-eql "fourteenth"       "~:r" 14)
           (assert-eql "fifteenth"        "~:r" 15)
           (assert-eql "sixteenth"        "~:r" 16)
           (assert-eql "seventeenth"      "~:r" 17)
           (assert-eql "eighteenth"       "~:r" 18)
           (assert-eql "nineteenth"       "~:r" 19)
           (assert-eql "twentieth"        "~:r" 20)
           (assert-eql "twenty-first"     "~:r" 21)
           (assert-eql "thirtieth"        "~:r" 30)
           (assert-eql "fourtieth"        "~:r" 40)
           (assert-eql "fiftieth"         "~:r" 50)
           (assert-eql "sixtieth"         "~:r" 60)
           (assert-eql "seventieth"       "~:r" 70)
           (assert-eql "eightieth"        "~:r" 80)
           (assert-eql "ninetieth"        "~:r" 90)
           (assert-eql "one hundredth"               "~:r" 100)
           (assert-eql "two hundred fourth"          "~:r" 204)
           (assert-eql "three hundred sixteenth"     "~:r" 316)
           (assert-eql "four hundred thirty-sixth"   "~:r" 436)
           (assert-eql "two thousandth"              "~:r" 2000)
           (assert-eql "three thousand fifth"        "~:r" 3005)
           (assert-eql "four thousand twelvth"       "~:r" 4012)
           (assert-eql "five thousand two hundredth" "~:r" 5200)
           (assert-eql "eighty thousandth"           "~:r" 80000)
           (assert-eql "five hundred thousandth"     "~:r" 500000)
           (assert-eql "two millionth"               "~:r" 2000000)
           (assert-eql "three million sixth"         "~:r" 3000006)
           (assert-eql "four million two thousandth" "~:r" 4002000))

;;;
(test-case Radix-Roman-numbers
           (assert-eql "I"       "~@r" 1)
           (assert-eql "II"      "~@r" 2)
           (assert-eql "III"     "~@r" 3)
           (assert-eql "IV"      "~@r" 4)
           (assert-eql "V"       "~@r" 5)
           (assert-eql "VI"      "~@r" 6)
           (assert-eql "VII"     "~@r" 7)
           (assert-eql "VIII"    "~@r" 8)
           (assert-eql "IX"      "~@r" 9)
           (assert-eql "X"       "~@r" 10)
           (assert-eql "XI"      "~@r" 11)
           (assert-eql "XII"     "~@r" 12)
           (assert-eql "XIII"    "~@r" 13)
           (assert-eql "XIV"     "~@r" 14)
           (assert-eql "XV"      "~@r" 15)
           (assert-eql "XVI"     "~@r" 16)
           (assert-eql "XVII"    "~@r" 17)
           (assert-eql "XVIII"   "~@r" 18)
           (assert-eql "XIX"     "~@r" 19)
           (assert-eql "XX"      "~@r" 20)
           (assert-eql "XXX"     "~@r" 30)
           (assert-eql "XL"      "~@r" 40)
           (assert-eql "L"       "~@r" 50)
           (assert-eql "LXIV"    "~@r" 64)
           (assert-eql "XCIX"    "~@r" 99)
           (assert-eql "C"       "~@r" 100)
           (assert-eql "CXLVII"  "~@r" 147)
           (assert-eql "CDLXXXIX" "~@r" 489)
           (assert-eql "DCCCXXXI" "~@r" 831)
           (assert-eql "M"        "~@r" 1000)
           (assert-eql "MMXL"     "~@r" 2040)
           (assert-eql "MMMXC"    "~@r" 3090))

;;;
(test-case Radix-Roman-numerals
           (assert-eql "I"            "~:@r" 1)
           (assert-eql "II"           "~:@r" 2)
           (assert-eql "III"          "~:@r" 3)
           (assert-eql "IIII"         "~:@r" 4)
           (assert-eql "V"            "~:@r" 5)
           (assert-eql "VI"           "~:@r" 6)
           (assert-eql "VII"          "~:@r" 7)
           (assert-eql "VIII"         "~:@r" 8)
           (assert-eql "VIIII"        "~:@r" 9)
           (assert-eql "X"            "~:@r" 10)
           (assert-eql "XI"           "~:@r" 11)
           (assert-eql "XII"          "~:@r" 12)
           (assert-eql "XIII"         "~:@r" 13)
           (assert-eql "XIIII"        "~:@r" 14)
           (assert-eql "XV"           "~:@r" 15)
           (assert-eql "XVI"          "~:@r" 16)
           (assert-eql "XVII"         "~:@r" 17)
           (assert-eql "XVIII"        "~:@r" 18)
           (assert-eql "XVIIII"       "~:@r" 19)
           (assert-eql "XX"           "~:@r" 20)
           (assert-eql "XXX"          "~:@r" 30)
           (assert-eql "XXXX"         "~:@r" 40)
           (assert-eql "L"            "~:@r" 50)
           (assert-eql "LXIIII"       "~:@r" 64)
           (assert-eql "LXXXXVIIII"   "~:@r" 99)
           (assert-eql "C"            "~:@r" 100)
           (assert-eql "CXXXXVII"     "~:@r" 147)
           (assert-eql "CCCCLXXXVIIII" "~:@r" 489)
           (assert-eql "DCCCXXXI"      "~:@r" 831)
           (assert-eql "M"             "~:@r" 1000)
           (assert-eql "MMXXXX"        "~:@r" 2040)
           (assert-eql "MMMLXXXX"      "~:@r" 3090))

;;;  
(test-case Radix-numbers-mincol
           (assert-eql "123"       "~10,1r" 123)
           (assert-eql "123"       "~10,2r" 123)
           (assert-eql "123"       "~10,3r" 123)
           (assert-eql " 123"      "~10,4r" 123)
           (assert-eql "  123"     "~10,5r" 123))

;;;
(test-case radix-number-padchar-colon-comma  
           (assert-eql "xx123"         "~10,5,'xr" 123)
           (assert-eql "xx123"         "~10,5,'x:r" 123)
           (assert-eql "xx1,234"       "~10,7,'x:r" 1234)
           (assert-eql "xx551,234"     "~10,9,'x:r" 551234)
           (assert-eql "xx66,551,234"  "~10,12,'x:r" 66551234)
           (assert-eql "xx66a551a234"  "~10,12,'x,'a:r" 66551234))

;;;
(test-case radix-with-modifiers
           (assert-eql " 12345"             "~10,#r" 12345 nil nil nil nil nil)
           (assert-eql  "///123456789"      "~10,12,vr" #\/ 123456789)
           (assert-eql  "123/456/789"       "~10,,,v:r" #\/ 123456789)
           (assert-eql  "123,456,789"       "~10,,,v:r" nil 123456789)
           (assert-eql  "12,345,670"        "~8,,,,v:R" nil #o12345670)
           (assert-eql "12,34,56,70"        "~8,,,,v:R" 2 #o12345670)
           (assert-eql "1234,5670"          "~16,,,,#:r" #x12345670 nil nil nil)
           (assert-eql "1,2,3,4,5,6,7,0"    "~16,,,,1:r" #x12345670))


;;; Directive D
(test-case decimal
           (assert-eql "123"              "~1d" 123)
           (assert-eql "123"              "~2d" 123)
           (assert-eql "123"              "~3d" 123)
           (assert-eql " 123"             "~4d" 123)
           (assert-eql "  123"            "~5d" 123)
           (assert-eql "xx123"            "~5,'xd" 123)
           (assert-eql "xx123"            "~5,'x:d" 123)
           (assert-eql "xx1,234"          "~7,'x:d" 1234)
           (assert-eql "xx551,234"        "~9,'x:d" 551234)
           (assert-eql "xx66,551,234"     "~12,'x:d" 66551234)
           (assert-eql "xx66a551a234"     "~12,'x,'a:d" 66551234))
;;;
(test-case decimal-1
           (flatten)
           (assert-eql "+12,34,56,78,90"            "~,,,#@:D" 1234567890 (make-list 0))
           (assert-eql "+12,34,56,78,90"            "~,,,#@:D" 1234567890 (make-list 1))
           (assert-eql "+1,234,567,890"             "~,,,#@:D" 1234567890 (make-list 2))
           (assert-eql "+12,3456,7890"              "~,,,#@:D" 1234567890 (make-list 3))
           (assert-eql "+12345,67890"               "~,,,#@:D" 1234567890 (make-list 4))
           (assert-eql "+1234,567890"               "~,,,#@:D" 1234567890 (make-list 5))
           (assert-eql "+123,4567890"               "~,,,#@:D" 1234567890 (make-list 6))
           (assert-eql "+12,34567890"               "~,,,#@:D" 1234567890 (make-list 7))
           (assert-eql "+1,234567890"               "~,,,#@:D" 1234567890 (make-list 8))
           (assert-eql "+1234567890"                "~,,,#@:D" 1234567890 (make-list 9))
           (assert-eql "+1234567890"                "~,,,#@:D" 1234567890 (make-list 10)))
;;;
(test-case decimal-2
           (flatten)
           (assert-eql "12,34,56,78,90"      "~,,,#:D" 1234567890 (make-list 0))
           (assert-eql "12,34,56,78,90"      "~,,,#:D" 1234567890 (make-list 1))
           (assert-eql "1,234,567,890"       "~,,,#:D" 1234567890 (make-list 2))
           (assert-eql "12,3456,7890"        "~,,,#:D" 1234567890 (make-list 3))
           (assert-eql "12345,67890"         "~,,,#:D" 1234567890 (make-list 4))
           (assert-eql "1234,567890"         "~,,,#:D" 1234567890 (make-list 5))
           (assert-eql "123,4567890"         "~,,,#:D" 1234567890 (make-list 6))
           (assert-eql "12,34567890"         "~,,,#:D" 1234567890 (make-list 7))
           (assert-eql "1,234567890"         "~,,,#:D" 1234567890 (make-list 8))
           (assert-eql "1234567890"          "~,,,#:D" 1234567890 (make-list 9))
           (assert-eql "1234567890"          "~,,,#:D" 1234567890 (make-list 10)))
;;;
(test-case decimal-3
           (flatten)
           (assert-eql "12345"            "~#d" 12345 (make-list 0))
           (assert-eql "12345"            "~#d" 12345 (make-list 1))
           (assert-eql "12345"            "~#d" 12345 (make-list 2))
           (assert-eql "12345"            "~#d" 12345 (make-list 3))
           (assert-eql "12345"            "~#d" 12345 (make-list 4))
           (assert-eql " 12345"           "~#d" 12345 (make-list 5))
           (assert-eql "  12345"          "~#d" 12345 (make-list 6))
           (assert-eql "   12345"         "~#d" 12345 (make-list 7))
           (assert-eql "    12345"        "~#d" 12345 (make-list 8))
           (assert-eql "     12345"       "~#d" 12345 (make-list 9))
           (assert-eql "      12345"      "~#d" 12345 (make-list 10)))
;;;
(test-case decimal-4
           (assert-eql "100"          "~vD" nil 100)
           (assert-eql "   100"       "~6,vD" nil 100)
           (assert-eql  "12,345"      "~,,v:d" nil 12345)
           (assert-eql "12*345"       "~,,'*,v:d" nil 12345 "12*345"))

;;; Directive F
(test-case float
           (assert-eql "0.123457"        "~f"           0.1234567890)
           (assert-eql "0.1235"          "~,4f"         0.1234567890)
           (assert-eql "0.12346"         "~,5f"         0.1234567890)
           (assert-eql "     0.12346"    "~12,5f"       0.1234567890)
           (assert-eql "        0.12"    "~12,2f"       0.1234567890))

;;; Directive O
(test-case octal
           (assert-eql "123"               "~1o"         #o123)
           (assert-eql "123"               "~2o"         #o123)
           (assert-eql "123"               "~3o"         #o123)
           (assert-eql " 123"              "~4o"         #o123)                           
           (assert-eql "  123"             "~5o"         #o123)
           (assert-eql "xx123"             "~5,'xo"      #o123)
           (assert-eql "xx123"             "~5,'x:o"     #o123)
           (assert-eql "xx1,234"           "~7,'x:o"     #o1234)
           (assert-eql "xx551,234"         "~9,'x:o"     #o551234)
           (assert-eql "xx66,551,234"      "~12,'x:o"    #o66551234)
           (assert-eql "xx66a551a234"      "~12,'x,'a:o" #o66551234))

;;;
(test-case octal-V-with-nil
           (assert-eql "100"                 "~vO" nil #o100)
           (assert-eql "   100"              "~6,vO" nil #o100)
           (assert-eql "12,345"              "~,,v:o" nil #o12345)
           (assert-eql "12*345"              "~,,'*,v:o" nil #o12345))

;;;
(test-case  octal-modifiers
            (assert-err    "~+10o"                         #o1234)
            (assert-err    "~+10@O"                        #o1234)
            (assert-err    "~-1O"                          #o1234)
            (assert-err    "~-1000000000000000000o"        #o1234)
            (assert-eql "      1234"      "~10o"                         #o1234)
            (assert-eql "     +1234"      "~10@O"                        #o1234)
            (assert-eql "1234"            "~1O"                          #o1234)
            (assert-eql "1234"            "~vo" (1- most-negative-fixnum) #o1234))

;;; Directive B
(test-case binary-mincol
           (assert-eql "101"               "~1b" #b101)
           (assert-eql "101"               "~2b" #b101)
           (assert-eql "101"               "~3b" #b101)
           (assert-eql " 101"              "~4b" #b101)
           (assert-eql "  101"             "~5b" #b101))

;;;
(test-case binary-padchar
           (assert-eql "xx101"             "~5,'xb" #b101))

;;;
(test-case binary-modifier
           (assert-eql "xx101"             "~5,'x:b" #b101)
           (assert-eql "xx1,011"           "~7,'x:b" #b1011)
           (assert-eql "xx111,011"         "~9,'x:b" #b111011)
           (assert-eql "xx10,111,011"      "~12,'x:b" #b10111011)
           (assert-eql "xx10a111a011"      "~12,'x,'a:b" #b10111011))


;;; X Directive
(test-case x-directive-integrated-text
           (flatten)
           (assert-eql   "1B3FE"           "~#X" #x1b3fe (make-list 0))
           (assert-eql   "1B3FE"           "~#X" #x1b3fe (make-list 1))
           (assert-eql   "1B3FE"           "~#X" #x1b3fe (make-list 2))
           (assert-eql   "1B3FE"           "~#X" #x1b3fe (make-list 3))
           (assert-eql   "1B3FE"          "~#X" #x1b3fe (make-list 4))
           (assert-eql   " 1B3FE"         "~#X" #x1b3fe (make-list 5))
           (assert-eql   "  1B3FE"        "~#X" #x1b3fe (make-list 6))
           (assert-eql   "   1B3FE"       "~#X" #x1b3fe (make-list 7))
           (assert-eql   "    1B3FE"      "~#X" #x1b3fe (make-list 8))
           (assert-eql   "     1B3FE"     "~#X" #x1b3fe (make-list 9))
           ;;
           (assert-eql   "12,34,56,78,90"      "~,,,#:x" #x1234567890 (make-list 0))
           (assert-eql   "12,34,56,78,90"      "~,,,#:x" #x1234567890 (make-list 1))
           (assert-eql   "1,234,567,890"       "~,,,#:x" #x1234567890 (make-list 2))
           (assert-eql   "12,3456,7890"        "~,,,#:x" #x1234567890 (make-list 3))
           (assert-eql   "12345,67890"         "~,,,#:x" #x1234567890 (make-list 4))
           (assert-eql   "1234,567890"         "~,,,#:x" #x1234567890 (make-list 5))
           (assert-eql   "123,4567890"         "~,,,#:x" #x1234567890 (make-list 6))
           (assert-eql   "12,34567890"         "~,,,#:x" #x1234567890 (make-list 7))
           (assert-eql   "1,234567890"         "~,,,#:x" #x1234567890 (make-list 8))
           (assert-eql   "1234567890"          "~,,,#:x" #x1234567890 (make-list 9))
           (assert-eql   "1234567890"          "~,,,#:x" #x1234567890 (make-list 10))
           ;;
           (assert-eql   "+12,34,56,78,90"      "~,,,#@:X" #x1234567890 (make-list 0))
           (assert-eql   "+12,34,56,78,90"      "~,,,#@:X" #x1234567890 (make-list 1))
           (assert-eql   "+1,234,567,890"       "~,,,#@:X" #x1234567890 (make-list 2))
           (assert-eql   "+12,3456,7890"        "~,,,#@:X" #x1234567890 (make-list 3))
           (assert-eql   "+12345,67890"         "~,,,#@:X" #x1234567890 (make-list 4))
           (assert-eql   "+1234,567890"         "~,,,#@:X" #x1234567890 (make-list 5))
           (assert-eql   "+123,4567890"         "~,,,#@:X" #x1234567890 (make-list 6))
           (assert-eql   "+12,34567890"         "~,,,#@:X" #x1234567890 (make-list 7))
           (assert-eql   "+1,234567890"         "~,,,#@:X" #x1234567890 (make-list 8))
           (assert-eql   "+1234567890"          "~,,,#@:X" #x1234567890 (make-list 9))
           (assert-eql   "+1234567890"          "~,,,#@:X" #x1234567890 (make-list 10))
           ;;
           (assert-err   "~+10x"  #x1234)
           (assert-err   "~+10@X" #x1234)
           (assert-err   "~-1X"   #x1234)
           (assert-err   "~-1x"   #x1234)
           (assert-eql   "      1234"   "~10x"   #x1234)
           (assert-eql   "     +1234"   "~10@X"  #x1234)
           (assert-eql   "1234"         "~1X"    #x1234)
           (assert-eql   "1234"         "~1x"    #x1234))

;;; Directive A
(test-case aesthetic-eql-princ
           (assert-eql (princ-to-string 123)         "~a" 123)
           (assert-eql (princ-to-string 14.66)       "~a" 14.66)
           (assert-eql (princ-to-string 'symbol)     "~a" 'symbol)
           (assert-eql (princ-to-string "string")    "~a" "string")
           (assert-eql (princ-to-string #\space)     "~a" #\space)
           (assert-eql (princ-to-string nil)         "~a" nil))

;;;
(test-case  aesthetic-modifier-nil
            (assert-eql "()"                "~:a"   nil)
            (assert-eql "hello  "           "~7a"   "hello")
            (assert-eql "  hello"           "~7@a"  "hello"))

;;;
(test-case aesthetic-minpad-colinc-padchar
           (assert-eql "hello                     "     "~21,7a"      "hello")
           (assert-eql "hello   "                       "~,,3a"       "hello")
           (assert-eql "helloxxx"                       "~0,1,3,'xa"  "hello"))

;;;
(test-case aesthetic-params
           (assert-eql "ABC  "     "~3,,+2A"   "ABC")
           (assert-eql "ABC"       "~3,,0A"    "ABC")
           (assert-eql "ABC"       "~3,,-1A"   "ABC")
           (assert-eql "ABCD"      "~3,,0A"    "ABCD")
           (assert-eql "ABCD"      "~3,,-1A"   "ABCD"))

(test-case aesthetic-params-1
           (assert-eql "ABC"                   "~3,,vA" 0 "ABC")
           (assert-eql "ABC "                  "~3,,vA" 1 "ABC")
           (assert-eql "ABC  "                 "~3,,vA" 2 "ABC")
           (assert-eql "ABC   "                "~3,,vA" 3 "ABC")
           (assert-eql "ABC    "               "~3,,vA" 4 "ABC")
           (assert-eql "ABC     "              "~3,,vA" 5 "ABC")
           ;;
           (assert-eql "ABC"                   "~3,,v@A" 0 "ABC")
           (assert-eql " ABC"                  "~3,,v@A" 1 "ABC")
           (assert-eql "  ABC"                 "~3,,v@A" 2 "ABC")
           (assert-eql "   ABC"                "~3,,v@A" 3 "ABC")
           (assert-eql "    ABC"               "~3,,v@A" 4 "ABC")
           (assert-eql "     ABC"              "~3,,v@A" 5 "ABC"))

;;;
(test-case aesthetic-params-2
           (assert-eql   "abc "        "~#A"      "abc" nil nil nil)
           (assert-eql   "   abc"      "~#@a"     "abc" nil nil nil nil nil)
           (assert-eql  "abc    "      "~5,#a"    "abc" nil nil nil)
           (assert-eql  "    abc"      "~5,#@A"   "abc" nil nil nil)
           (assert-eql  "abc   "       "~4,#A"    "abc" nil nil)
           (assert-eql  "   abc"       "~4,#@A"   "abc" nil nil)
           (assert-eql  "abc    "      "~#,#A"    "abc" nil nil nil)
           (assert-eql "    abc"       "~#,#@A"   "abc" nil nil nil)
           (assert-err    "~-100A"   "abc")
           (assert-err    "~-100000000000000000000a" "abc"))

;;;
(test-case standard-directive
           (assert-eql "#(NIL)" "~:s" #(nil))
           (assert-eql (prin1-to-string 123)                    "~s" 123)
           (assert-eql (prin1-to-string -123)                   "~s" -123)
           (assert-eql (prin1-to-string 1.97)                   "~s" 1.97)
           (assert-eql (prin1-to-string 'abc)                   "~s" 'abc)
           (assert-eql (prin1-to-string "abc")                  "~s" "abc")
           (assert-eql (prin1-to-string #\z)                    "~s" #\z)
           (assert-eql (prin1-to-string #\space)                "~s" #\space)
           (assert-eql (prin1-to-string nil)                    "~s" nil)
           (assert-eql "()"                                     "~:s" nil)
           (assert-eql "12345  "                                "~7s" 12345)
           (assert-eql "  12345"                                "~7@s" 12345)
           (assert-eql "12345                     "             "~21,7s" 12345)
           (assert-eql "12345   "                               "~,,3s" 12345)
           (assert-eql "12345xxx"                               "~0,1,3,'xs" 12345))

(test-case standard-1
           (assert-eql "()"             "~1@:S" nil)
           (assert-eql "()"             "~2@:S" nil)
           (assert-eql " ()"            "~3@:S" nil)
           (assert-eql "  ()"           "~4@:S" nil)
           (assert-eql "   ()"          "~5@:S" nil)
           (assert-eql "    ()"         "~6@:S" nil)
           (assert-eql "     ()"        "~7@:S" nil)
           (assert-eql "      ()"       "~8@:S" nil)
           (assert-eql "       ()"      "~9@:S" nil)
           (assert-eql "        ()"     "~10@:S" nil))

(test-case standard-2
           (assert-eql "()"             "~1:S" nil)
           (assert-eql "()"             "~2:S" nil)
           (assert-eql "() "            "~3:S" nil)
           (assert-eql "()  "           "~4:S" nil)
           (assert-eql "()   "          "~5:S" nil)
           (assert-eql "()    "         "~6:S" nil)
           (assert-eql "()     "        "~7:S" nil)
           (assert-eql "()      "       "~8:S" nil)
           (assert-eql "()       "      "~9:S" nil)
           (assert-eql "()        "     "~10:S" nil))

(test-case standard-3
           (assert-eql "()"             "~v:s" 1 nil)
           (assert-eql "()"             "~v:s" 2 nil)
           (assert-eql "() "            "~v:s" 3 nil)
           (assert-eql "()  "           "~v:s" 4 nil)
           (assert-eql "()   "          "~v:s" 5 nil)
           (assert-eql "()    "         "~v:s" 6 nil)
           (assert-eql "()     "        "~v:s" 7 nil)
           (assert-eql "()      "       "~v:s" 8 nil)
           (assert-eql "()       "      "~v:s" 9 nil)
           (assert-eql "()        "     "~v:s" 10 nil))

(test-case standard-4
           (assert-eql "()"             "~v:@s" 1 nil)
           (assert-eql "()"             "~v:@s" 2 nil)
           (assert-eql " ()"            "~v:@s" 3 nil)
           (assert-eql "  ()"           "~v:@s" 4 nil)
           (assert-eql "   ()"          "~v:@s" 5 nil)
           (assert-eql "    ()"         "~v:@s" 6 nil)
           (assert-eql "     ()"        "~v:@s" 7 nil)
           (assert-eql "      ()"       "~v:@s" 8 nil)
           (assert-eql "       ()"      "~v:@s" 9 nil)
           (assert-eql "        ()"     "~v:@s" 10 nil))


(test-case standard-5-colin
           (assert-eql  "NIL"          "~3,1s" nil)
           (assert-eql  "NIL   "       "~4,3s" nil)
           (assert-eql  "NIL"          "~3,3@s" nil)
           (assert-eql  "    NIL"      "~4,4@s" nil)
           (assert-eql  "   NIL"       "~5,3@s" nil)
           (assert-eql  "NIL   "       "~5,3S" nil)
           (assert-eql  "      NIL"    "~7,3@s" nil)
           (assert-eql  "NIL      "    "~7,3S" nil))

(test-case standard-6-minpad
           (assert-eql "ABC  "          "~v,,2S" -1 'ABC)
           (assert-eql "ABC  "          "~v,,2S" 0 'ABC)
           (assert-eql "ABC  "          "~v,,2S" 1 'ABC)
           (assert-eql "ABC  "          "~v,,2S" 2 'ABC)
           (assert-eql "ABC  "          "~v,,2S" 3 'ABC)
           (assert-eql "ABC  "          "~v,,2S" 4 'ABC)
           (assert-eql "ABC  "          "~v,,2S" 5 'ABC)
           (assert-eql "ABC   "         "~v,,2S" 6 'ABC)
           (assert-eql "ABC    "        "~v,,2S" 7 'ABC)
           (assert-eql "ABC     "       "~v,,2S" 8 'ABC)
           (assert-eql "ABC      "      "~v,,2S" 9 'ABC))


(test-case standard-7-padchar
           (assert-eql  "ABXX"          "~4,,,'XS"   'AB)
           (assert-eql  "AB  "          "~4,,,s"     'AB)
           (assert-eql  "XXAB"          "~4,,,'X@s"  'AB)
           (assert-eql  "  AB"          "~4,,,@S"    'AB)
           (assert-eql  "ABCDE     "    "~10,,,vS"   nil 'ABCDE)
           (assert-eql  "     ABCDE"    "~10,,,v@S"  nil 'ABCDE)
           (assert-eql  "ABCDE*****"    "~10,,,vs"   #\* 'ABCDE)
           (assert-eql  "*****ABCDE"    "~10,,,v@s"  #\* 'ABCDE))

(test-case standard-8-others
           (assert-eql "ABC"            "~3,,vS" 0 'ABC)
           (assert-eql "ABC "           "~3,,vS" 1 'ABC)
           (assert-eql "ABC  "          "~3,,vS" 2 'ABC)
           (assert-eql "ABC   "         "~3,,vS" 3 'ABC)
           (assert-eql "ABC    "        "~3,,vS" 4 'ABC)
           (assert-eql "ABC     "       "~3,,vS" 5 'ABC)
           (assert-eql "ABC      "      "~3,,vS" 6 'ABC))


(test-case standard-9-others
           (assert-eql "ABC"            "~3,,v@S" 0 'ABC)
           (assert-eql " ABC"           "~3,,v@S" 1 'ABC)
           (assert-eql "  ABC"          "~3,,v@S" 2 'ABC)
           (assert-eql "   ABC"         "~3,,v@S" 3 'ABC)
           (assert-eql "    ABC"        "~3,,v@S" 4 'ABC)
           (assert-eql "     ABC"       "~3,,v@S" 5 'ABC)
           (assert-eql "      ABC"      "~3,,v@S" 6 'ABC))


;;;
(test-case any-from-http-cs.smu.edu-1
           (assert-eql  "The answer is 5."             "The answer is ~D." 5) 
           (assert-eql  "The answer is   5."           "The answer is ~3D." 5)
           (assert-eql  "The answer is 005."           "The answer is ~3,'0D." 5)
           (assert-eql  "The answer is 229,345,007."   "The answer is ~:D." (expt 47 5))
           (assert-eql  "3 items found."               "~D item~:P found." 3)
           (assert-eql  "1111101011001110"          "~,,' ,4B" #xFACE)
           (assert-eql  "111001110"                 "~,,' ,4B" #x1CE)
           (assert-eql "   1111101011001110"        "~19,,' ,4B" #xFACE)
           (assert-eql "          111001110"        "~19,,' ,4B" #x1CE))
;;;
(test-case plural-from-cs.smu.edu-1
           (assert-eql  "7 tries/1 win" "~D tr~:@P/~D win~:P" 7 1)
           (assert-eql  "1 try/0 wins" "~D tr~:@P/~D win~:P" 1 0)
           (assert-eql  "1 try/3 wins" "~D tr~:@P/~D win~:P" 1 3))

;;; Errors
(test-case Errors
           (assert-err  "~::d" 0)
           (assert-err  "~@@d" 0)
           (assert-err  "~@:@d" 0)
           (assert-err  "~:@:d" 0)
           (assert-err  "~2c" #\a)
           (assert-err  "~'2c" #\a)
           (assert-err  "~#c" #\a)
           (assert-err  "~vc" #\a)
           (assert-err  "~'a%")
           (assert-err  "~1,2%")
           (assert-err "~:%")
           (assert-err "~@%")
           (assert-err  "~'a&")
           (assert-err  "~1,2&")
           (assert-err  "~:&")
           (assert-err  "~@&")
           (assert-err  "~'a~")
           (assert-err "~1,2~")
           (assert-err "~:~")
           (assert-err "~@~")
           (assert-err "~")
           (assert-err "~~~"))

(test-case write
  ;; Test that objects are renendered as with write.
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
        do  (assert-eql (with-output-to-string (stream)
                            (write obj :stream stream))
                          "~s" obj))
  ;; test that it can handle circular lists
  (let ((*print-circle* t))
     (assert-eql "#1=(1 . #1#)"
                    "~w"
                           (let ((l (list 1)))
                             (setf (cdr l) l))))
  ;; test that this directive reports an error
  ;; if a parameter is given
  (assert-err "~1w" 234)
  (assert-err "~'aw" 234))


(test-case go-to
   (assert-eql "ac"  "~c~*~c" #\a #\b #\c #\d)
   (assert-eql "ab"  "~c~0*~c" #\a #\b #\c #\d)
   (assert-eql "ac"  "~c~1*~c" #\a #\b #\c #\d)
   (assert-eql "ad"  "~c~2*~c" #\a #\b #\c #\d)
   (assert-eql "aa"  "~c~:*~c" #\a #\b #\c #\d)
   (assert-eql "ab"  "~c~0:*~c" #\a #\b #\c #\d)
   (assert-eql "aa"  "~c~1:*~c" #\a #\b #\c #\d)
   (assert-eql "aba"  "~c~c~2:*~c" #\a #\b #\c #\d)
   (assert-eql "aba"  "~c~c~@*~c" #\a #\b #\c #\d)
   (assert-eql "abb"  "~c~c~1@*~c" #\a #\b #\c #\d)
   (assert-eql "abd"  "~c~c~3@*~c" #\a #\b #\c #\d)
  ;; Test that going beyond the first or last argument
  ;; gives an error.
  (assert-err "~c~c~*" #\a #\b)
  (assert-err "~c~c~2*~:2*~c" #\a #\b #\c)
  (assert-err "~c~:2*~2*~c" #\a #\b #\c)
  (assert-err "~c~-1@*~0@*~c" #\a #\b #\c)
  (assert-err "~c~4@*~0@*~c" #\a #\b #\c))

(test-case
 conditional
 (assert-eql "abc"   "~[xyz~;abc~;def~]" 1)
 (assert-eql "xyz"   "~[xyz~;abc~;def~]" 0)
 (assert-eql ""      "~[xyz~;abc~;def~]" 3)
 ;; test the default clause
 (assert-eql "abc"   "~[xyz~;abc~:;def~]" 1)
 (assert-eql "xyz"   "~[xyz~;abc~:;def~]" 0)
 (assert-eql "def"   "~[xyz~;abc~:;def~]" 3)
 (assert-eql "abc"   "~:[xyz~;abc~]" nil)
 (assert-eql "xyz"   "~:[xyz~;abc~]" 24)
 (assert-eql "xyz23" "~@[xyz~]~d" 23)
 (assert-eql "23"    "~@[xyz~]~d" nil 23)
 ;; test the use of the parameter instead of the argument
 (assert-eql "abc"   "~#[xyz~;abc~;def~]" 10)
 (assert-eql "xyz"   "~#[xyz~;abc~;def~]")
 (assert-eql ""      "~#[xyz~;abc~;def~]" 10 10 10)
 (assert-eql "abc"   "~v[xyz~;abc~;def~]" 1)
 (assert-eql "xyz"   "~v[xyz~;abc~;def~]" 0)
 (assert-eql ""      "~v[xyz~;abc~;def~]" 3)
 (assert-eql "abc"   "~1[xyz~;abc~;def~]" 10)
 (assert-eql "xyz"   "~0[xyz~;abc~;def~]" 10)
 (assert-eql ""      "~3[xyz~;abc~;def~]" 10)
 ;; test that giving the : modifier fails if there
 ;; are not exactly two clauses
 (assert-err         "~:[xyz~;abc~;def~]" nil)
 (assert-err         "~:[xyz~]" nil)
 ;; test that giving the @ modifier fails if there
 ;; is not exactly one clause
 (assert-err         "~@[xyz~;abc~]~d" nil 23)
 ;; test that giving no clauses fails
 (assert-err         "~[~]" nil 23)
 ;; test that giving both modifiers gives an error.
 (assert-err         "~:@[xyz~;abc~;def~]" 1 2 3)
 ;; test that giving the : modifier to a clause separator
 ;; other than the last gives an error
 (assert-err         "~[xyz~:;abc~:;def~]" 3)
 ;; test that giving the modifiers to ~] gives an error
 ;; test that giving parameters to ~; or ~] gives an error
 (assert-err         "~[xyz~;abc~;def~2]" 3)
 (assert-err         "~[xyz~;abc~2;def~]" 3)
 (assert-err         "~[xyz~;abc~;def~#]" 3)
 (assert-err         "~[xyz~;abc~#;def~]" 3)
 (assert-err         "~[xyz~;abc~;def~v]" 3)
 (assert-err         "~[xyz~;abc~v;def~]" 3))

(test-case
 iteration
 (assert-eql "ABCDE"  "~{~a~a~}~a" '(a b c d) 'e)
 ;; test that, with a parameter, at most that many
 ;; iterations are done.
 (assert-eql "ABE"    "~1{~a~a~}~a" '(a b c d) 'e)
 (assert-eql "E"      "~0{~a~a~}~a" '(a b c d) 'e)
 ;; test that the `:' modifier is taken into account
 (assert-eql "ABCDE"  "~:{~a~a~}~a" '((a b 1) (c d 2)) 'e)
 (assert-eql "ABE"    "~1:{~a~a~}~a" '((a b 1) (c d 2)) 'e)
 (assert-eql "E"      "~0:{~a~a~}~a" '((a b 1) (c d 2)) 'e)
 ;; test that the `@' modifier is taken into account
 (assert-eql "ABCD"   "~@{~a~a~}" 'a 'b 'c 'd)
 (assert-eql "ABC"    "~1@{~a~a~}~a" 'a 'b 'c 'd 'e)
 (assert-eql "A"      "~0@{~a~a~}~a" 'a 'b 'c 'd 'e)
 ;; test that using both modifiers is taken into account
 (assert-eql "ABCD"   "~:@{~a~a~}" '(a b) '(c d))
 (assert-eql "ABE"    "~1:@{~a~a~}~a" '(a b) 'e)
 (assert-eql "E"      "~0:@{~a~a~}~a" 'e))


(test-case
 directive.&
 (loop for i from 1 to 100
       for s1 = (make-string (1- i) :initial-element #\Newline)
       do (assert-eql s1 "~V&" i)))


;;; eof

