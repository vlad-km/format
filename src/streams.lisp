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


(defstruct (simple-output-stream (:constructor %make-simple-output-stream)
                          :named (:type vector) (:initial-offset 8))
  ;; initial-offset:
  ;;           0 - signature STREAM
  ;;           1 - write-fn
  ;;           2 - read-char-fn
  ;;           3 - peek-char-fn
  ;;           4 - kind
  ;;           5 - data
  ;;           6 - (direction :out :in)
  ;;           7 - at-line-start
  (fd nil))

(defun make-simlpe-output-stream (&key pathname)
  (let ((fd (%fs-create-writable-stream pathname))
        (stm))
    (setq stm (%make-simple-output-stream :fd fd))
    (setf (aref stm 0) 'stream)
    (setf (aref stm 1) (lambda (string)
                         (%fs-stream-write fd string)))
    (setf (aref stm 6) :out)
    (setf (aref stm 7) t)
    stm))

;;; node:fs writible stream
(defun %fs-create-writable-stream (pathname)
  (let ((stm)
        (dir (jscl::oget (#j:FsPath:parse pathname) "dir")))
    (if (> (length dir) 0)
        (unless (#j:Fs:existsSync dir)
          ;; dont create new directory
          (error "No such output directory ~s" dir))
        ;; path eq "file.lisp"
        ;; so, dir eq "" and eq "./" by default
        )
    (setq stm (#j:Fs:createWriteStream pathname))
    stm))

(defun %fs-close-writable-stream (stm)
  ((jscl::oget stm "end")))

(defun %fs-stream-write (stm string)
  ((jscl::oget stm "write") string))

;;; stub cl function's - OPEN CLOSE
(defun open (pathname mode &optional (options '(type text)))
  (check-type pathname (or string symbol))
  (if (symbolp pathname) (setq pathname (symbol-name pathname)))
  (check-type options list)
  (if (consp (car options)) (setq options (car options)))
  ;;(print (list :args pathname mode options (consp options)))
  (let ((coding (if (eq (car options) 'type)
                    (case (cadr options)
			                ((text) :text)
			                ((binary) :binary)
                      (otherwise
                       (error "What the fuck option ~a." (cadr options))))
                    (error "Incorrect options ~a." options)))
        (stm)
	      (direction (case mode
		                 ((read) :input)
		                 ((write) :output)
		                 ((append) :append)
                     (otherwise
                      (error "What the fuck mode ~a?" mode)))))
    ;; restricted options - without aliases
    (cond ((eq direction :input)
           (setq stm (make-simple-input-stream
                      :buffer (%fs-read-file (%coerce-file-ext pathname) coding))))
          ((eq direction :output)
           (setq stm (make-simlpe-output-stream :pathname pathname)))
          (t (error "This stream (~a) is not implemented yet." direction)))
    stm))

(defun close (stm)
  (cond #+nil((sample-input-stream-p stm)
         (%close-sample-input-stream stm))
        ((simple-output-stream-p stm)
         (%fs-close-writable-stream (simple-output-stream-fd stm)))
        (t (error "the ~a is not stream~&" stm))))
;;; EOF
