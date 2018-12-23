(in-package :nautowrite)

(define-condition error-not-ground () ())
(define-condition error-incorrect-signature () ())
(define-condition error-read () ())

(defun load-file-from-absolute-filename  (absolute-filename reader)
  (with-open-file (foo absolute-filename :direction :input :if-does-not-exist nil)
    (if foo
	(funcall reader foo)
	(prog1
	    nil
	  (format *error-output* "unable to open ~A~%" absolute-filename)))))

(defun absolute-filename (filename)
  (concatenate 'string *data-directory* filename))

(defun read-termset (stream)
  (handler-case
      (parse-termset stream)
     (autowrite-parse-error (c)
       (prog1 nil
	 (format *error-output* "read-termset parse error ~A: ~A~%" (token c) (message c) )))
     (all-errors () 
       (prog1  nil
	 (format *error-output* "Unable to read a termset ~%")))))

(defun read-term (stream)
  (let ((term
	 (handler-case
	     (parse-term stream)
	   (autowrite-parse-error (c)
	     (prog1 nil
	       (format *error-output* "read-term parse error ~A: ~A~%" (token c) (message c) )))
	  (all-errors ()
	     (prog1 nil (format *error-output* "Unable to read a term~%"))))))
    (if term
	(if (ground term)
	    term
	    (prog1 nil (format *error-output* "unground term~%")))
	(format *error-output* "no term~%"))))

(defun read-rules (stream)
  (handler-case
      (parse-rules stream)
    (autowrite-parse-error (c)
      (prog1 nil (format *error-output* "read-rules parse error ~A: ~A~%" (token c) (message c))))
    (all-errors ()
      (prog1 nil
	(format *error-output* "Unable to read rules")))))

(defun read-transitions (stream)
  (handler-case
      (parse-transitions stream)
    (autowrite-parse-error (c)
      (prog1 nil (format *error-output* "read-transitions parse error ~A: ~A~%" (token c) (message c))))
    (all-errors ()
      (prog1 nil
	(format *error-output* "Unable to read transitions")))))

(defun read-automaton (stream)
  (handler-case 
      (parse-automaton stream)
    (autowrite-parse-error (c)
      (prog1 nil (format *error-output* "read-automaton parse error ~A: ~A~%" (token c) (message c))))
    (error ()
      (prog1 nil
	(format *error-output* "error")))))

(defun input-term (s)
  (with-input-from-string (foo s) (parse-term foo)))
