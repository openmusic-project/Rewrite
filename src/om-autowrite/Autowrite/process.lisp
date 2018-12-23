(in-package :gautowrite)

(defvar *with-processes* nil)
(defvar *aux-process* nil)

(defgeneric display-processname (name)
  (:documentation "print process name in appropriate location"))

(defmethod display-processname ((name string))
  (format t "~A" name))

(defmethod process-yield ()
  (clim-sys::process-yield))

(defmacro in-process (call name)
  `(progn
    (display-processname ,name)
    (if *with-processes*
	(setf *aux-process*
	      (clim-sys::make-process
	       (lambda ()
		 (setf *standard-output* *output-stream*)
		 ,call
		 (setf *aux-process* nil)
		 (display-processname "")
		 )
		 :name ,name))
	,call)
    (force-output *output-stream*)))

(defmacro in-process-with-time (call name)
  `(avec-temps-to-output-stream
    (in-process ,call ,name)))
 