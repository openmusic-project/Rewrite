(in-package :terms)
(defvar *aux-variables* nil)
(defvar *aux-variable-number* -1)

(defclass aux-var (var) ())

(defun reset-aux-variables (&optional (n -1))
  (setf *aux-variables* nil)
  (setf *aux-variable-number* n))
  
(defun make-aux-var (&optional (str "Y"))
  (let* ((name (if (zerop (incf *aux-variable-number*))
		  str
		  (format nil "~A~A" str *aux-variable-number*)))
	 (found (find-var-from-name name *aux-variables*)))
    (or found
	(let ((newvar (make-instance 'aux-var :name (make-name name))))
	  (push newvar *aux-variables*)
	  newvar))))

(defun aux-var-p (v)
  (typep v 'aux-var))
