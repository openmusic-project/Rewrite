(in-package :terms)

(defvar *variables* nil "variables declared in spec")

(defun init-variables ()
  (setf *variables* nil))

(defclass var (named-object abstract-term) ())

(defmethod print-object :after ((v var) stream))
;;  (format stream "!"))

(defmethod show ((v var) &optional stream)
  (format stream "~A" (name v)))

(defun var-p (v)
  (typep v 'var))

(defun find-var-from-name (name vars)
  (car (member name vars :test #'eq :key #'name)))

(defmethod compare-object ((v1 var) (v2 var))
  (eq (name v1) (name v2)))

(defun make-var (name)
  (let ((found (find-var-from-name name *variables*)))
    (or found
	(let ((newvar (make-instance 'var :name name)))
	  (push newvar *variables*)
	  newvar))))

(defun variable-member (v l)
  (member v l :test #'eq))

(defun variable-delete-duplicates (l)
  (delete-duplicates l :test #'eq))

(defun variable-delete (v l)
  (delete v l :test #'eq))

(defgeneric vars-of (object))

(defmethod vars-of ((objects list))
  (my-remove-duplicates
   (mappend #'vars-of objects)))

(defmethod vars-of ((var var))
  (list var))
