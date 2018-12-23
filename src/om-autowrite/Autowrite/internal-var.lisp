(in-package :nautowrite)

(defvar *internal-var-number* 0)

(defclass internal-var (var)
  ((number :initform (incf *internal-var-number*) :reader number)))

(defun make-internal-var (&optional (name *internal-var-name*))
  (make-instance 'internal-var :name name))

(defmethod print-object :after ((v internal-var) stream)
  (format stream  "~A" (number v)))

(defmethod compare-object ((v1 internal-var) (v2 internal-var))
  (and (eq (name v1) (name v2))
       (= (number v1) (number v2))))

(defun internal-var-p (v)
  (typep v 'internal-var))


