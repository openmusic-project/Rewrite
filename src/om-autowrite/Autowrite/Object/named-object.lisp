(in-package :object)

(defgeneric name (named-object)
  (:documentation "the name of the NAMED-OBJECT"))

(defclass named-object ()
  ((name :initarg :name
	 :initform ""
	 :accessor name)))

(defmethod compare-object ((o1 named-object) (o2 named-object))
  (eq (name o1) (name o2)))

(defmethod print-object ((object named-object) stream)
  (format stream "~A" (name object))
  object)

(defmethod show ((object named-object) &optional (stream t))
  (format stream "~A" (name object)))

(defun rename-object (object name)
  (setf (name object) name)
  object)


