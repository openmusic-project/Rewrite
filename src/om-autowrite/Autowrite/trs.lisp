(in-package :nautowrite)

(defvar *d-iterations* 0)

(defgeneric get-rules (trs)
  (:documentation "access to the rules of a trs"))

(defgeneric (setf get-rules) (trs newrules)
  (:documentation "modifies the rules of the trs"))

(defclass trss ()
  ((trs-table :initform (make-hash-table :test #'equal)
		    :accessor table)))

(defun make-trss ()
  (make-instance 'trss))

(defclass trs (named-object signature-mixin)
  ((rules :initform nil
	  :initarg :rules
	  :accessor get-rules
	  )))

(defmethod symbols-from ((trs trs))
  (symbols-from (get-rules trs)))

(defun make-trs (name rules signature)
  (make-instance 'trs :name name :rules rules :signature signature))

(defmethod show :after ((trs trs) &optional (stream t))
  (format stream "~%~A"  (get-rules trs)))

(defmethod print-object :after ((trs trs) stream)
  (format stream "~%"))

(defmethod size ((trs trs))
  (size (get-rules trs)))

(defmethod growing ((trs trs))
  (growing (get-rules trs)))
