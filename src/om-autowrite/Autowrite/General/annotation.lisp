(in-package :general)

(defgeneric annotation (object)
  (:documentation "annotation of OBJECT"))

(defclass annotation-mixin ()
  ((%annotation :initarg :annotation :accessor annotation :initform nil)))

(defmethod print-object :after ((a annotation-mixin) stream)
  (format stream "+~A" (annotation a)))

(defgeneric erase-annotation (a)
  (:documentation "annotation set to NIL"))

(defmethod erase-annotation ((a annotation-mixin))
  (setf (annotation a) nil))
  
(defmethod compare-objext ((a1 annotation-mixin) (a2 annotation-mixin))
  (equalp (annotation a1) (annotation a2)))