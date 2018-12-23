(in-package :symbols)

(defgeneric  signature (signed-object)
  (:documentation "signature of a signed object"))

(defgeneric constant-signature (signed-object)
  (:documentation "signature of a signed object"))

(defclass signed-object () ())

(defclass signature-mixin (signed-object)
  ((signature
    :initform nil
    :initarg :signature
    :accessor signature)))

(defmethod constant-signature ((signed-object signed-object))
  (constant-signature (signature signed-object)))


