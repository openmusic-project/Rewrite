(in-package :nautowrite)

(defclass named-state (abstract-state)
  ((name :reader name :initarg :name)))

(defgeneric make-named-state (state-name)
  (:documentation "makes a new name-state labeled with STATE-NAME"))

(defmethod make-named-state ((state-name string))
  (make-instance 'named-state :name (make-name state-name)))

(defmethod print-object ((named-state named-state) stream)
  (format stream "~A" (name named-state)))
