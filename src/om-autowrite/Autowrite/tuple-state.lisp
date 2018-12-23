(in-package :nautowrite)

(defclass tuple-state (abstract-state)
  ((%tuple :initarg :tuple :reader tuple)))

(defmethod print-object ((tuple-state tuple-state) stream)
  (format stream "<")
  (display-sequence (tuple tuple-state) stream :sep ",")
  (format stream ">"))

;; (defmethod state-key ((tuple-state tuple-state))
;;   (tuple tuple-state))

(defun make-tuple-state (tuple)
  (make-instance 'tuple-state :tuple tuple))

(defmethod compare-object ((state1 tuple-state) (state2 tuple-state))
  (every #'compare-object (tuple state1) (tuple state2)))

(defmethod tuple ((casted-state casted-state))
  (tuple (state casted-state)))