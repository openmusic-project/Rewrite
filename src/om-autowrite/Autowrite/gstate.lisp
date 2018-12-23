(in-package :nautowrite)

(defclass gstate (abstract-state)
  ((container :initarg :container :reader container)))

;; (defmethod state-key ((gstate gstate))
;;   (contents (container gstate)))

(defmethod print-object ((gstate gstate) stream)
  (format stream "[~A]" (container gstate)))

(defmethod make-gstate ((container container))
  (make-instance
   'gstate
   :container (make-container
	       (sort (copy-list
		      (contents container)) #'strictly-ordered-state-p))))

(defmethod make-gstate ((container ordered-container))
  (make-instance 'gstate :container container))

(defmethod make-gstate ((state abstract-state))
  (make-instance 'gstate :container (make-container-from-state state)))

(defmethod make-gstate ((c (eql nil)))
  nil)

(defmethod compare-object ((s1 gstate) (s2 gstate))
  (compare-object (container s1) (container s2)))