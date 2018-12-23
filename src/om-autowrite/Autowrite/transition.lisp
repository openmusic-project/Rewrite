(in-package :nautowrite)

(defclass transition (rule) ())

(defgeneric make-transition (lh rh)
  (:documentation
   "create an automaton transition from a left-handside LH
(state or flat-term) and a right-handside RH (a state)"))

(defmethod make-transition ((lh abstract-state) (rh abstract-state))
  (make-instance 'transition :lh lh :rh rh))

(defmethod make-transition ((lh term) (rh casted-state))
  (assert (every #'casted-state-p (arg lh)))
  (make-instance 'transition :lh lh :rh rh))

(defmethod compare-object ((transition1 transition) (transition2 transition))
  (and
   (compare-object (left-handside transition1) (left-handside transition2))
   (compare-object  (right-handside transition1)
		    (right-handside transition2))))


