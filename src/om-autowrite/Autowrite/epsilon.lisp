;;; Methods used by the reduction and the epsilon closure of an automaton.
(in-package :nautowrite)

(defgeneric epsilon-closure (stuff)
  (:documentation
   "Returns an equivalent automaton of automaton without epsilon-transitions."))

(defgeneric epsilon-closure-automaton (automaton)
  (:documentation
   "Returns an equivalent automaton of automaton without epsilon-transitions."))

(defmethod epsilon-p (automaton)
  (epsilon-p (transitions-of automaton)))

(defmethod nepsilon-closure-automaton ((automaton table-automaton))
  (when (epsilon-p automaton)
    (setf (transitions-of automaton)
	  (epsilon-closure (transitions-of automaton))))
  automaton)

(defmethod epsilon-closure-automaton ((automaton table-automaton))
;;  (nreduce-automaton
  (nepsilon-closure-automaton (duplicate-automaton automaton)))
;;)

(defun epsilon-transition-p (transition)
  (casted-state-p (left-handside transition)))

(defmethod expand-epsilon-transition ((key abstract-state) (value container) (transitions transitions))
  (maphash
   (lambda (key2 value2)
     (when (container-member key value2)
       (add-transition-to-transitions key2 (merge-containers (list value value2)) transitions)))
   transitions))
      
(defmethod expand-epsilon-transitions ((transitions transitions))
  (maphash
   (lambda (key value)
     (when (casted-state-p key)
       (remhash key transitions)
       (expand-epsilon-transition key value transitions)))
   transitions))

