(in-package :nautowrite)

(defun complement-automaton-name (name)
  (or (decompose-name "C-" name)
      (compose-name "C-" name)))

(defmethod ncomplement-automaton ((automaton table-automaton))
  (assert (and (deterministic-p automaton) (complete-p automaton)))
  (setf (get-finalstates automaton)
	(container-difference (get-states automaton) (get-finalstates automaton))
	(name automaton)
	(complement-automaton-name (name automaton)))
  (unset-puits-state automaton)
  automaton)

(defgeneric complement-automaton (automaton))

(defmethod complement-automaton ((automaton table-automaton))
  :documentation "complements of automaton"
  (when (and (complete-p automaton) (deterministic-p automaton))
    (setf automaton (duplicate-automaton automaton))) ;; car si c'est le cas il ne sera pas duplique
  (ncomplement-automaton
     (complete-automaton (determinize-automaton automaton))))

(defmethod inclusion-automaton ((aut1 abstract-automaton) (aut2 abstract-automaton))
  :documentation "automata must have the same signature
  returns t, nil if l(aut1) included in l(aut2) 
  returns nil, term otherwise"
  (let ((caut2 	(nreduce-automaton (complement-automaton aut2))))
    (intersection-emptiness aut1 caut2)))

(defmethod equality-automaton ((aut1 abstract-automaton) (aut2 abstract-automaton))
  (multiple-value-bind (res term)  
      (inclusion-automaton aut1 aut2)
    (if res
	(inclusion-automaton aut2 aut1)
	(values nil term))))
