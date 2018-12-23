(in-package :nautowrite)

(defmethod find-puits-state ((automaton table-automaton))
;;  (assert (complete-p automaton))
  (let ((nonfinalstates (container-difference (get-states automaton) (get-finalstates automaton))))
    (find-if
     (lambda (state)
       (dead-end-state-p state (transitions-of automaton)))
     (contents nonfinalstates))))

(defmethod nsimplify-minimal-automaton ((automaton table-automaton))
  :documentation "Destructive: simplifies minimal AUTOMATON"
  (nuncomplete-automaton automaton))

(defmethod simplify-minimal-automaton ((automaton table-automaton))
  :documentation "returns AUTOMATON if reduced or the reduced version of AUTOMATON otherwise"
  (nsimplify-minimal-automaton (duplicate-automaton automaton)))

(defmethod nsimplify-automaton ((automaton table-automaton))
  (assert (deterministic-p automaton))
  (unless (simplified-p automaton)
    (if (minimal-p automaton)
	(nsimplify-minimal-automaton automaton)
	(progn
	  (nreduce-automaton automaton)
	  (setf automaton (determinize-automaton automaton))
	  (nquotient-automaton
	   automaton
	   (representative-states
	    (contents (get-states automaton))
	    (equivalence-classes automaton)))
	  (when (find-puits-state automaton)
	    (ncomplete-automaton automaton))
	  (when (minimal-p automaton)
	    (nsimplify-minimal-automaton automaton)))))
  automaton)

(defmethod simplify-automaton ((automaton table-automaton))
  (if (simplified-p automaton)
      automaton
      (if (deterministic-p automaton)
	  (nsimplify-automaton (duplicate-automaton automaton))
	  (nsimplify-automaton (determinize-automaton automaton)))))
 