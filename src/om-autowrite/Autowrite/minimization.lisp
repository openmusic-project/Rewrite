(in-package :nautowrite)

(defgeneric minimal-automaton-p (automaton)
  (:documentation "true if AUTOMATON is minimal"))

(defmethod minimal-automaton-p ((automaton table-automaton))
  (and (deterministic-p automaton) (complete-p automaton) (reduced-p automaton)
       (every #'container-singleton-p (equivalence-classes-automaton automaton))))

(defgeneric nminimize-automaton (automaton)
  (:documentation "DESTRUCTIVE minimizes AUTOMATON"))

(defgeneric minimize-automaton (automaton)
  (:documentation
   "returns an automaton which is the minimization of AUTOMATON (non destructive)"))

(defmethod quasi-simplified-p ((automaton table-automaton))
  (and
   (reduced-p automaton)
   (deterministic-p automaton)
   (every
    #'singleton-container-p
    (equivalence-classes automaton))))

(defmethod simplified-p ((automaton table-automaton))
  (and (quasi-simplified-p automaton)
       (not
	(and (complete-p automaton) (find-puits-state automaton)))))

(defmethod minimal-p ((a table-automaton))
  (and
   (quasi-simplified-p a)
   (complete-p a)))

(defmethod nminimize-automaton ((automaton table-automaton))
  (unless (minimal-p automaton)
    (nsimplify-automaton automaton)
    (ncomplete-automaton automaton))
  automaton)

(defmethod minimize-automaton ((automaton table-automaton))
  (if (minimal-p automaton)
      automaton
      (nminimize-automaton (duplicate-automaton automaton))))
