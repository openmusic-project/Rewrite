(in-package :nautowrite)

(defun disjoint-union-automaton-gen (automata)
  (setf automata (remove-duplicates automata :test #'eq))
  (when (null (cdr automata))
    (return-from disjoint-union-automaton-gen (car automata)))
  (setf automata
	(mapcar #'duplicate-automaton automata))
  (nindex-states-automata automata)
  (let ((transitions (disjoint-union-transitions-gen (mapcar #'transitions-of automata))))
    (make-automaton
     transitions
     :name (format nil "U~A" (mapcar #'name automata))
     :signature (apply #'merge-signature (mapcar #'signature automata))
     :finalstates (merge-containers (mapcar #'get-finalstates automata)))))

(defmethod disjoint-union-automaton ((a1 table-automaton) (a2 table-automaton))
  (disjoint-union-automaton-gen (list a1 a2)))

(defmethod union-automaton :before ((a1 abstract-automaton) (a2 abstract-automaton))
  (assert (merge-signature (signature a1) (signature a2))))

(defmethod union-automaton ((aut1 abstract-automaton) (aut2 abstract-automaton))
  (union-automaton
   (to-fly-automaton (complete-automaton aut1))
   (to-fly-automaton (complete-automaton aut2))))

(defmethod union-automaton ((aut1 table-automaton) (aut2 table-automaton))
  (compile-automaton (call-next-method)))
