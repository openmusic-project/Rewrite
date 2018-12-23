(in-package :nautowrite)
;; f(q1,q2) -> q3
;; f(p1,p2)-> p3
;; donne 
;; ;; f(<q1,p1>,<q2,p2>) -> <q3,p3> ainsi que

;; c'est beaucoup plus compliqué si root est commutatif
;; car on a 
;; f(q2,q1) -> q3
;; f(p2,p1)-> p3
;; implicites 
;; ca donne
;; f(<q1,p1>,<q2,p2>) -> <q3,p3> ainsi que
;; f(<q1,p2>,<q2,p1>) -> <q3,p3>

(defmethod intersection-automaton-compatible :before ((a1 abstract-automaton) (a2 abstract-automaton))
  (assert (equiv-signature-p (signature a1) (signature a2))))

(defmethod intersection-automaton-compatible
    ((aut1 abstract-automaton) (aut2 abstract-automaton))
  (intersection-automaton-compatible
   (to-fly-automaton aut1)
   (to-fly-automaton aut2)))

(defmethod intersection-automaton-compatible ((aut1 table-automaton) (aut2 table-automaton))
  (compile-automaton
   (call-next-method)))

(defmethod intersection-automaton ((aut1 abstract-automaton) (aut2 abstract-automaton))
  :documentation "automata must be without epsilon transitions and have compatible signatures"
  (let ((s1 (signature-symbols (signature aut1)))
	(s2 (signature-symbols (signature aut2))))
    (let* ((is (symbol-intersection s1 s2))
	   (d1 (symbol-set-difference s1 is))
	   (d2 (symbol-set-difference s2 is))
	   (a1 (automaton-without-symbols d1 aut1))
	   (a2 (automaton-without-symbols d2 aut2)))
      (intersection-automaton-compatible a1 a2))))

(defmethod intersection-automaton-gen (&rest automata)
  (let ((is
	 (reduce #'symbol-intersection (mapcar #'signature automata))))
    (apply #'intersection-automaton-compatible-gen
	   (mapcar (lambda (aut)
		     (automaton-without-symbols
		      aut
		      (symbol-set-difference (signature aut) is)))
		   automata))))
  
