(in-package :nautowrite)

(defmethod intersection-emptiness ((aut1 abstract-automaton) (aut2 abstract-automaton))
  (let ((signature1 (signature aut1))
	(signature2 (signature aut2)))
    (unless (homogeneous-constant-signatures-p signature1 signature2)
      (return-from intersection-emptiness (values nil 'constant-signatures-not-homogenous)))
    (automaton-emptiness
     (intersection-automaton-compatible
      (to-fly-automaton aut1)
      (to-fly-automaton aut2)))))
