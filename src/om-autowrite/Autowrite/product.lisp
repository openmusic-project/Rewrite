(in-package :nautowrite)

(defmethod product-automaton ((a1 abstract-automaton) (a2 abstract-automaton))
  (product-automaton (to-fly-automaton a1) (to-fly-automaton a2)))

(defmethod product-automaton ((a1 table-automaton) (a2 table-automaton))
  (compile-automaton (call-next-method)))
