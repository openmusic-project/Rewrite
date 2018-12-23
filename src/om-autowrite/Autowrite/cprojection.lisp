(in-package :nautowrite)

(defmethod cprojection ((s abstract-parity-symbol))
  (list s))

(defmethod cprojection ((s color-constant-symbol))
  (list (make-constant-symbol (name s))))

(defmethod cprojection ((signed-object signed-object))
  (vhomomorphism signed-object #'cprojection))

(defmethod cprojection-automaton ((a abstract-automaton))
  (cprojection (cast-automaton a)))

