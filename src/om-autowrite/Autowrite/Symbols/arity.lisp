(in-package :symbols)

(defgeneric arity (symbol)
  (:documentation "arity ot SYMBOL"))

(defclass parity-mixin ()
  ((arity :initarg :arity :reader arity)))