(in-package :symbols)

(defgeneric sym (annotated-symbol))

(defclass annotated-symbol (annotation-mixin)
  ((sym :initarg :sym :reader sym)))

(defmethod name ((s annotated-symbol))
  (name (sym s)))

(defclass annotated-arity-symbol (abstract-arity-symbol annotated-symbol) ())
(defclass annotated-parity-symbol (abstract-parity-symbol annotated-symbol) ())

(defmethod print-object ((s annotated-symbol) stream)
  (prin1 (sym s) stream)
  (princ #\+ stream)
  (princ (annotation s) stream))

(defmethod compare-object ((s1 annotated-symbol) (s2 annotated-symbol))
  (and (call-next-method) ;; to compare symbols without annotation
       (compare-object (annotation s1) (annotation s2))))

(defmethod make-annotated-parity-symbol ((psymbol parity-symbol) annotation)
  (make-instance 'annotated-parity-symbol :sym psymbol :annotation annotation))

(defmethod symbol-to-annotated-symbol ((psymbol parity-symbol))
  (make-annotated-parity-symbol psymbol nil))

(defmethod symbol-to-annotated-symbol ((symbol constant-symbol))
  symbol)

