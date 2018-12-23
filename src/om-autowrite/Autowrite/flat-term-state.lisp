(in-package :nautowrite)

(defun make-flat-term (root &optional (arg nil))
  (assert (every #'abstract-state-p arg))
  (build-term root arg))

(defgeneric flat-term-from-key (key)
  (:documentation "transforms a flat-term in a Lisp representation into a flat-term"))

(defgeneric flat-term-to-key (ft)
  (:documentation "transforms the FT into Lisp representation"))

(defmethod flat-term-from-key ((s abstract-state))
  s)

(defmethod flat-term-from-key ((key list))
  (make-flat-term (car key) (cdr key)))

(defmethod flat-term-to-key ((s abstract-state))
  s)

(defmethod flat-term-to-key ((term term))
  (cons (root term) (arg term)))

