(in-package :gautowrite)

(define-presentation-type data-file-name (&optional (pattern "*")))
(define-presentation-type automaton ())
;; (define-presentation-type trs ())
;; (define-presentation-type termset ())
;; (define-presentation-type term ())

(define-presentation-method present (object (type automaton) stream view &key)
  (declare (ignore view))
  (write-string (name object) stream))

