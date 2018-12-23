(in-package :terms)

;;; variables and substitution

(defun make-binding (var val)
  (list var val))

(defun g-gen-substitution (varlist states)
  (mapcar (lambda (x)
	    (mapcar #'list varlist x)) (arrange states (length varlist))))
