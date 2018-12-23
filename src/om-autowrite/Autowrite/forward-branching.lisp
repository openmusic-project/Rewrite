(in-package :nautowrite)
(defgeneric forward-branching-def (rules)
  (:documentation "returns a forward-branching index tree if the set of rules is forward-branching, nil otherwise"))

(define-condition not-fb () ())
(define-condition not-ss () ())
(define-condition overlapp () ())

(defun subscheme-p (s)
  (member (root s) *dsymbols* :test #'eq))

(defun blocage (p greatersubschemes)
  (some (lambda (x) (omega-term-p (term-at-position x p))) greatersubschemes))

;;; check forward-branching using definition
;;; cubic complexity
(defmethod forward-branching-def ((schemes list))
  (let* ((*dsymbols* (dsymbols schemes))
	 (preredexes  (strict-prefixes-of-terms schemes))
	 (subschemes (remove-if-not #'subscheme-p (subterms-list schemes)))
	 (blocage (find-if (lambda (M)
			     (every (lambda (u)
				      (blocage u (strictly-greater-terms M subschemes)))
				    (omega-positions M)))
			   preredexes)))
    (values (not blocage) blocage)))


(defmethod forward-branching-def ((sys trs))
  (forward-branching-def (schemes-from-terms (left-handsides (get-rules sys)))))
