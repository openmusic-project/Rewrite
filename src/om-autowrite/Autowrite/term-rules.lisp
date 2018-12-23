(in-package :nautowrite)

(defgeneric instance-of-any-rule (term rules)
  (:documentation "true if TERM is an instance of a rule in RULES"))

(defgeneric redex-positions-lhs (term lhs)
  (:documentation "return a list of redex positions in TERM according to the left-handsides LHS"))

(defgeneric outermost-redex-positions-lhs (term lhs)
  (:documentation "return a list of outermost redex positions in TERM according to the left-handsides LHS"))

(defmethod instance-of-any-rule (term (rules rules))
  (instance-of-any-rule term (rules-list rules)))

(defmethod instance-of-any-rule (term (rules list))
  (find-if (lambda (rule) (instance term (left-handside rule))) rules))

(defmethod redex-positions-lhs ((term term) lhs)
  (let ((tp (term-positions term)))
    (remove-if-not (lambda (p) (redex (term-at-position term p) lhs)) tp)))

(defmethod outermost-redex-positions-lhs ((term term) lhs)
  (outer-positions (redex-positions-lhs term lhs)))

(defun reduction (term position rules)
  (let* ((subterm (term-at-position term position))
	 (rule (instance-of-any-rule
		subterm
		rules)))
    (multiple-value-bind (res subst)
	(unify-substi (left-handside rule) subterm '())
      (values
       (replace-term-at-position term position (apply-substitution (right-handside rule) subst))
       res))))

