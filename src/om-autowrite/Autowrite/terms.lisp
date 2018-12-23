(in-package :nautowrite)

(defclass terms ()
  ((term-table :initform (make-hash-table :test #'equal)
		    :accessor table)))

(defun make-terms ()
  (make-instance 'terms))

(defun redex (term lhs)
  (if (linear-terms-p lhs)
      (linear-unify-with-a-term term lhs)
      (unify-with-a-term term lhs)))

(defun instance-of-any (term lhs)
  (find-if (lambda (lh) (instance term lh)) lhs))

(defun orthogonal-p (lhs)
  (and (linear-terms-p lhs) (not (overlapp lhs))))
 
(defun normal-form (term lhs)
  (or
   (constant term)
   (and
    (not (redex term lhs)) 
    (every (lambda (x) (normal-form x lhs)) (arg term)))))

(defun overlapp (lhs)
   (or 
    (overlapp-patterns lhs)
    (overlapp-subpatterns lhs (strict-subterms-list lhs))))

(defun overlapp-patterns (l)
  (if (null l)
       nil
       (or
	(linear-unify-with-a-term (car l) (cdr l))
	(overlapp-patterns (cdr l)))))

(defun overlapp-subpatterns (lhs subpatterns)
  (find-if (lambda (x) (linear-unify-with-a-term x subpatterns)) lhs))

(defun filter-normal-forms (l lhs)
  (remove-if-not (lambda (x) (normal-form x lhs)) l))

(defun filter-non-instance-of-strict-subterm  (l lhs)
  (remove-if (lambda (x) (instance-of-strict-subterm x lhs)) l))

(defun filter-non-redex (l lhs)
  (remove-if (lambda (x) (redex x lhs)) l))

(defun instance-of-strict-subterm (term lhs)
  (linear-unify-with-a-term term (strict-subterms lhs :var nil)))

(defun equiv-terms-p (terms1 terms2)
  (and
   (= (length terms1) (length terms2))
   (subsetp terms1 terms2 :test #'compare-object)
   (subsetp terms1 terms2 :test #'compare-object)))
