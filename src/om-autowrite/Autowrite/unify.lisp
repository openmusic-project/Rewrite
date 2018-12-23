(in-package :nautowrite)

(defun instance (t1 t2)
  (cond
    ((var-p t2) t)
    ((var-p t1) nil)
    ((not (eq (root t1) (root t2))) nil)
    (t (or (constant t1) (every #'instance (arg t1) (arg t2))))))


(defun matching-lhs (term lhs)
  (remove-if-not (lambda (x) (instance term x)) lhs))

(defun linear-unify-with-a-term (term l)
  (if (null l)
      nil
      (let ((u (linear-unify term (car l)))
	    (ul (linear-unify-with-a-term term (cdr l))))
	(if (null u)
	    ul
	    (cons u ul)))))

(defun linear-unify-list (l1 l2)
  (let ((u (linear-unify (car l1) (car l2))))
    (and
     (not (null u))
     (if (null (cdr l1))
         (list u)
         (let ((ul (linear-unify-list (cdr l1) (cdr l2))))
           (and (not (null ul)) (cons u ul)))))))

(defun linear-unify (t1 t2)
  (cond 
    ((var-p t1) t2)
    ((var-p t2) t1)
    ((not (eq (root t1) (root t2))) nil)
    ((constant t1) t1)
    (t (let ((ul (linear-unify-list (arg t1) (arg t2))))
         (and
          ul
          (build-term (root t1) ul))))))

;; unification which computes a substitution returns two
;; values: the firt is the result of unification (nil or t)
;; and the second is the substitution

;; do we need an occur check??

;; terms1 and terms2 non empty lists of equal length
(defun unify-substi-list (terms1 terms2 sub)
  (multiple-value-bind (res sub-res)
      (unify-substi (car terms1) (car terms2) sub)
    (and
     res
     (if (endp (cdr terms1))
         (values t sub-res)
	 (unify-substi-list
	  (mapcar (lambda (x) (apply-substitution x sub-res)) (cdr terms1))
	  (mapcar (lambda (x) (apply-substitution x sub-res)) (cdr terms2))
	  sub-res)))))

(defun unify-substi (t1 t2 sub)
  (cond 
   ((var-p t1)
    (let ((v (my-assoc t1 sub)))
      (if v
	  (unify-substi (cadr v) t2 sub)
	  (values t (cons (make-binding t1 t2) sub)))))
   ((var-p t2) (unify-substi t2 t1 sub))
   ((not (compare-object (root t1) (root t2))) (values nil '()))
   ((constant t1) (values t sub))
   (t (unify-substi-list (arg t1) (arg t2) sub))))

(defun unify (t1 t2)
  (unify-substi t1 t2 '()))

(defun unify-with-a-term (term l)
  (if (null l)
      nil
      (let ((u (unify term (car l)))
	    (ul (unify-with-a-term term (cdr l))))
	(if (null u)
	    ul
	    (cons u ul)))))

