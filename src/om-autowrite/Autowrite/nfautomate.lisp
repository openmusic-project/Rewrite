(in-package :nautowrite)

(defun new-terms (l1 l2)
  (if (or (null l1) (null l2))
      nil
      (my-union
       (linear-unify-with-a-term (car l1) l2)
       (new-terms (cdr l1) l2))))

(defun close-under-down-arrow (l)
  (let ((nt (new-terms l l)))
    (if (my-subsetp nt l)
	l
	(close-under-down-arrow (my-union nt l)))))

(defun unify-prefix (t1 t2) ; t1 unifies with t2 and t2 is a prefix of t1
  (cond
    ((var-p t2)
     t1)
    ((var-p t1)
     nil)
    ((not (eq (root t1) (root t2)))
     nil)
    ((constant t1)
     t2)
    (t (if (unify-prefix-arg (arg t1) (arg t2))
	    (build-term (root t1) (unify-prefix-arg (arg t1) (arg t2)))
	    nil))))

(defun unify-prefix-arg (l1 l2) ; non empty lists of args of terms
  (let ((u (unify-prefix (car l1) (car l2))))
    (cond
      ((null u)
       nil)
      ((null (cdr l1))
       (list (car l2)))
      (t
       (let ((ul (unify-prefix-arg (cdr l1) (cdr l2))))
	 (if (null ul)
	     nil
	     (cons (car l2) ul)))))))

(defun max-prefix (l)
  (if (null (cdr l))
      (car l)
      (if (unify-prefix (car l) (cadr l))
	  (max-prefix (cons (car l) (cddr l)))
	  (max-prefix (cdr l)))))

(defun unify-with-a-prefix (term l)
  (cond
    ((null l)
     nil)
    ((unify-prefix term (car l))
     (cons (car l) (unify-with-a-prefix term (cdr l))))
    (t
     (unify-with-a-prefix term (cdr l)))))

(defun make-nf-transitions-for-one-symbol (symbol lhs csub &key (redex nil))
  (let ((terms (mapcar
		(lambda (y)
		  (build-term symbol y))
		(arrange csub (arity symbol)))))
  (mapc
   (lambda (term)
     (let ((qkey (make-qkey term))
	   (instance-of-lhs (instance-of-any term lhs)))
       (when (or (not instance-of-lhs) redex)
	 (add-dtransition-to-transitions
	  qkey
	  (make-term-state
	   (max-prefix (unify-with-a-prefix term csub)))
	  *global-transitions*))))
   terms)))

(defmethod make-nf-transitions (lhs (signature signature))
  (let* ((nvsub (strict-subterms-list lhs))
	 (sub (cons *new-var* nvsub))
	 (csub  (close-under-down-arrow sub)))
    (mapc
     (lambda (x) (make-nf-transitions-for-one-symbol x lhs csub))
     (signature-symbols signature))))

(defun make-nf-automaton (lhs signature &key (bullet nil) (extra nil))
  "deterministic automaton which recognizes normal forms"
  (when bullet
    (setf signature (add-bullet-symbol signature)))
  (when extra
    (setf signature (add-extra-symbol signature)))
  (with-new-transitions
    (make-nf-transitions lhs signature)
    (nreduce-automaton
     (make-automaton *global-transitions*
		     :name "nf"
		     :signature signature
		     :finalstates (states-from-states-table (states-table *global-transitions*)))))
)



