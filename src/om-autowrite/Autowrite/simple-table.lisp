(in-package :nautowrite)

(defun make-empty-simple-table ()
  (make-hash-table :test #'equal))

(defmethod simple-table-set ((key list) rh (simple-table hash-table))
  (when (and (container-p rh) (singleton-container-p rh))
    (setf rh (car (contents rh))))
  (setf (gethash key simple-table) rh))

(defmethod simple-table-access ((key list) (simple-table hash-table))
  (gethash key simple-table))

(defmethod simple-table-access-modc ((key list) (simple-table hash-table))
  (simple-table-access (key-modc (car key) (cdr key)) simple-table))

(defmethod simple-table-set-modc ((key list) rh (simple-table hash-table))
  (simple-table-set (key-modc (car key) (cdr key)) rh simple-table))

(defmethod simple-table-add-modc ((key list) rh (simple-table hash-table))
  (let ((old (simple-table-access-modc key simple-table)))
    (simple-table-set-modc key (target-union rh old) simple-table)))

(defmethod simple-table-list ((st-table hash-table))
  (let ((transitions '()))
    (maphash
     (lambda (key value)
       (let ((lh (flat-term-from-key key)))
	 (dolist (state (contents value))
	   (push (make-transition lh state) transitions))))
     st-table)
    transitions))

(defmethod simple-table-copy ((simple-table hash-table))
  (let ((new-st (make-empty-simple-table)))
    (maphash
     (lambda (key target)
       (setf (gethash key new-st) target))
     simple-table)
    new-st))

(defmethod simple-table-number-of-transitions ((simple-table hash-table))
  (let ((n 0))
    (maphash
     (lambda (key value)
       (declare (ignore key))
       (incf n (length (contents value))))
     simple-table)
    n))

(defmethod simple-table-cright-handsides ((simple-table hash-table))
  (let ((crhs (make-empty-ordered-container)))
    (maphash
     (lambda (key value)
       (declare (ignore key))
       (mapc (lambda (state)
	       (setf crhs (container-adjoin state crhs)))
	     (contents value)))
     simple-table)
    crhs))
 

(defmethod  simple-table-deterministic-p ((simple-table hash-table))
  (maphash
   (lambda (key value)
     (declare (ignore key))
     (unless (target-deterministic-p value)
       (return-from simple-table-deterministic-p)))
   simple-table)
  t)
	
(defmethod simple-table-dead-end-state-p (state (simple-table hash-table))
  (let ((tuples-rh
	 (simple-table-key-value-list simple-table)))
    (notany
     (lambda (tuple-rh)
       (and (not (eq state (second tuple-rh)))
	    (state-member state (cdar tuple-rh))))
     tuples-rh)))

(defmethod simple-table-filter-with-states ((states ordered-container) (simple-table hash-table))
  (let ((new-simple-table (make-empty-simple-table)))
    (maphash
     (lambda (key value)
       (when (and (has-only-marked-states key states)
		  (has-only-marked-states value states))
	 (setf (gethash key new-simple-table) value)))
     simple-table)
    new-simple-table))

(defmethod simple-table-union ((simple-table1 hash-table) (simple-table2 hash-table))
  (let ((new-st (simple-table-copy simple-table1)))
    (maphash 
     (lambda (key value)
       (setf (gethash key new-st) (target-union (gethash key new-st) value)))
      simple-table2)
     new-st))

(defmethod simple-table-key-value-list ((simple-table hash-table))
  (let ((acc '()))
    (maphash
     (lambda (key value)
       (push (list key value) acc))
     simple-table)
    (nreverse acc)))

(defmethod simple-table-show ((simple-table hash-table) &optional (stream t))
  (maphash
   (lambda (key value)
     (format stream "~A -> ~A~%" (flat-term-from-key key) value))
   simple-table))

(defmethod simple-table-get-states-from ((simple-table hash-table))
  (let ((container (make-empty-ordered-container)))
    (maphash
     (lambda (key value)
       (loop
	  for state in (cdr key)
	  do (setf container (container-adjoin state container)))
       (loop
	  for state in (contents value)
	  do (setf container (container-adjoin state container))))
     simple-table)
    container))

(defmethod intersection-two-transitions (root states1 rhs1 states2 rhs2 (ist hash-table))
  (simple-table-set
   (cons
    root
    (mapcar
     (lambda (s1 s2) (make-tuple-state (list s1 s2)))
     states1
     states2))
   (make-container
    (mapcar #'make-tuple-state
	    (cartesian-product
	     (list (contents rhs1) (contents rhs2)))))
   ist))

;; side effect on ist
(defmethod simple-table-intersection ((simple-table1 hash-table) (simple-table2 hash-table))
  (let ((st1 (simple-table-uncommutative simple-table1))
	(st2 (simple-table-uncommutative simple-table2))
	(ist (make-empty-simple-table)))
    (maphash
     (lambda (key1 value1)
       (maphash
	(lambda (key2 value2)
	  (let ((root1 (car key1))
		(root2 (car key2)))
	    (when (eq root1 root2)
	      (intersection-two-transitions root1 (cdr key1) value1 (cdr key2) value2 ist))))
	st2))
     st1)
    (simple-table-to-commutative ist)))

;; ir pourra contenir plusieurs transitions dans le cas de symboles commutatif
;; dans le cas non commutatif ir contient un seul element et le mapc ne fait qu'un truc

(defmethod simple-table-epsilon-p ((simple-table hash-table))
  (maphash
   (lambda (key value)
     (when (casted-state-p key)
       (return-from simple-table-epsilon-p (make-transition key (car (contents value))))))
   simple-table))

(defmethod simple-table-expand-epsilon-transition
    ((key1 casted-state) (value1 casted-state) (simple-table hash-table))
  (maphash
   (lambda (key2 value2)
     (when (container-member key1 value2)
       (simple-table-set-modc key2 (merge-containers (list value1 value2)) simple-table)))
   simple-table))
 
(defmethod simple-table-expand-epsilon-transitions ((simple-table hash-table))
  (maphash
   (lambda (key value)
     (when (casted-state-p key)
       (remhash key simple-table)
       (dolist (state (contents value))
	 (table-expand-epsilon-transition key state simple-table))))
   simple-table))

(defmethod simple-table-epsilon-closure ((simple-table hash-table))
  (let ((new-simple-table (simple-table-copy simple-table)))
    (loop
       while (simple-table-epsilon-p new-simple-table)
       do (simple-table-expand-epsilon-transitions new-simple-table)
       finally (return new-simple-table))))

(defmethod simple-table-apply-signature-mapping ((simple-table hash-table))
  (let ((new-simple-table (make-empty-simple-table)))
    (maphash 
     (lambda (key value)
       (let ((syms (apply-signature-mapping (car key))))
	 (unless (listp syms)
	   (setf syms (list syms)))
	 (mapc
	  (lambda (newkey)
	    (simple-table-add-modc
	     (cons newkey (cdr key)) value new-simple-table))
	  syms)))
     simple-table)
    new-simple-table))

;; on suppose la table non commutative
(defmethod simple-table-apply-states-mapping ((simple-table hash-table))
  (let ((new-simple-table (make-empty-simple-table)))
    (maphash
     (lambda (key value)
       (let ((newkey (apply-states-mapping key)))
	 (setf (gethash newkey new-simple-table)
	       (target-union
		(apply-states-mapping value)
		(gethash newkey new-simple-table)))))
     simple-table)
    new-simple-table))

(defmethod simple-table-uncommutative ((simple-table hash-table))
;;  (format *error-output* "uncommutative  ~A ~%" simple-table)
  (let ((new-simple-table (make-empty-simple-table)))
    (maphash
     (lambda (key value)
       (let* ((sym (car key))
	      (keys
	       (if (commutative-symbol-p sym)
		   (mapcar (lambda (states)
			     (cons sym states))
			   (permutations (cdr key)))
		   (list key)
		   )))
	 (loop
	    for key in keys
	    do (simple-table-set key value new-simple-table))))
     simple-table)
    new-simple-table))

(defmethod simple-table-to-commutative ((simple-table hash-table))
  (let ((new-simple-table (make-empty-simple-table)))
    (maphash
     (lambda (key value)
       (simple-table-set-modc key value new-simple-table))
     simple-table)
    new-simple-table))

;; uniquement sur table non commutative
(defmethod simple-table-possibly-equivalent-to-state (q qp (simple-table hash-table))
;;  (format *trace-output* "q ~A qp ~A ~%" q qp)
;;  (format *trace-output* "~A possibliy-equivalent-to-state-tt ~A:~% " q qp)
  (maphash
   (lambda (key target)
     (let* ((keyv (coerce key 'vector))
	    (len (arity (aref keyv 0))))
       (loop
	  for i from 1 to len
	  do
	    ;; a cause de l'incompletude on est oblige de considerer les deux cas.
	    (when (or (eq q (aref keyv i)) (eq qp (aref keyv i))) 
	      (if
	       (eq q (aref keyv i))
	       (setf (aref keyv i) qp)
	       (setf (aref keyv i) q))
	      (let* ((keyp (coerce keyv 'list))
		     (targetp (simple-table-access-modc keyp simple-table)))
		(unless
		    (in-the-same-eqclass target targetp)
		  (return-from simple-table-possibly-equivalent-to-state nil)))
	      (if
	       (eq q (aref keyv i))
	       (setf (aref keyv i) qp)
	       (setf (aref keyv i) q))
	      	      ))))
   simple-table)
   t)
