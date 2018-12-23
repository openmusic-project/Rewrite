(in-package :nautowrite)

(defun casted-state-or-container-p (target)
  (or (casted-state-p target) (ordered-container-p target)))

(defun make-empty-dag-table ()
  (make-hash-table :test #'eq))

(defmethod dag-table-access ((key list) (dag-table hash-table))
  (loop
     for e in key
     and target = dag-table then (gethash e  target)
     unless target
     do (return)
       finally (return (gethash e target))))

(defmethod dag-table-set ((key list) (rh (eql nil)) (dag-table hash-table))
  )

(defmethod dag-table-set ((key list) rh (dag-table hash-table))
;;  (assert key)
;;  (when (typep (car key) 'arity-symbol)
;;    (print key)
;;    (assert (equal key (key-modc (car key) (cdr key)))))
  (let ((state (car key)))
    (if (null (cdr key))
	(setf (gethash state dag-table) rh)
	(let ((target (gethash state dag-table)))
	  (unless target
	    (setf target (setf (gethash state dag-table) (make-empty-dag-table))))
	  (dag-table-set (cdr key) rh target))))
  dag-table)

(defmethod dag-table-list ((dag-table hash-table))
  (let ((acc '()))
    (labels ((aux (target rfts)
	       ;;	       (format t "target ~A rfts ~A~%" target rfts)
	       (if (casted-state-or-container-p target)
		   (let ((lh (flat-term-from-key (reverse rfts))))
		     (mapcar
		      (lambda (q)
			(push (make-transition lh q) acc))
		      (contents target)))
		   (maphash
		    (lambda (key value)
		      (aux value (cons key rfts)))
		    target))))
      (aux dag-table ())
      acc)))

(defmethod dag-table-number-of-transitions ((dag-table hash-table))
  (let ((n 0))
    (labels
	((aux (target)
	   (if (casted-state-or-container-p target)
	       (incf n (length (contents target)))
	       (maphash
		(lambda (key value)
		  (declare (ignore key))
		  (aux value))
		target))))
      (aux dag-table))
    n))

(defmethod dag-table-copy ((target casted-state))
  target)

(defmethod dag-table-copy ((target ordered-container))
  target)

(defmethod dag-table-copy ((dag-table hash-table))
  (let ((new-dt (make-empty-dag-table)))
    (maphash
     (lambda (key target)
       (setf (gethash key new-dt) (dag-table-copy target)))
     dag-table)
    new-dt))

(defmethod dag-table-union (target1 target2)
  (target-union target1 target2))

(defmethod dag-table-union ((dag-table1 hash-table) (dag-table2 hash-table))
  (let ((new-dt (dag-table-copy dag-table1)))
    (maphash
     (lambda (key value)
       (setf (gethash key new-dt)
	     (dag-table-union (dag-table-copy value) (gethash key new-dt))))
     dag-table2)
    new-dt))

(defmethod apply-states-mapping ((dag-table hash-table))
;;  (format *trace-output* "apply-states-mapping to dag-table dag-table~%")
  (let ((new-dag-table (make-empty-dag-table)))
    (maphash
     (lambda (key value)
;;       (format t "key ~A value ~A ~%" key value)
       (let ((newkey (apply-states-mapping key)))
	 (setf (gethash  newkey new-dag-table)
	       (dag-table-union
		(apply-states-mapping value)
		(gethash newkey new-dag-table)))))
     dag-table)
    new-dag-table))

(defmethod dag-table-cright-handsides ((dag-table hash-table))
  (let ((crhs (make-empty-ordered-container)))
    (labels ((aux (table)
	       (maphash
		(lambda (k value)
		  (declare (ignore k))
		  (unless (null value)
		    (if (casted-state-or-container-p value)
			(mapc (lambda (state)
				(setf crhs (container-adjoin state crhs)))
			      (contents value))
			(aux value))))
		table)))
      (aux dag-table))
    crhs))

(defmethod dag-table-key-value-list ((dag-table hash-table))
  (dag-table-things-from
   dag-table
   (lambda (target tuple)
     (list (reverse tuple) target))
   #'cons))

(defmethod dag-table-show ((dag-table hash-table) &optional (stream t))
  (labels
      ((aux (target tuple)
	 (if (casted-state-or-container-p target)
	     (format
	      stream
	      "~A -> ~A~%"
	      (flat-term-from-key (reverse tuple)) target)
	     (maphash
	      (lambda (key value)
		(aux value (cons key tuple)))
	      target))))
    (aux dag-table ())))

(defmethod dag-table-filter-with-states ((states ordered-container) (dag-table hash-table))
  (let ((new-dt (make-empty-dag-table)))
    (labels
	((aux (target tuple)
	   (if (casted-state-or-container-p target)
	       (progn
		 (when (has-only-marked-states target states)
		   (dag-table-set (reverse tuple) target new-dt)))
	       (maphash
		(lambda (key value)
;;		 (format *error-output* "aux ~A ~A~%" key states)
		  (when (has-only-marked-states key states)
		    (aux value (cons key tuple))))
		target))))
      (aux dag-table ()))
    new-dt))

(defmethod dag-table-new-marked ((dag-table hash-table) (marked ordered-container))
  (let ((tomark (make-empty-ordered-container)))
    (labels
	((aux (target rfts)
	   (if (casted-state-or-container-p target)
	       (let ((key (reverse rfts)))
		 (when (has-only-marked-states key marked)
		   (setf tomark (target-union tomark target))))
	       (maphash
		(lambda (key value)
		  (aux value (cons key rfts)))
		target))))
      (aux dag-table ()))
    tomark))

(defmethod dag-table-tuples-rh ((dag-table hash-table))
  (let ((tuples-rh '()))
    (labels ((aux (target tuple)
	       (if (casted-state-or-container-p target)
		   (push (list (reverse tuple) target) tuples-rh)
		   (maphash
		    (lambda (key value) (aux value (cons key tuple)))
		    target))))
      (aux dag-table ())
      tuples-rh)))

;; (f target rtuple)

(defmethod dag-table-things-from ((dag-table hash-table) f &optional (op #'cons))
  (let ((things '()))
    (labels ((aux (target rtuple)
	       (if (casted-state-or-container-p target)
		   (setf things (funcall op (funcall f target rtuple) things))
		   (maphash
		    (lambda (key value) (aux value (cons key rtuple)))
		    target))))
      (aux dag-table ())
      things)))

(defmethod dag-table-get-states-from ((dag-table hash-table))
  (let ((container (make-empty-ordered-container)))
    (labels ((aux (target)
	       (if (casted-state-or-container-p target)
		   (loop
		      for casted-state in (contents target)
		      do (setf container (container-adjoin casted-state container)))
		   (maphash
		    (lambda (key value)
		      (declare (ignore key))
		      (aux value))
		    target))))
      (aux dag-table))
    container))

(defmethod dag-table-tuples-rh ((dag-table hash-table))
  (dag-table-things-from
   dag-table
   (lambda (target rtuple) (list (reverse rtuple) target))))

(defmethod dag-table-tuples-from ((dag-table hash-table))
   (dag-table-things-from
    dag-table
    (lambda (target rtuple) (declare (ignore target)) (reverse rtuple))))
 
(defmethod dag-table-uncommutative ((dag-table casted-state))
  dag-table)

(defmethod dag-table-uncommutative ((dag-table ordered-container))
  dag-table)

(defmethod dag-table-uncommutative ((dag-table hash-table))
  (let ((new-dt (make-empty-dag-table)))
    (mapc
     (lambda (tuple-rh)
;;       (format *trace-output* "~A~%" tuple-rh)
       (dag-table-set (first tuple-rh) (second tuple-rh) new-dt))
     (mapcan (lambda (tuple-rh)
	      (mapcar (lambda (states)
			(list states (second tuple-rh)))
		      (permutations (first tuple-rh))))
	     (dag-table-tuples-rh dag-table)))
    new-dt))

(defmethod dag-table-to-commutative ((dag-table hash-table))
  (let ((new-dt (make-empty-dag-table)))
    (mapc
     (lambda (tuple-rh)
       (dag-table-set (sort-casted-states (copy-list (first tuple-rh))) (second tuple-rh) new-dt))
     (dag-table-tuples-rh dag-table))
    new-dt))


(defmethod dag-table-deterministic-p (target)
  (target-deterministic-p target))

(defmethod  dag-table-deterministic-p ((dag-table hash-table))
  (maphash
   (lambda (key value)
     (declare (ignore key))
     (unless (dag-table-deterministic-p value)
       (return-from dag-table-deterministic-p)))
   dag-table)
  t)

(defmethod dag-table-dead-end-state-p ((state casted-state) (dag-table hash-table))
  (let ((tuples-rh
	 (dag-table-things-from
	  dag-table
	  (lambda (target tuple)
	    (list (butlast tuple) target))
	  #'cons)))
    (notany
     (lambda (tuple-rh)
       (and (not (eq state (second tuple-rh)))
	    (state-member state (car tuple-rh))))
     tuples-rh)))


(defgeneric dag-table-equiv-mod-classes (tt1 tt2))

(defmethod dag-table-equiv-mod-classes ((dt1 (eql nil)) (dt2 (eql nil)))
  t)

(defmethod dag-table-equiv-mod-classes ((dag-table1 (eql nil)) dag-table2)
  nil)

(defmethod dag-table-equiv-mod-classes (dag-table1 (dag-table2 (eql nil)))
  nil)

(defmethod dag-table-equiv-mod-classes (dag-table1 dag-table2)
  (when (casted-state-or-container-p dag-table1)
    (return-from dag-table-equiv-mod-classes
      (in-the-same-eqclass
       (car (contents dag-table1))
       (car (contents dag-table2)))))
  (maphash
   (lambda (key value)
     (let ((value2 (gethash key dag-table2)))
       (unless (and value2 (dag-table-equiv-mod-classes value value2))
	 (return-from dag-table-equiv-mod-classes nil))))
   dag-table1)
  t)

(defmethod dag-table-product ((dag-table1 hash-table) (dag-table2 hash-table))
  (let* ((new-dag-table (make-empty-dag-table))
	 (tuples-rh1 (dag-table-tuples-rh dag-table1))
	 (tuples-rh2 (dag-table-tuples-rh dag-table2)))
    (mapc
     (lambda (pair)
       (let* ((tuple-rh1 (first pair))
	      (tuple-rh2 (second pair))
	      (tuple1 (first tuple-rh1))
	      (tuple2 (first tuple-rh2))
	      (rh1 (second tuple-rh1))
	      (rh2 (second tuple-rh2)))
;;	 (format *trace-output* "rh1 ~A ~A, rh2 ~A ~A~%" rh1 (type-of rh1) rh2 (type-of rh2))
	 (dag-table-set
	  (mapcar (lambda (s1 s2) (cast-state (make-tuple-state (list s1 s2)))) tuple1 tuple2)
	  (target-product rh1 rh2)
	  new-dag-table)))
     (cartesian-product (list tuples-rh1 tuples-rh2)))
    new-dag-table))


