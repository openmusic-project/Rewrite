(in-package :nautowrite)

(defclass sym-transitions (transitions) ())

(defmethod make-sym-transitions ((transitions-table hash-table))
  (make-transitions 'sym-transitions transitions-table))

(defmethod transitions-access ((key list) (sym-transitions sym-transitions))
  (dag-table-access key (transitions-table sym-transitions)))

(defmethod transition-set ((key list) (rh (eql nil)) (sym-transitions sym-transitions)))
  
(defmethod transition-set ((key list) rh (sym-transitions sym-transitions))
;;  (assert (equal key (key-modc (car key) (cdr key))))
  (dag-table-set key rh (transitions-table sym-transitions)))

(defmethod transitions-list ((sym-transitions sym-transitions))
  (dag-table-list (transitions-table sym-transitions)))

(defmethod number-of-transitions ((sym-transitions sym-transitions))
  (dag-table-number-of-transitions (transitions-table sym-transitions)))

(defmethod apply-states-mapping ((sym-transitions sym-transitions))
  (to-commutative
   (make-sym-transitions
    (apply-states-mapping
     (transitions-table
      (uncommutative sym-transitions))))))

(defmethod key-value-list ((sym-transitions sym-transitions))
  (dag-table-key-value-list (transitions-table sym-transitions)))

(defmethod show ((sym-transitions sym-transitions) &optional (stream t))
  (dag-table-show (transitions-table sym-transitions) stream))

(defmethod table-filter-with-states
    ((states ordered-container) (sym-transitions sym-transitions))
  (dag-table-filter-with-states states (transitions-table sym-transitions)))

(defmethod cright-handsides ((sym-transitions sym-transitions))
  (dag-table-cright-handsides (transitions-table sym-transitions)))

(defmethod new-marked ((sym-transitions sym-transitions) (marked container))
  (dag-table-new-marked (transitions-table sym-transitions) marked))

(defmethod uncommutative ((sym-transitions sym-transitions))
;;  (format *error-output* "uncommutative  ~A ~%" sym-transitions)
  (let ((new-dag-table (make-empty-dag-table)))
    (maphash
     (lambda (sym value)
       (setf (gethash sym new-dag-table)
	     (if (commutative-symbol-p sym)
		 (dag-table-uncommutative value)
		 (dag-table-copy value))))
     (transitions-table sym-transitions))
    (make-sym-transitions new-dag-table)))

;; a faire
;; (defmethod nto-commutative ((sym-transitions sym-transitions))

(defmethod to-commutative ((sym-transitions sym-transitions))
  (let ((new-dag-table (make-empty-dag-table)))
    (maphash
     (lambda (sym value)
       (setf (gethash sym new-dag-table)
	     (if (commutative-symbol-p sym)
		 (dag-table-to-commutative value)
		 (dag-table-copy value))))
     (transitions-table sym-transitions))
    (make-sym-transitions new-dag-table)))

(defmethod sym-table-apply-signature-mapping ((sym-table hash-table))
  (let ((new-dag-table (make-empty-dag-table)))
    (maphash
     (lambda (sym value)
       (let ((newsyms (apply-signature-mapping sym)))
	 (mapc
	  (lambda (newsym)
	    (setf (gethash newsym new-dag-table)
		  (dag-table-union value (gethash newsym new-dag-table))))
	  (if (listp newsyms)
	      newsyms
	      (list newsyms)))))
     sym-table)
    new-dag-table))
  
(defmethod table-apply-signature-mapping ((sym-transitions sym-transitions))
  (sym-table-apply-signature-mapping 
   (transitions-table sym-transitions)))

;; uniquement sur tables de transitions non-commutatives
(defmethod possibly-equivalent-to-state (q qp (sym-transitions sym-transitions))
;;  (format *trace-output* "possibliy-equivalent-to-state q ~A qp ~A ~A~%" q qp sym-transitions)
  (labels
      ((compat-states (i n table)
;;	 (format *trace-output*
;;		 "compat-states i=~A n=~A q=~A qp=~A ~%" i n q qp)
	 (cond
	   ((> n i) nil)
	   ((= i n)
	    (let ((target (gethash q table))
		  (targetp (gethash qp table)))
	      (dag-table-equiv-mod-classes target targetp)))
	   (t ;; i > n
	    (maphash
	      (lambda (key value)
		(declare (ignore key))
		(unless (compat-states i (1+ n) value)
		  (return-from compat-states nil)))
	      table)
	    t))))
    (maphash
     (lambda (sym value)
       (dotimes (i (arity sym))
;;	 (format *trace-output* "sym=~A i = ~A ~%" sym i)
	 (unless (compat-states i 0 value)
	   (return-from possibly-equivalent-to-state nil))))
     (transitions-table sym-transitions))
;;    (format *trace-output* "OUI~%")
    t))
  
(defmethod remove-those-with-symbols ((symbols list) (sym-transitions sym-transitions))
  (let ((new-dag-table (make-empty-dag-table)))
    (maphash
     (lambda (sym value)
       (unless (symbol-member sym symbols)
	 (setf (gethash sym new-dag-table)
	       (dag-table-copy value))))
     (transitions-table sym-transitions))
    (make-sym-transitions new-dag-table)))

(defmethod transitions-union ((sym-transitions1 sym-transitions) (sym-transitions2 sym-transitions))
  (make-sym-transitions
   (dag-table-union (transitions-table sym-transitions1) (transitions-table sym-transitions2))))

(defmethod get-states-from ((sym-transitions sym-transitions))
  (dag-table-get-states-from (transitions-table sym-transitions)))

(defmethod deterministic-p ((sym-transitions sym-transitions))
  (dag-table-deterministic-p (transitions-table sym-transitions)))

(defmethod epsilon-p ((sym-transitions sym-transitions))
  (maphash
   (lambda (key value)
     (when (casted-state-p key)
       (return-from epsilon-p (make-transition key (car (dag-table-list value))))))
   (transitions-table sym-transitions)))

(defmethod dead-end-state-p ((state casted-state) (sym-transitions sym-transitions))
  (dag-table-dead-end-state-p state (transitions-table sym-transitions)))

(defmethod uncommutative-for-equivalence-classes ((sym-transitions sym-transitions))
  (uncommutative sym-transitions))
