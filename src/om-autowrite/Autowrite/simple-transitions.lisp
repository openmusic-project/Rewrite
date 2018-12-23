(in-package :nautowrite)

(defclass simple-transitions (transitions) ())

(defmethod make-empty-simple-transitions ()
  (make-empty-transitions 'simple-transitions))

(defmethod make-simple-transitions ((transitions-table hash-table))
  (make-transitions 'simple-transitions transitions-table))

(defmethod transitions-access ((key list) (simple-transitions simple-transitions))
  (simple-table-access key (transitions-table simple-transitions)))

(defmethod transitions-list ((st-table simple-transitions))
  (simple-table-list (transitions-table st-table)))
	     
(defmethod transition-set ((key list) rh (simple-transitions simple-transitions))
  (simple-table-set key rh (transitions-table simple-transitions)))

(defmethod number-of-transitions ((simple-transitions simple-transitions))
  (simple-table-number-of-transitions (transitions-table simple-transitions)))

(defmethod key-value-list ((simple-transitions simple-transitions))
  (simple-table-key-value-list (transitions-table simple-transitions)))

(defmethod show ((simple-transitions simple-transitions) &optional (stream t))
  (simple-table-show (transitions-table simple-transitions) stream))

(defmethod table-filter-with-states
    ((accessible-states ordered-container) (simple-transitions simple-transitions))
  (simple-table-filter-with-states accessible-states (transitions-table simple-transitions)))

(defmethod new-marked ((simple-transitions simple-transitions) (marked container))
  (let ((new-marked (make-empty-ordered-container)))
    (maphash
     (lambda (key value)
       (when (has-only-marked-states key marked)
	 (container-nunion new-marked value)))
     (transitions-table simple-transitions))
    new-marked))

(defmethod remove-those-with-symbols ((symbols list) (simple-transitions simple-transitions))
  (let ((new-simple-transitions (make-empty-simple-transitions)))
    (maphash
     (lambda (key value)
       (when (or (casted-state-p key) (not (symbol-member (car key) symbols)))
	 (transition-set key value new-simple-transitions)))
     (transitions-table simple-transitions))
    new-simple-transitions))

(defmethod cright-handsides ((simple-transitions simple-transitions))
  (simple-table-cright-handsides (transitions-table simple-transitions)))

(defmethod deterministic-p ((simple-transitions simple-transitions))
  (simple-table-deterministic-p (transitions-table simple-transitions)))

(defmethod dead-end-state-p ((state casted-state) (simple-transitions simple-transitions))
  (simple-table-dead-end-state-p state (transitions-table simple-transitions)))

(defmethod union-simple-transitions ((target1 simple-transitions) (target2 simple-transitions))
  (make-simple-transitions
   (simple-table-union
    (transitions-table target1) (transitions-table target2))))

(defmethod get-states-from ((simple-transitions simple-transitions))
  (simple-table-get-states-from (transitions-table simple-transitions)))

(defmethod apply-states-mapping ((simple-transitions simple-transitions))
  (make-simple-transitions 
   (simple-table-to-commutative
    (simple-table-apply-states-mapping
     (transitions-table (uncommutative simple-transitions))))))

(defmethod epsilon-p ((simple-transitions simple-transitions))
  (simple-table-epsilon-p (transitions-table simple-transitions)))

(defmethod epsilon-closure ((simple-transitions simple-transitions))
  (make-simple-transitions
   (simple-table-epsilon-closure (transitions-table simple-transitions))))

(defmethod table-apply-signature-mapping ((simple-transitions simple-transitions))
  (simple-table-apply-signature-mapping (transitions-table simple-transitions)))

(defmethod uncommutative ((simple-transitions simple-transitions))
;;  (format *trace-output* "on ne uncomutative pas une simple-transitions?~%" )
  (make-simple-transitions
   (simple-table-uncommutative (transitions-table simple-transitions))))
  
(defmethod to-commutative ((simple-transitions simple-transitions))
  (make-simple-transitions
   (simple-table-to-commutative (transitions-table simple-transitions))))

(defmethod possibly-equivalent-to-state (q qp (simple-transitions simple-transitions))
  (simple-table-possibly-equivalent-to-state q qp (transitions-table simple-transitions)))

(defmethod uncommutative-for-equivalence-classes ((simple-transitions simple-transitions))
  simple-transitions)