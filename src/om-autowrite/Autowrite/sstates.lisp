(in-package :nautowrite)
(defvar *with-bv* t)

(defmethod make-sstates ((states list))
  (if *with-bv*
      (states-to-bv states)
      (make-ordered-container states)))

(defmethod make-sstates ((state casted-state))
  (make-sstates (list state)))

(defmethod make-sstates ((states ordered-container))
  (if *with-bv*
      (states-to-bv (contents states))
      states))

(defmethod make-sstates ((states bit-vector))
  (if *with-bv*
      states
      (sstates-to-container states)))

(defmethod make-sstates-from-state ((q integer))
  (istate-to-bv q))

(defmethod make-sstates-from-state ((q casted-state))
  (make-container-from-state q))

(defmethod empty-sstates-p ((container ordered-container))
  (empty-container-p container))

(defmethod empty-sstates-p ((sstates (eql nil)))
  t)
    
(defmethod empty-sstates-p ((sstates casted-state))
  (declare (ignore sstates))
  t)
    
(defmethod empty-sstates-p ((bit-vector bit-vector))
  (empty-bv-p bit-vector))

(defmethod in-sstates ((q casted-state) (sstates ordered-container))
  (container-member q sstates))

(defmethod in-sstates ((q casted-state) (sstates bit-vector))
  (not (zerop (aref sstates (state-number q)))))

(defmethod sstates-adjoin ((q casted-state) (sstates ordered-container))
  (container-adjoin q sstates))

(defmethod sstates-adjoin ((q integer) (sstates bit-vector))
  (let ((ss (make-sstates sstates)))
    (setf (aref ss q) 1)
    ss))

(defmethod sstates-contents ((sstates ordered-container))
  (contents sstates))

(defmethod sstates-cardinality ((sstates bit-vector))
  (count 1 sstates))

(defmethod sstates-cardinality ((sstates ordered-container))
  (length (contents sstates)))

(defmethod sstates-singleton-p ((sstates bit-vector))
  (= 1 (sstates-cardinality sstates)))

(defmethod sstates-singleton-p ((sstates ordered-container))
  (= 1 (container-size sstates)))

(defmethod sstates-contents ((sstates bit-vector))
  (loop
     for i from 0 below *states-vector-len*
     unless (zerop (aref sstates i))
     collect i))

(defmethod sstates-to-container ((sstates ordered-container))
  sstates)

(defun print-sstates (sstates &optional (stream t))
  (format stream "~A " (sstates-to-container sstates)))

(defmethod sstates-to-container ((sstates bit-vector))
  (make-ordered-container (expand-bv sstates)))

(defmethod sstates-realstate ((q integer))
  (aref *states-vector* q))

(defmethod sstates-realstate ((q casted-state)) q)

(defmethod sstates-difference ((sstates1 ordered-container) (sstates2 ordered-container))
  (container-difference sstates1 sstates2))

(defmethod sstates-difference ((sstates1 bit-vector) (sstates2 bit-vector))
  (bit-andc2  sstates1 sstates2))

