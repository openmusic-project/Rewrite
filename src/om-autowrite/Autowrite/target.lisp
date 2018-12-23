(in-package :nautowrite)

(defmethod make-target ((container container))
  (let ((states (contents container)))
    (if (null (cdr states))
	(car states)
	container)))

(defgeneric target-size (target))

(defmethod target-size ((target (eql nil)))
  0)

;; (defmethod target-size ((target (eql *puits-state*))) ;;; A VOIRq





;;   1)

(defmethod target-size ((target abstract-state))
  1)

(defmethod target-size ((target container))
  (container-size target))

(defmethod target-empty-p (target)
  (zerop (target-size target)))

(defmethod target-union ((target1 container) (target2 container))
  (make-target (container-union target1 target2)))

(defmethod target-union ((target1 casted-state) (target2 casted-state))
  (make-target (container-union target1 target2)))

(defmethod target-union ((target1 abstract-state) (target2 abstract-state))
  (let ((container (make-container (list target1 target2) :clean t)))
    (make-target container)))

(defmethod target-union ((target1 abstract-state) (target2 container))
  (container-adjoin target1 target2))

(defmethod target-union ((target1 container) (target2 abstract-state))
  (container-adjoin target2 target1))

(defmethod target-union ((target1 (eql nil)) (target2 (eql nil)))
  nil)

(defmethod target-union ((target1 (eql nil)) target2)
  target2)

(defmethod target-union (target1 (target2 (eql nil)))
  target1)

(defmethod target-union ((target1 (eql *puits-state*)) (target2 (eql *puits-state*)))
  *puits-state*)

(defmethod target-union ((target1 (eql *puits-state*)) target2)
  (or target2 target1))

(defmethod target-union (target1 (target2 (eql *puits-state*)))
  (or target1 target2))

;;; target-intersection
(defmethod target-intersection ((target1 container) (target2 container))
  (make-target
   (container-intersection target1 target2)))

(defmethod target-intersection ((target1 casted-state) (target2 casted-state))
  (and (eq target1 target2) target1))

(defmethod target-intersection ((target1 abstract-state) (target2 abstract-state))
  (and (compare-object target1 target2) target1))

(defmethod target-intersection ((target1 abstract-state) (target2 container))
  (and (container-member target1 target2) target1))

(defmethod target-intersection ((target1 container) (target2 abstract-state))
  (and (container-member target2 target1) target2))

(defmethod target-intersection ((target1 (eql nil)) (target2 (eql nil)))
  (error "twos NIL in target-intersection"))

(defmethod target-intersection ((target1 (eql nil)) target2)
  (declare (ignore target2))
  nil)

(defmethod target-intersection (target1 (target2 (eql *puits-state*)))
  (declare (ignore target1))
  *puits-state*)

(defmethod target-intersection ((target1 (eql *puits-state*)) (target2 (eql *puits-state*)))
  (error "twos *PUITS-STATE* in target-intersection"))

(defmethod target-intersection ((target1 (eql *puits-state*)) target2)
  (declare (ignore target2))
  *puits-state*)

(defmethod target-intersection (target1 (target2 (eql *puits-state*)))
  (declare (ignore target1))
  *puits-state*)

(defmethod target-product ((rh1 ordered-container) (rh2 ordered-container))
  (make-ordered-container
   (mapcar
    (lambda (state)
      (cast-state (make-tuple-state state)))
    (cartesian-product (list (contents rh1) (contents rh2))))))

(defmethod target-product ((rh1 casted-state) (rh2 ordered-container))
  (make-ordered-container
   (mapcar
    (lambda (state)
      (cast-state (make-tuple-state (list rh1 state))))
    (contents rh2))))
   
(defmethod target-product ((rh1 ordered-container) (rh2 casted-state))
  (make-ordered-container
   (mapcar
    (lambda (state)
      (cast-state (make-tuple-state (list state rh2))))
    (contents rh1))))

(defmethod target-product ((rh1 abstract-state) (rh2 abstract-state))
  (cast-state (make-tuple-state (list rh1 rh2))))

(defmethod targets-product ((targets list))
  (cartesian-product (mapcar #'contents targets)))

(defmethod cast-target ((container container))
  (make-ordered-container (mapcar #'cast-state (contents container))))

(defmethod cast-target ((state abstract-state)) (cast-state state))

(defmethod cast-target ((target (eql nil)))
  nil)

(defmethod casted-target-p ((state (eql nil))) nil)
(defmethod casted-target-p ((state abstract-state)) nil)
(defmethod casted-target-p ((state container)) nil)
(defmethod casted-target-p ((state ordered-container)) t)
(defmethod casted-target-p ((state casted-state)) t)

(defmethod target-deterministic-p ((target casted-state))
  (declare (ignore target))
  t)

(defmethod target-deterministic-p ((target container))
  (null (cdr (contents target))))

(defmethod index-target ((container ordered-container) (index integer))
  (make-ordered-container
   (mapcar
    (lambda (state) (index-state state index))
    (contents container))))

(defmethod index-target ((container container) (index integer))
  (make-container
   (mapcar
    (lambda (state) (index-state state index))
    (contents container))))

(defmethod index-target ((state abstract-state) (index integer))
  (index-state state index))

(defmethod index-target ((target (eql nil)) (index integer))
  nil)
