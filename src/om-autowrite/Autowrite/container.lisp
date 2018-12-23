(in-package :nautowrite)

(defgeneric contents (container))

(defclass container ()
  ((contents :initform '() :initarg :contents :reader contents))
  (:documentation "container of sorted states"))

(defclass ordered-container (container) ())

(defmethod contents ((abstract-state abstract-state))
;;  (format *error-output* "contents state Warning à supprimer ")
  (list abstract-state))

(defmethod contents ((target (eql nil))) '())

(defmethod print-object :before ((container ordered-container) stream)
  (format stream "o"))

(defmethod print-object ((container container) stream)
  (format stream "{")
  (display-sequence (contents container) stream)
  (format stream "}"))

(defgeneric make-container (states &key clean))

(defmethod make-container ((states list) &key (clean t))
  (assert (every #'state-p states))
  (assert (notany #'casted-state-p states))
  (setf states (copy-list states)) ;; proper list
  (when clean
    (setf states (state-remove-duplicates states)))
  (make-instance 'container
		 :contents states))

(defmethod make-ordered-container ((states list) &key (clean t) (sort t))
  (assert (every #'casted-state-p states))
  (setf states (copy-list states)) ;; proper list
  (when clean
    (setf states (casted-state-remove-duplicates states)))
  (when sort
    (setf states (sort-casted-states states)))
  (make-instance 'ordered-container :contents states))

;; comme sort-casted-states fait un copy-list
;; les containers ont proper liste
(defun make-empty-ordered-container ()
  (make-ordered-container '() :clean nil :sort nil))

(defun make-empty-container ()
  (make-container '() :clean nil))

(defmethod make-container-from-state ((state abstract-state))
  (make-container (list state) :clean nil))

(defmethod make-container-from-state ((state casted-state))
  (make-ordered-container (list state) :clean nil :sort nil))

(defmethod compare-object ((container1 container) (container2 container))
  (or (eq container1 container2)
      (let ((states1 (contents container1))
	    (states2 (contents container2)))
	(and (subsetp states1 states2 :test #'compare-object)
	     (subsetp states2 states1 :test #'compare-object)))))

(defmethod compare-object ((container1 ordered-container) (container2 ordered-container))
  (or (eq container1 container2)
      (let ((states1 (contents container1))
	    (states2 (contents container2)))
	(ordered-equal states1 states2))))

(defun container-p (s) (typep s 'container))
(defun ordered-container-p (s) (typep s 'ordered-container))

(defgeneric container-member (state container))
(defgeneric container-size (container))

(defmethod container-size ((container container))
  (length (contents container)))

(defmethod container-member ((state casted-state) (container ordered-container))
  (member-sort state (contents container) :key #'state-internal-number))

(defmethod container-member ((state abstract-state) (container container))
  (member state (contents container) :test #'compare-object))

(defgeneric container-adjoin (state container))

(defmethod container-adjoin ((state casted-state) (container ordered-container))
  (make-ordered-container
   (casted-state-adjoin-sort state (contents container))
   :sort nil :clean nil))

(defmethod container-adjoin ((state abstract-state) (container container))
  (make-container
   (adjoin state (contents container) :test #'compare-object)))

(defgeneric empty-container-p (container))

(defmethod empty-container-p ((container container))
  (endp (contents container)))

(defgeneric empty-container-p (container))

(defmethod empty-container-p ((container container))
  (endp (contents container)))

(defgeneric singleton-container-p (container))

(defmethod singleton-container-p ((container container))
  (let ((contents (contents container)))
    (and contents (null (cdr contents)))))

(defgeneric merge-containers (containers))

(defmethod merge-containers (containers)
  (if (null (cdr containers))
      (car containers)
      (reduce #'container-union containers)))

(defmethod container-difference ((container1 container) (container2 container))
  (make-container
   (set-difference (contents container1) (contents container2) :test #'compare-object)))

(defmethod container-difference ((container1 ordered-container) (container2 ordered-container))
  (make-ordered-container
   (difference-sort-casted-states (contents container1) (contents container2))
   :sort nil
   :clean nil))

(defmethod container-difference ((c1 ordered-container) (c2 casted-state))
  (make-ordered-container
   (remove-sort-casted-states c2 c1)
   :sort nil
   :clean nil))

(defmethod container-difference ((c1 casted-state) (c2 ordered-container))
  (make-ordered-container
   (remove-sort-casted-states c1 c2)
   :sort nil
   :clean nil))

(defmethod container-difference ((c1 casted-state) (c2 casted-state))
  (if (eq c1 c2)
      (make-empty-ordered-container)
      c1))

(defmethod container-intersection ((container1 container) (container2 container))
  (make-container
   (intersection (contents container1) (contents container2) :test #'compare-object)
   :clean nil))

(defmethod container-intersection ((container1 ordered-container) (container2 ordered-container))
  (make-ordered-container
   (intersection-sort-casted-states (contents container1)(contents container2))
   :clean nil
   :sort nil))

(defmethod container-intersection ((c1 casted-state) (c2 ordered-container))
  (if (container-member c1 c2)
      c2
      (make-empty-ordered-container)))

(defmethod container-intersection ((c1 ordered-container) (c2 casted-state))
  (if (container-member c2 c1)
      c1
      (make-empty-ordered-container)))

(defmethod container-intersection ((c1 casted-state) (c2 casted-state))
  (if (eq c1 c2)
      c1
      (make-empty-ordered-container)))

(defmethod container-union ((container1 container) (container2 container))
  (make-container
   (union (contents container1) (contents container2) :test #'compare-object)
   :clean nil))

(defmethod container-union ((c1 casted-state) (c2 casted-state))
  (make-ordered-container
   (list c1 c2)))

(defmethod container-union ((container1 ordered-container) (container2 ordered-container))
  (make-ordered-container
   (union-sort-casted-states (contents container1) (contents container2))
   :sort nil
   :clean nil))

(defmethod container-nunion ((container1 ordered-container) (container2 ordered-container))
  (setf (slot-value container1 'contents)
	(union-sort-casted-states (contents container1) (contents container2))))

(defmethod container-nunion ((container1 ordered-container) (c2 casted-state))
  (setf (slot-value container1 'contents)
	(casted-state-adjoin-sort c2 (contents container1))))

(defmethod container-subset-p ((container1 container) (container2 container))
  (empty-container-p (container-difference container1 container2)))

(defmethod container-equal-p ((container1 container) (container2 container))
  (and
   (container-subset-p container1 container2)
   (container-subset-p container2 container1)))

(defmethod container-singleton-p ((container container))
  (null (cdr (contents container))))

(defmethod containers-product ((containers list))
  (cartesian-product (mapcar #'contents containers)))

(defmethod container-remove ((state abstract-state) (container container))
  (make-container
   (remove state (contents container) :test #'compare-object) :clean nil))

(defmethod container-remove ((casted-state casted-state) (container ordered-container))
  (make-ordered-container
   (difference-sort-casted-states (contents container) (list casted-state)) :clean nil :sort nil))

(defmethod container-remove-if-not (fun (container ordered-container))
  (make-ordered-container
   (remove-if-not fun (contents container))
   :clean nil
   :sort nil))

(defmethod container-remove-if (fun (container ordered-container))
  (make-ordered-container
   (remove-if fun (contents container))
   :clean nil
   :sort nil))

(defmethod container-remove-if-not (fun (container ordered-container))
  (make-ordered-container
   (remove-if-not fun (contents container))
   :clean nil))

(defmethod find-casted-state-from-state ((state abstract-state) (l container))
  (find-casted-state-from-state state (contents l)))
