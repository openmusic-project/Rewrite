(in-package :general)

(defclass casted-mixin ()
  ((%internal-number :reader internal-number)
   (%object :initform nil :initarg :object :accessor object)))

(defmethod compare-object ((o1 casted-mixin) (o2 casted-mixin))
  (eq s1 s2))

(defclass abstract-object () ())

(defun make-empty-object-table ()
  (make-hash-table :test #'equal))

;;; specifique
(defvar *object-table* nil)

(defclass object (abstract-object)
  ((%object-counter :allocation :class :initform 0 :accessor object-counter)
   (%object-table :allocation :class :initform '*object-table*)))

(defmethod object-table ((casted-mixin casted-mixin))
  (eval (slot-value casted-mixin '%object-table)))

(defmethod initialize-instance :after ((casted-mixin casted-mixin) 
				       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value casted-mixin '%internal-number)
	(incf (object-counter casted-mixin))))

(defmethod find-casted-mixin-from-object ((object abstract-object) (l list))
  (car (member object l :key #'object :test #'compare-object)))

(defun find-object-in-object-table (key object-table)
  (gethash key (object-table object-table))

(defun sort-casted-mixins (objects)
  (sort (copy-list objects)
	#'< :key #'internal-number))

(defgeneric object-adjoin (object objects)
  (:documentation "adjoin OBJECT to the list of OBJECTS (no duplicates)"))

(defmethod name ((s abstract-object))
  (strong-name (format nil "~A" s)))

(defun object-p (s)
  (typep s 'object))

(defun abstract-object-p (s)
  (typep s 'abstract-object))

(defun union-sort-casted-mixins (l1 l2)
  (union-sort l1 l2 :key #'internal-number))

(defun intersection-sort-casted-mixins (l1 l2)
  (intersection-sort l1 l2 :key #'internal-number))

(defmethod casted-mixin-adjoin-sort ((s casted-mixin) l)
  (adjoin-sort s l :key #'internal-number))

(defun difference-sort-casted-mixins (l1 l2)
  (difference-sort l1 l2 :key #'object-internal-number))

;; devrait tirer parti du fait que c' est tri'e
(defun remove-sort-casted-mixins (q l)
  (remove q l :key #'object-internal-number))

(defun casted-mixin-p (s)
  (typep s 'casted-mixin))

(defvar *print-object-readably* t)

(defmethod print-object ((casted-object casted-object) stream)
  (format stream "!")
  (if *print-object-readably*
      (format stream "~A" (object casted-object))
      (call-next-method)))

(defgeneric put-object-in-object-table (key casted-mixin))

(defmethod put-object-in-object-table (key (casted-mixin casted-mixin))
 (setf (gethash key (objects-table (object casted-mixin)) casted-mixin)))

(defmethod cast-object ((object abstract-object))
  (let* ((key (let ((*print-object-readably* nil)) (format nil "~A" object)))
	 (casted-object (find-object-in-object-table key (object-table object))))
    (unless casted-object
      (setf casted-object  (make-instance 'casted-object :object object))
      (put-object-in-object-table key casted-object))
    casted-object))

(defmethod cast-object-if-not-casted ((object object))
  (cast-object object))

(defmethod cast-object-if-not-casted ((casted-object casted-object))
  casted-object)

(defmethod object-member ((object object) l)
  (member object l :test #'compare-object))

(defmethod object-member ((casted-object casted-object) l)
  ;; s may be nil returned by compute-object
  (member casted-object l :test #'eq))

(defun casted-object-subsetp (l1 l2)
  (subsetp l1 l2 :test #'eq))

(defun object-remove-duplicates (l)
  (remove-duplicates l :test #'compare-object))

(defun casted-object-remove-duplicates (l)
  (remove-duplicates l :test #'eq))

(defmethod object-adjoin ((s casted-object) l)
  (adjoin s l :test #'eq))

(defun casted-object-intersection (l1 l2)
  (intersection l1 l2 :test #'eq))

(defun casted-object-set-difference (l1 l2)
  (set-difference l1 l2 :test #'eq))

(defun casted-object-nset-difference (l1 l2)
  (nset-difference l1 l2 :test #'eq))

(defmacro with-new-object-table (&body body)
  `(let ((*objects-table* (make-empty-object-table)))
     ,@body))

(defun set-new-object-table ()
  (setf *object-table* (make-empty-object-table)))
