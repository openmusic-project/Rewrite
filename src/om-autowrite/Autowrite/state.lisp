(in-package :nautowrite)

(defgeneric state-size (state))

(defvar *state-internal-number* 0)
(defvar *print-states-readably* t)
(defvar *states-index* nil)

(defmethod state-size ((abstract-state abstract-state))
  1)

(defmethod state-size ((l list))
  (loop
    for s in l
    count (state-size s)))

(defclass indexed-state (abstract-state)
  ((state :initform nil :initarg :state :reader state)
   (state-index :initform nil :initarg :state-index :accessor state-index)))

(defmethod print-object ((indexed-state indexed-state) stream)
  (format stream "~A-~A" (state indexed-state) (state-index indexed-state)))

(defmethod compare-object ((s1 indexed-state) (s2 indexed-state))
  (and
   (= (state-index s1) (state-index s2))
   (compare-object (state s1)  (state s2))))

(defmethod make-indexed-state ((state abstract-state) index)
  (make-instance 'indexed-state :state state :state-index index))

(defmethod index-state ((state abstract-state) index)
  (make-indexed-state state index))

(defmethod state ((state abstract-state))
  (warn "state applied on uncasted state")
  state)

(defclass casted-state (abstract-state)
  ((internal-number
    :initform (incf *state-internal-number*)
    :reader state-internal-number)
   (state :initform nil :initarg :state :accessor state)
   (state-number :initform nil :initarg :state-number :accessor state-number)))

(defmethod state-size ((s casted-state))
  (+ (state-size (state s)) 2))

(defmethod compare-object ((s1 casted-state) (s2 casted-state))
  (eq s1 s2))

(defmethod nindex-state ((casted-state casted-state) index)
  (setf (state casted-state)
	(make-indexed-state (state casted-state) index)))

(defun sort-casted-states (states)
  (sort (copy-list states) #'< :key #'state-internal-number))

(defgeneric state-adjoin (state states)
  (:documentation "adjoin STATE to the list of states STATES (no duplicates)"))

(defun state-p (s)
  (typep s 'abstract-state))

(defun union-sort-casted-states (l1 l2)
  (union-sort l1 l2 :key #'state-internal-number))

(defun intersection-sort-casted-states (l1 l2)
  (intersection-sort l1 l2 :key #'state-internal-number))

(defmethod casted-state-adjoin-sort ((s casted-state) l)
  (adjoin-sort s l :key #'state-internal-number))

(defun difference-sort-casted-states (l1 l2)
  (difference-sort l1 l2 :key #'state-internal-number))

;; devrait tirer parti du fait que c' est tri'e
(defun remove-sort-casted-states (q l)
  (remove q l :key #'state-internal-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; casted-states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *automaton-states-table* nil)

(defgeneric state (casted-state)
  (:documentation "state of a casted state"))

(defun casted-state-p (s)
  (typep s 'casted-state))

;; (defmethod print-object :around ((casted-state casted-state) stream)
;;   (format stream "~A" (state-internal-number casted-state)))

(defmethod print-object :before ((casted-state casted-state) stream)
  (format stream "!")
  (let ((state (state casted-state)))
  (when (and (typep state 'indexed-state) (state-index state))
    (format stream "~A" (state-index state)))))

;; (defmethod print-object :after ((casted-state casted-state) stream)
;;  (format stream "~A" (state-internal-number casted-state)))

(defmethod print-object ((casted-state casted-state) stream)
  (if *print-states-readably*
      (if (state-number casted-state)
	  (format stream "[~A]" (state-number casted-state))
	  (format stream "~A" (state casted-state)))
      (format stream "~A" (state-internal-number casted-state))))

(defmethod find-casted-state-from-state ((state abstract-state) (l list))
  (car (member state l :key #'state :test #'compare-object)))

(defun make-empty-automaton-states-table ()
  (make-hash-table :test #'equal))

(defun find-state-in-automaton-states-table (key automaton-states-table)
 (gethash key automaton-states-table))

(defun automaton-states-table-count ()
  (hash-table-count *automaton-states-table*))

(defgeneric put-state-in-automaton-states-table (key casted-state))

(defmethod put-state-in-automaton-states-table (key (casted-state casted-state))
 (setf (gethash key *automaton-states-table*) casted-state))

(defun make-casted-state (state)
  (make-instance 'casted-state :state state))

(defmethod state-key ((state abstract-state))
  (let ((*print-states-readably* nil))
    (format nil "~A" state)))

(defmethod cast-state ((state abstract-state))
  (let* ((key (state-key state))
	 (casted-state (find-state-in-automaton-states-table key *automaton-states-table*)))
    (unless casted-state
      (setf casted-state (make-casted-state state))
      (put-state-in-automaton-states-table key casted-state))
    casted-state))

;; attention en rajoutant ca on a fait heriter casted-state de state au lieu de absctrat state
(defmethod cast-state ((casted-state casted-state))
  (warn "it is stupid to cast a casted state")
  (call-next-method))

(defmethod state-member ((state abstract-state) l)
  (member state l :test #'compare-object))

(defmethod state-member ((casted-state casted-state) l)
  ;; s may be nil returned by compute-state
  (member casted-state l :test #'eq))

(defun casted-state-subsetp (l1 l2)
  (subsetp l1 l2 :test #'eq))

(defun state-remove-duplicates (l)
  (remove-duplicates l :test #'compare-object))

(defun casted-state-remove-duplicates (l)
  (remove-duplicates l :test #'eq))

(defmethod state-adjoin ((s casted-state) l)
  (adjoin s l :test #'eq))

(defun casted-state-intersection (l1 l2)
  (intersection l1 l2 :test #'eq))

(defun casted-state-set-difference (l1 l2)
  (set-difference l1 l2 :test #'eq))

(defun casted-state-nset-difference (l1 l2)
  (nset-difference l1 l2 :test #'eq))

(defmacro with-states-table (transitions &body body)
  `(let ((*automaton-states-table* (states-table ,transitions)))
     ,@body))

(defclass neutral-state (abstract-state) ())
(defclass final-neutral-state (neutral-state) ())
(defclass ok-state (abstract-state) ()) ;; oplus(Ok,q) -> Ok forall q not in { Error, Ok}
                               ;; oplus(Ok,Ok) -> Error

(defclass ok-ok-state (abstract-state) ()) ;; oplus(Okk,q) -> Okk forall q not in { Error, Okk}
                                  ;; oplus(Okk,Okk) -> Okk

(defclass puits-state (abstract-state) ())

(defmethod print-object ((state ok-state) stream)
  (format stream "Ok"))

(defmethod print-object ((state ok-ok-state) stream)
  (format stream "Okk"))

(defmethod print-object ((state puits-state) stream)
  (format stream "Error"))

(defmethod print-object ((state neutral-state) stream)
  (format stream "#"))

(defmethod print-object ((state final-neutral-state) stream)
  (format stream "#f"))

(defvar *neutral-state-final-p* nil)
(defvar *neutral-state* (make-instance 'neutral-state))
(defvar *final-neutral-state* (make-instance 'final-neutral-state))
(defvar *puits-state* (make-instance 'puits-state))
(defvar *ok-state* (make-instance 'ok-state))
(defvar *ok-ok-state* (make-instance 'ok-ok-state))

(defgeneric strictly-ordered-state-p (state1 state2))

(defmethod strictly-ordered-state-p ((state1 puits-state) state2) t)
(defmethod strictly-ordered-state-p (state1 (state2 puits-state)) nil)

 (defmethod strictly-ordered-state-p (state1 state2)
  (warn "strictly-ordered-state-p ~A ~A ~%" (type-of state1) (type-of state2))
  nil)

(defmethod order-state ((go1 abstract-state) (go2 abstract-state))
  (if (or (compare-object go1 go2) (strictly-ordered-state-p go1 go2))
      (list go1 go2)
      (list go2 go1)))

(defgeneric state-final-p (state))
(defmethod state-final-p ((s abstract-state))  nil)
(defmethod state-final-p ((o (eql nil)))  nil)
(defmethod state-final-p ((state ok-state)) t)
(defmethod state-final-p ((state ok-ok-state)) t)
(defmethod state-final-p ((puits-state puits-state)) nil)
(defmethod state-final-p ((s neutral-state))  nil)
(defmethod state-final-p ((s final-neutral-state)) t)

(defgeneric state-success-p (state))
(defmethod state-success-p ((s abstract-state))  nil)
(defmethod state-success-p ((o (eql nil)))  nil)

(defgeneric state-puits-p (state))

(defmethod state-puits-p ((s abstract-state))  nil)
(defmethod state-puits-p ((state (eql nil)))  t)
(defmethod state-puits-p ((state (eql *puits-state*)))  t)

