(in-package :nautowrite)

(defvar *st-reducible* nil)
(defvar *cstates* nil)
(defvar *st-redex* nil)

(defclass dstate (abstract-state)
  ((s-comps :allocation :class :accessor s-comps)
   (b-comps :allocation :class :accessor b-comps)
   (p-comps :allocation :class :accessor p-comps)
   (sc :initarg :sc :reader s-comp)
   (bc :initarg :bc :reader b-comp)
   (pc :initarg :pc :reader p-comp)))

(defun init-dstates ()
  (let ((s (make-instance 'dstate)))
    (setf (s-comps s) nil (b-comps s) nil (p-comps s) nil)))

(defun make-b-comp (b)
  (let ((s (make-instance 'dstate)))
    (or (car (member b (b-comps s) :test #'compare-object))
	(prog1 b
	  (push b (b-comps s))))))

(defun make-s-comp (s &key deterministic)
  (if deterministic
      s
      (let ((ts (make-instance 'dstate)))
	(or (car (member s (s-comps ts) :test #'compare-object))
	  (prog1 s (push s (s-comps ts)))))))

(defun make-p-comp (p)
  (and p
       (let ((s (make-instance 'dstate)))
	 (or (car (member p (p-comps s) :test #'compare-object))
	     (prog1 p (push p (p-comps s)))))))

(defun make-dstate (sc bc pc &key (deterministic t))
  (when (typep sc 'ordered-container)
    (assert (cdr (contents sc))))
  (let* ((s (make-s-comp sc :deterministic deterministic))
	 (b (make-b-comp bc))
	 (p (make-p-comp pc)))
    (make-instance 'dstate :sc s :bc b :pc p)))

(defmethod print-object ((dstate dstate) stream)
  (format stream "[")
  (if (listp (s-comp dstate))
      (display-sequence (s-comp dstate) stream)
      (print-object  (s-comp dstate) stream))
  (format stream " : ~A : ~A" (b-comp dstate) (p-comp dstate))
  (format stream "]"))


(defmethod compare-object ((d1 dstate) (d2 dstate))
 (and
  (eq (s-comp d1) (s-comp d2))
  (eq (b-comp d1) (b-comp d2))
  (eq (p-comp d1) (p-comp d2))))

(defmethod b-comp ((d-state casted-state))
  (b-comp (state d-state)))

(defmethod s-comp ((d-state casted-state))
  (s-comp (state d-state)))

(defmethod p-comp ((d-state casted-state))
  (p-comp (state d-state)))

(defun are-finalstates (pc cfinalstates)
  (every (lambda (state)
	   (container-member state cfinalstates))
	 pc))

(defun bc-redex-p (bc)
  (member *st-redex* bc :test #'eq))

(defun is-final-state-d (d-state cfinalstates)
  (let ((bc (b-comp d-state))
	(pc (p-comp d-state)))
    (and
     (not (bc-redex-p bc))
     (member *st-reducible* bc)
     (not (null pc))
     (are-finalstates pc cfinalstates)
     d-state)))

(defun b-projection (arg)
  (mapcar #'make-ordered-container
	  (mapcar #'b-comp arg)))
