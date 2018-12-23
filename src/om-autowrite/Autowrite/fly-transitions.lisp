(in-package :nautowrite)

(defclass fly-transitions (abstract-transitions)
  ((tfun :initarg :tfun :reader tfun)
   (deterministic :reader deterministic-p :initarg :deterministic)
   (state-index :initform nil :accessor state-index :initarg :state-index)))

(defclass fly-cast-transitions (fly-transitions  cast-states-mixin) ())
(defclass fly-table-transitions (fly-cast-transitions) ())
(defclass fly-casting-transitions (fly-cast-transitions) ())

(defgeneric fly-table-transitions-p (transitions))
(defgeneric fly-casted-transitions-p (transitions))

(defmethod fly-casted-transitions-p ((transitions fly-transitions)) nil)
(defmethod fly-casted-transitions-p ((transitions fly-table-transitions)) t)
(defmethod fly-casted-transitions-p ((transitions fly-casting-transitions)) t)

(defmethod fly-table-transitions-p (transitions)
  (typep transitions 'fly-table-transitions))

(defmethod duplicate-transitions ((fly-transitions fly-transitions))
  (make-instance
   (type-of fly-transitions)
   :tfun  (slot-value fly-transitions 'tfun)
   :deterministic (slot-value fly-transitions 'deterministic)
   :state-index (slot-value fly-transitions 'state-index)))

(defun apply-signature-mapping-tfun (tfun)
  (let ((inverse-mapping (inverse-mapping *signature-mapping*)))
    (lambda (root states)
      (let ((symbols
	     (let ((*signature-mapping* inverse-mapping))
	       (apply-signature-mapping root)))
	    (target nil))
;;	(format *trace-output* "asm ~A~A -> " symbols states)
	(loop
	   for r in symbols
	   do (setf target
		    (target-union
		     (funcall tfun r states)
		     target)))
;;	(format *trace-output* "~A~%" target)
	target))))

(defmethod apply-signature-mapping ((fly-transitions fly-transitions))
  (let ((injective (injective-mapping-p *signature-mapping*)))
    (make-instance
     (type-of fly-transitions)
     :tfun (apply-signature-mapping-tfun (tfun fly-transitions))
     :deterministic (and (deterministic-p fly-transitions) injective)
     :state-index (slot-value fly-transitions 'state-index))))

(defmethod nindex-fly-transitions ((fly-transitions fly-transitions)
				   (state-index integer))
  (setf (state-index fly-transitions) state-index)
  fly-transitions)

(defmethod index-fly-transitions ((fly-transitions fly-transitions)
				   (state-index integer))
  (make-instance
   (type-of 'fly-transitions)
   :tfun (tfun fly-transitions)
   :state-index (slot-value fly-transitions 'state-index)
   :deterministic  (slot-value fly-transitions 'deterministic)))

(defmethod transitions-fun ((fly-transitions fly-transitions))
  (let ((index (state-index fly-transitions)))
    (if index
	(lambda (root states)
	  (index-target
	   (funcall
	    (tfun fly-transitions)
	    root
	    (mapcar #'state states)) index))
	(tfun fly-transitions))))

(defmethod transitions-fun ((fly-transitions fly-casting-transitions))
  (lambda (root casted-states)
      (let ((target (funcall (call-next-method)
			    root (mapcar #'state casted-states))))
	(when target
	  (let ((*automaton-states-table* (states-table fly-transitions)))
	    (let ((casted-target (cast-target target)))
	      casted-target))))))

(defun make-fly-transitions (tfun cast &key (deterministic t))
  (make-instance
   (if cast 'fly-casting-transitions 'fly-transitions)
   :tfun tfun
   :deterministic deterministic))

(defmethod make-p-target (&rest targets)
  (make-target
   (make-container
    (mapcar #'make-tuple-state (cartesian-product
				(mapcar #'contents targets))))))

(defmethod product-transitions-fun
    ((transitions1 fly-transitions) (transitions2 fly-transitions))
  (lambda (root pstates)
    (let* ((states1 (mapcar
		     (lambda (pstate)
		       (first (tuple pstate)))
		     pstates))
	   (states2 (mapcar
		     (lambda (pstate)
		       (second (tuple pstate)))
		     pstates))
	   (rhs1 (apply-transition-function-gft root states1 transitions1))
	   (rhs2 (apply-transition-function-gft root states2 transitions2)))
      (when (not (or (target-empty-p rhs1) (target-empty-p rhs2)))
	(make-p-target rhs1 rhs2)))))

(defmethod product-transitions-fun-gen (&rest ltransitions)
  (lambda (root pstates)
    (assert (every (lambda (tr) (typep tr 'abstract-transitions)) ltransitions))
    (let ((rhs
	   (if (endp pstates)
	       (mapcar
		(lambda (transitions)
		  (apply-transition-function-gft root '() transitions))
		ltransitions)
	       (loop
		 for transitions in ltransitions
		 for tuples = (mapcar #'tuple pstates)
		   then (mapcar #'cdr tuples)
		 collect (apply-transition-function-gft
			       root (mapcar #'car tuples) transitions)))))
;;    (format *trace-output* "rhs ~A ~%" rhs)
      (unless (some #'target-empty-p rhs)
	(apply #'make-p-target rhs)))))

(defmethod det-transitions-fun ((transitions fly-transitions))
  (lambda (root states)
    (let ((target
	   (apply-transition-function-gft
	    root (mapcar
		  (lambda (state)
		    (container state))
		  states)
	    transitions)))
      (when target
	(make-gstate target)))))

(defun complete-tfun (tfun)
  (let ((puits-state (new-puits-state)))
    (lambda (root states)
      (let ((target
	     (funcall tfun root states)))
	(or
	 target
       puits-state)))))

(defmethod complete-fly-transitions ((fly-transitions fly-transitions))
  (make-instance
   (type-of fly-transitions)
   :tfun (complete-tfun (tfun fly-transitions))
   :deterministic (slot-value fly-transitions 'deterministic)
   :state-index (slot-value fly-transitions 'state-index)))

;;;;;;;;;;;;;;;;;;;;;;; A TESTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *max-number-of-states* 1000)
(defvar *clean-states-table* nil)

(defmethod apply-transition-function-gft :after 
    ((root abstract-arity-symbol) (targets list) (transitions fly-casting-transitions))
  (when (and *clean-states-table*
	     (> (hash-table-count (states-table transitions)) *max-number-of-states*))
    (reset-states-table transitions)))

(defmacro with-clean-states-table (&body body)
  ` (let ((*clean-states-table* t))
      ,@body))