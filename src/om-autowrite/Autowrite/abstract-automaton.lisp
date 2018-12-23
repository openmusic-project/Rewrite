(in-package :nautowrite)

(defgeneric compute-target (term automaton &optional signal)
  (:documentation "computes the target of TERM with AUTOMATON"))

(defgeneric final-state-fun (automaton)
  (:documentation
   "the predicate to be applied to a state of AUTOMATON to decide whether it is a finalstate"))

(defgeneric deterministic-p (automaton))

(defclass automata ()
  ((automaton-table :initform (make-hash-table :test #'equal)
		    :accessor table)))

(defun make-automata () (make-instance 'automata))

(defgeneric transitions-of (automaton)
  (:documentation
   "transitions of AUTOMATON"))

(defclass abstract-automaton (named-object signature-mixin)
  ((transitions :initarg :transitions :accessor transitions-of)))

(defmethod print-object ((a abstract-automaton) stream)
  (format stream "~A" (automaton-name a)))

(defgeneric automaton-name (a))

(defmethod automaton-name ((a abstract-automaton))
  (name a))

(define-condition state-found ()
  ((state :initarg :state :reader state)
   (subterm :initarg :subterm :reader subterm)))

(define-condition puits-found (state-found) ())
(define-condition success-found (state-found) ())

(defgeneric final-state-p (state abstract-automaton))
(defmethod final-state-p ((state (eql nil)) (a abstract-automaton)) nil)
(defmethod final-state-p ((state abstract-state) (a abstract-automaton))
;;  (format *error-output* "final-state-p abstract-state abstract-automaton ~A ~A~%" state a)
  (funcall (final-state-fun a) state))

(defgeneric puits-state-p (state abstract-automaton))
(defmethod puits-state-p ((state (eql nil)) (a abstract-automaton)) t)
(defmethod puits-state-p ((state abstract-state) (a abstract-automaton))
  (funcall (puits-state-fun a) state))

(defgeneric puits-target-p (target abstract-automaton))
(defmethod puits-target-p (target (a abstract-automaton))
  (every
   (lambda (state)
     (puits-state-p state a))
   (contents target)))

(defgeneric final-target-p (target automaton))

(defmethod final-target-p (target (a abstract-automaton))
  (some
   (lambda (state)
     (final-state-p state a))
   (contents target)))

(defmethod deterministic-p ((a abstract-automaton))
  (deterministic-p (transitions-of a)))

(defmethod signal-target ((term term) target)
  (signal (make-condition 'state-found
			  :state target
			  :subterm term)))
  
(defvar *save-run* nil)

(defun toggle-save-run ()
  (setf *save-run* (not *save-run*)))

(defgeneric compute-target-and-signal (term automaton signal))

(defmethod compute-target-and-signal
    ((term term) (automaton abstract-automaton) signal)
  (let ((targets
	 (mapcar
	  (lambda (arg)
	    (compute-target-and-signal arg automaton signal))
	  (arg term))))
    (let ((target
	   (apply-transition-function-gft
	    (root term)
	    targets
	    (transitions-of automaton))))
      (when (and signal (puits-target-p target automaton))
	(signal-target term target))
      (when *save-run*
	(setf (run term) target))
      target)))

(defmethod compute-target ((term term) (a abstract-automaton) &optional (signal t))
  (when *save-run*
    (erase-run term))
  (handler-case
     (compute-target-and-signal term a signal)
    (puits-found (c)
      (prog1 (state c)
	(format *standard-output* "puits reached at ~A~%"
		(subterm c))))))

(defmethod compute-state ((term term) (a abstract-automaton))
  (let ((target (compute-target term a)))
    (and target
	 (if (abstract-state-p target)
	     target
	     (first (contents target))))))

(defgeneric recognized-p (term automaton)
  (:documentation "true if TERM is recongized by AUTOMATON"))

(defmethod recognized-p ((term term) (a abstract-automaton))
  (let ((target (compute-target term a)))
    (values
      (final-target-p target a)
      target)))

(defmethod non-emptiness-witness ((a abstract-automaton))
  (unless (casted-p a)
    (setf a (cast-automaton a)))
  (multiple-value-bind (table state)
      (state-term-table
       (lambda (state) (final-state-p state a))
       a)
    (when state
      (term-from-state-term-table table state))))

(defgeneric automaton-emptiness (automaton)
  (:documentation
   "Returns t, nil if the recognized language is empty.
    Returns nil, term if the recognized language is not empty."))

(defmethod automaton-emptiness ((a abstract-automaton))
  (let ((term (non-emptiness-witness a)))
    (if term
	(values nil term)
	(values t nil))))

(defmethod constant-signature ((a abstract-automaton))
  (constant-signature (signature a)))
