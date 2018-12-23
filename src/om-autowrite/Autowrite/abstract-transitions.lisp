(in-package :nautowrite)

(defclass abstract-transitions () ())

(defgeneric transitions-fun (transitions)
  (:documentation
   "the transition function to be applied to a symbol of arity i and i states
    returns a set of states or a single state if deterministic"))

(defgeneric transitions-compute-target (term transitions)
  (:documentation "computes the target of TERM with TRANSITIONS"))

(defgeneric apply-transition-function (root states transitions)
  (:documentation "computes the target of ROOT(STATES) with TRANSITIONS"))

(defgeneric apply-transition-function-gft (root targets transitions)
  (:documentation "computes the target of ROOT(TARGETS) with TRANSITIONS"))

(defmethod apply-transition-function 
    ((root abstract-arity-symbol) (states list) (abstract-transitions abstract-transitions))
  (funcall (transitions-fun abstract-transitions) root states))

(defmethod transitions-compute-target ((state abstract-state) (transitions abstract-transitions))
  (declare (ignore transitions))
  state)

(defmethod transitions-compute-target ((term term) (transitions abstract-transitions))
  (let ((targets
	 (mapcar
	  (lambda (arg)
	    (transitions-compute-target arg transitions))
	  (arg term))))
    (apply-transition-function-gft (root term) targets transitions)))

(defmethod apply-transition-function-gft
    ((root abstract-arity-symbol) (targets list) (transitions abstract-transitions))
  (do ((newargs (targets-product targets) (cdr newargs))
       (target nil))
      ((null newargs) target)
    (let ((cvalue (apply-transition-function root (car newargs) transitions)))
      (when cvalue
	(setf target (target-union cvalue target))
	target))))
