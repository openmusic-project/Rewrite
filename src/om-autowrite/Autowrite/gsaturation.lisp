(in-package :nautowrite)

(defvar *u-states*) ; liste d'etats de u

(defmethod make-q (theta term (allstates container))
  (if (var-p term)
      (let ((insubst (my-assoc term theta)))
	(if insubst
	    (list (second insubst))
	    (contents *u-states*)))
      (list (find-casted-state-from-state
	     (make-indexed-state (make-term-state term) 0)
	     *u-states*))))

(defmethod make-q-containers (theta args (allstates container))
  (mapcar
   (lambda (arg)
     (make-ordered-container (make-q theta arg allstates)))
   args))

(defmethod make-q-args (theta args (allstates container))
  (mapcar
   (lambda (arg)
     (make-q theta arg allstates))
   args))

;; returns T if transition really added
(defmethod add-transitions-for-theta
    ((lh term) theta target (transitions transitions))
  (let ((args (cartesian-product
	       (make-q-args theta (arg lh) *u-states*)))
	(new nil))
    (mapc
     (lambda (arg)
       (setf new
	     (or new (add-transition-to-transitions
		      (cons (root lh) arg)
		      target
		      transitions))))
     args)
    new))

(defmethod add-transitions-for-theta
    ((lh var) theta target transitions)
  (let ((state (cadr (assoc lh theta))))
    (add-transition-to-transitions (list state) target transitions)))

;; returns T if at least one transition has been added
(defmethod add-transitions-for-lh
    ((lh abstract-term) (tsl list) (transitions transitions))
  (let ((new nil))
    (dolist (theta-state tsl)
      (setf new
	    (or new
		(add-transitions-for-theta
		 lh (car theta-state)
		 (make-target
		  (make-ordered-container (cadr theta-state)))
		 transitions))))
    new))

(defmethod make-theta-state-list (rh (transitions transitions))
;; rh term of variable!!!
  (let* ((vars (vars-of rh))
	 (substs (g-gen-substitution vars (contents *u-states*))))
    (remove-if #'null
	       (mapcar
		(lambda (theta)
		  (let* ((term (apply-substitution rh theta))
			 (target (transitions-compute-target term transitions)))
		  (list theta
			(contents target)
			 )))
		substs)
	       :key #'cadr)))

;; Returns T if at least one transition has been added
(defmethod add-transitions-for-rule ((transitions transitions) (rule rule))
  (add-transitions-for-lh
   (left-handside rule)
   (make-theta-state-list (right-handside rule) transitions)
   transitions))

(defmethod add-transitions-for-rules
    ((transitions transitions) (rules list))
  (let ((new nil))
    (mapc
     (lambda (rule)
       (setf new (or new (add-transitions-for-rule transitions rule))))
     rules)
    new))

(defun nsaturation (rules automaton)
  (let* ((transitions (transitions-of automaton))
	 (rules-list (rules-list rules))
	 (*u-states* (get-states automaton)))
    (loop
       until
	 (or (not (add-transitions-for-rules
		   transitions
		   rules-list)))))
  (epsilon-closure-automaton automaton))

(defmethod saturation ((rules rules) (automaton table-automaton))
  (nsaturation rules (duplicate-automaton automaton)))
