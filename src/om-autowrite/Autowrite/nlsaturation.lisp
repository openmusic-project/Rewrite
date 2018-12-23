(in-package :nautowrite)

; retourne un ordered-container d'etats de u
(defmethod make-new-rh (rhetheta (nlstates ordered-container))
  (if (casted-state-p rhetheta)
      (container (state rhetheta))
      (if (var-p rhetheta)
	  *u-states*
	  (let* ((vars (vars-of rhetheta))
		 (gstates
		  (mapcar
		   (lambda (theta)
		     (let ((term (apply-substitution rhetheta theta)))
		       (apply-transition-function
			(root term) (arg term) *global-transitions*)))
		   (g-gen-substitution vars (contents nlstates))))
		 (containers (mapcar (lambda (cgs) (container (state cgs)))
				     (remove nil gstates))))
;;	    (format *trace-output* "~A containers~%" containers)
	    (reduce #'container-union containers
		    :initial-value (make-empty-ordered-container))))))

(defun gmatch (argse args)
  (notany
   #'empty-container-p
   (mapcar
    (lambda (c1 c2) (container-intersection c1 (container (state c2))))
    argse
    args)))

;; container updated with new accessible states
(defmethod update-rhs-deltatransition-with-rule (key rule (nlstates ordered-container))
  (let* ((lhe (left-handside rule))
	 (rhe (right-handside rule))
	 (argse (make-q-containers '() (arg lhe) *u-states*)))
    (and (eq (root lhe) (car key))
	 (gmatch argse (cdr key))
	 (let* ((theta (remove-if-not
			(lambda (x) (var-p (car x)))
			(mapcar #'list (arg lhe) (cdr key))))
		(rhetheta  (apply-substitution rhe theta)))
	   (make-new-rh rhetheta nlstates)))))

(defmethod update-rhs-deltatransition-with-rules
    (key (value casted-state) (rules rules) (nlstates ordered-container))
  (let ((oldcontainer (container (state value)))
	(new-containers
	 (remove nil
		 (mapcar
		  (lambda (rule)
		    (update-rhs-deltatransition-with-rule key rule nlstates))
		  (rules-list rules)))))
    (if new-containers
	(reduce #'container-union
		(cons oldcontainer new-containers))
	oldcontainer)))

(defmethod update-deltatransitions-with-rules ((rules rules) (nlstates ordered-container))
  (let ((new nil))
    (mapc 
     (lambda (transition)
       (let ((key (car transition))
	     (value (cadr transition)))
	 (let ((container
		(update-rhs-deltatransition-with-rules
		 key value rules nlstates)))
	   (when (> (container-size container)
		    (container-size (container (state value))))
	     (setf new t)
	     (add-dtransition-to-transitions key container *global-transitions*)))))
     (key-value-list *global-transitions*))
  new))

(defmethod new-add-deltatransitions (afa transitions (ncsignature signature))
  (mapc
   (lambda (symbol)
     (mapc
      (lambda (arg)
	(add-dtransition-to-transitions
	 (cons symbol arg)
	 (make-gstate
	  (apply-transition-function-gft
	   symbol
	   (mapcar
	    (lambda (c) (container (state c)))
	    arg)
	   transitions))
	 *global-transitions*))
      (aref afa (arity symbol))))
   (signature-symbols ncsignature)))

(defmethod adapt-deltatransitions ((rules rules) states newstates aut-u)
  (let* ((signature (signature aut-u))
	 (ncsignature (non-constant-signature signature))
	 (transitions (transitions-of aut-u))
	 (nlstates (container-union states newstates))
	 (afa (arrange-for-arities-and-filter
	       (contents nlstates) (max-arity ncsignature)
	       (contents states) signature)))
    (new-add-deltatransitions afa transitions ncsignature)
    (update-deltatransitions-with-rules rules nlstates)))

(defmethod add-deltatransitions-for-constants ((aut-u abstract-automaton))
  (let ((csignature (constant-signature (signature aut-u)))
	(transitions (transitions-of aut-u)))
    (mapc
     (lambda (symbol)
       (let ((target (apply-transition-function  symbol () transitions)))
	 (unless (target-empty-p target)
	   (add-dtransition-to-transitions
	    (list symbol)
	    (make-gstate target)
	    *global-transitions*))))
     (signature-symbols csignature))))

(defmethod delta-transitions ((rules rules) (aut-u abstract-automaton))
  (add-deltatransitions-for-constants aut-u)
  (do* ((states (make-empty-ordered-container))
	(newstates (cright-handsides *global-transitions*)
	  (container-difference (cright-handsides *global-transitions*) states))
	(new
	  (adapt-deltatransitions rules states newstates aut-u)
	  (adapt-deltatransitions rules states newstates aut-u)))
      ((and (empty-container-p newstates) (not new)))
;;      (format *error-output* "*newstates*: ~A ~%" newstates)
    (container-nunion states newstates)))

(defmethod final-det-states ((dstates ordered-container) (finalstates ordered-container))
  (container-remove-if
   (lambda (dstate)
     (empty-container-p
      (container-intersection (container (state dstate)) finalstates)))
   dstates))

(defmethod det-saturation-automaton
    ((rules rules) (automaton table-automaton) &key (simplify nil))
  (with-new-transitions
    (let ((*u-states* (get-states automaton))
	  (signature (signature automaton)))
      (delta-transitions rules automaton)
      (let ((a
	     (make-automaton
	      *global-transitions*
	      :name (format nil "aut-c-toyama-~A~A~A~A"
			    (name (trs *spec*))
			    (if (in-signature-extra signature)
				"@" "")
			    (get-approx-name (seqsys-approx (trs *spec*)))
			    (if (in-signature *bullet-symbol* signature)
				"-o" ""))
	      :signature signature
	      :finalstates (final-det-states (states-from-states-table
					      (states-table *global-transitions*))
					     (get-finalstates automaton)))))
	(setf a (nreduce-automaton a))
	(if simplify
	    (nsimplify-automaton a)
	    a)))))
