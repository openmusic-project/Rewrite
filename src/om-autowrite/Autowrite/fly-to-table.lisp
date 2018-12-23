(in-package :nautowrite)
(defmethod compute-constant-transitions ((f fly-cast-automaton) deterministic)
  (mapc
   (lambda (root)
     (funcall
      (if deterministic
	  #'add-dtransition-to-transitions
	  #'add-transition-to-transitions)
      (list root)
      (apply-transition-function root '() (transitions-of f))
      *global-transitions*))
   (signature-symbols (constant-signature (signature f)))))

(defmethod add-transitions-for-one-symbol ((f fly-cast-automaton) symbol afai deterministic)
  (let ((transitions (transitions-of f)))
    (mapc
     (lambda (arg)
       (let ((target
	      (apply-transition-function-gft
	       symbol
	       arg
	       transitions)))
	 (when target
	   (funcall
	    (if deterministic
		#'add-dtransition-to-transitions
		#'add-transition-to-transitions)
	    (cons symbol arg)
	    ;;	  (cast-target target)
	    target
	    *global-transitions*))))
     afai)))

(defmethod add-transitions ((f fly-cast-automaton) (ncsign signature)
			    (states ordered-container) (newstates ordered-container) deterministic)
  (let ((afa
	 (arrange-for-arities-and-filter
	  (append (contents states)
		  (contents newstates))
	  (max-arity ncsign) (contents states) ncsign)))
  (mapc
     (lambda (symbol)
       (add-transitions-for-one-symbol
	f symbol (aref afa (arity symbol)) deterministic))
     (signature-symbols ncsign))))

(defmethod compute-transitions ((fly-automaton fly-cast-automaton) deterministic)
  (let ((ncsign (non-constant-signature (signature fly-automaton))))
    (do* ((states
	    (make-empty-ordered-container))
	  (newstates
	   (get-states-from *global-transitions*)
	    (container-difference
	     (get-states-from *global-transitions*)
	     states)))
	 ((empty-container-p newstates))
      (add-transitions
       fly-automaton
       ncsign
       states newstates
       deterministic)
      (container-nunion states newstates))))


(defmethod compile-automaton :before ((f fly-automaton))
  (when (signature-empty-p (constant-signature f))
    (warn "fly-automaton should have a nonempty constant signature to be transformed to a table automaton")))

(defmethod compute-all-transitions ((f fly-cast-automaton))
  (let ((deterministic (deterministic-p f)))
    (compute-constant-transitions f deterministic)
;;    (show *global-transitions*)
    (compute-transitions f deterministic)
;;    (show *global-transitions*)
    ))

(defmethod compile-automaton ((f fly-automaton))
  (unless (fly-casted-transitions-p (transitions-of f))
    (setf f (cast-automaton f)))
  (let ((*global-transitions* (make-empty-transitions))
	(fly-transitions (transitions-of f)))
    (reset-states-table fly-transitions)
    (compute-all-transitions f)
    (setf (states-table *global-transitions*)
	  (states-table fly-transitions))
    (setf (states-table fly-transitions) (make-empty-automaton-states-table))
    (let ((a
	   (make-automaton
	    *global-transitions*
	    :name (name f)
	    :signature (signature f)
	    :finalstates
	    (container-remove-if-not
	     (lambda (state)
	       (final-state-p state f))
	     (get-states *global-transitions*)))))
      a)))

(defmethod compile-automaton ((automaton table-automaton))
  automaton)

(defmethod to-fly-automaton ((f fly-automaton) &key (copy t))
  (if copy (duplicate-automaton f) f))

(defmethod to-fly-automaton ((automaton table-automaton) &key (copy t))
  (when copy
    (setf automaton (duplicate-automaton automaton)))
  (let ((fly-transitions
	 (make-instance
	  'fly-table-transitions
	  :states-table (states-table (transitions-of automaton))
	  :tfun (transitions-fun (transitions-of automaton))
	  :deterministic (deterministic-p automaton))))
  (make-instance
   'fly-table-automaton
   :signature (signature automaton)
   :transitions fly-transitions
   :complete (complete-p automaton)
   :final-state-fun (final-state-fun automaton)
   :puits-state-fun (puits-state-fun automaton)
   :name (name automaton))))
