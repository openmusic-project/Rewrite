(in-package :nautowrite)

(defun new-puits-state ()
  (make-named-state (symbol-name (gensym))))

(defgeneric new-marked (transitions marked)
  (:documentation "states accessible assuming marked states are accessible"))

(defmethod has-only-marked-states ((key list) (marked container))
  (casted-state-subsetp (cdr key) (contents marked)))

(defmethod has-only-marked-states ((casted-state casted-state)
				   (marked container))
  (container-member casted-state marked))

(defmethod has-only-marked-states ((arity-symbol abstract-arity-symbol)
				   (marked container))
  t)

(defmethod has-only-marked-states ((container container) (marked container))
  (container-subset-p container marked))

(defgeneric mark-states (transitions marked)
  (:documentation "returns a container of accessible states"))

(defgeneric accessible-states (transitions)
  (:documentation "returns a container of accessible states"))

(defmethod accessible-states ((transitions transitions))
  (mark-states transitions (make-empty-ordered-container)))
  
(defmethod mark-states ((transitions transitions) (marked container))
  (let* ((nm (new-marked transitions marked))
	 (sd (container-difference nm marked)))
    (if (empty-container-p sd)
	marked
	(mark-states transitions (container-union marked sd)))))

(defmethod reduced-automaton-p ((automaton table-automaton))
  (let ((inaccessible
	 (container-difference
	  (get-states automaton)
	  (accessible-states (transitions-of automaton)))))
    (values
     (empty-container-p inaccessible)
     inaccessible)))

(defmethod nreduce-automaton ((automaton table-automaton))
  :documentation "Destructive: reduces AUTOMATON"
  (unless (reduced-p automaton)
    (let* ((transitions (transitions-of automaton))
	   (newstates (accessible-states transitions)))
      (nfilter-with-states newstates transitions)
      (setf (get-finalstates automaton)
	    (container-intersection newstates (get-finalstates automaton))))
      (setf (get-reduced automaton) t))
  automaton)

(defgeneric reduce-automaton (automaton)
  (:documentation "returns AUTOMATON if reduced or the reduced version of AUTOMATON otherwise"))

(defmethod reduce-automaton ((automaton table-automaton))
  :documentation "returns AUTOMATON if reduced or the reduced version of AUTOMATON otherwise"
  (nreduce-automaton (duplicate-automaton automaton)))

(defgeneric complete-transitions-p (signature afa transitions)
  (:documentation "true if the TRANSITIONS are complete according to the SIGNATURE and
all possible combination of states in AFA"))

(defmethod complete-transitions-p
    (signature afa (transitions abstract-transitions))
  (every
   (lambda (symbol)
     (every
      (lambda (arg)
	(transitions-compute-target (build-term symbol arg) transitions))
      (aref afa (arity symbol))))
   (signature-symbols signature)))

(defgeneric complete-automaton-p (automaton))

(defmethod complete-automaton-p ((automaton table-automaton))
  (let* ((signature (signature automaton))
	 (states (contents (get-states automaton)))
	 (afa (arrange-for-arities states (max-arity signature))))
    (complete-transitions-p
     signature afa
     (transitions-of automaton))))

(defmethod complete-transitions-with-puits
    ((automaton table-automaton) (puits casted-state))
  (let* ((signature (signature automaton))
	 (transitions (transitions-of automaton))
	 (newstates (contents
		     (states-from-states-table
		      (states-table transitions))))
;;(cons puits (contents (get-states automaton))))
	 (afa (arrange-for-arities newstates (max-arity signature))))
    (complete-transitions puits signature afa transitions)))

(defmethod ncomplete-automaton ((automaton table-automaton))
  :documentation "Destructive! completes automaton adding possibly an extra state and the necessary transitions"
    (unless (complete-p automaton)
      (let ((puits (find-puits-state automaton) ))
	(unless puits
	  (with-states-table (transitions-of automaton)
	    (setf puits (cast-state (new-puits-state)))))
	(complete-transitions-with-puits automaton puits)
	(setf (get-complete automaton) t)
	(setf (get-puits-state automaton) puits)
	(when (equivalence-classes-boundp automaton)
	  (setf (get-equivalence-classes automaton)
		(cons (make-container-from-state puits)
		      (get-equivalence-classes automaton))))
	;;	    (unset-equivalence-classes automaton)
	))
  automaton)

(defmethod complete-automaton ((automaton table-automaton))
  :documentation "returns A if complete, a copy completed of AUTOMATON otherwise"
  (if (complete-p automaton)
      automaton
      (let ((ca (duplicate-automaton automaton)))
	(setf (name ca)
	      (concatenate 'string "completion-of-" (name automaton)))
	(ncomplete-automaton ca))))

(defgeneric nautomaton-without-symbol (symbol automaton))
(defgeneric nautomaton-without-symbols (symbols automaton))

(defmethod nautomaton-without-symbol ((symbol abstract-arity-symbol) (automaton abstract-automaton))
  (nautomaton-without-symbols (list symbol) automaton))

(defmethod nautomaton-without-symbols ((symbols list) (automaton table-automaton))
  (setf (signature automaton) (remove-symbols-from-signature symbols (signature automaton)))
  (setf (transitions-of automaton) (remove-those-with-symbols symbols (transitions-of automaton)))
  (set-reduced-unknown automaton)
  (nreduce-automaton automaton)
  automaton)

(defgeneric automaton-without-symbol (symbol automaton))
(defgeneric automaton-without-symbols (symbols automaton))

(defmethod automaton-without-symbols ((symbols list) (automaton abstract-automaton))
  (if symbols
      (nautomaton-without-symbols symbols (duplicate-automaton automaton))
      automaton))

(defmethod automaton-without-symbol ((symbol abstract-arity-symbol) (automaton table-automaton))
  (automaton-without-symbols (list symbol) automaton))

;; destructive
(defmethod nuncomplete-automaton ((automaton table-automaton))
  :documentation "Destructive: uncompletes AUTOMATON"
  (let ((puits (find-puits-state automaton)))
    (when puits ;; there is not always one
      (nfilter-with-states (container-remove puits (get-states automaton))
			   (transitions-of automaton))
      (when (equivalence-classes-boundp automaton)
	(setf (get-equivalence-classes automaton)
	      (remove puits (equivalence-classes automaton)
		      :key (lambda (container) (car (contents container)))
		      :test #'eq)))
      (unset-puits-state automaton)
      (set-complete-unknown automaton)
      ))
  automaton)

(defmethod uncomplete-automaton ((automaton table-automaton))
  (nuncomplete-automaton (duplicate-automaton automaton)))
