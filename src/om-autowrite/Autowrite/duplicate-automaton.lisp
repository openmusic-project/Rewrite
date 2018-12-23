(in-package :nautowrite)

;; (defmethod duplicate-state ((state casted-state))
;;   (make-instance 'casted-state
;; 		 :state-number (state-number state)
;; 		 :state (state state)))

(defmethod create-states-mapping ((container container) f)
  ;; on passe par un container comme ca on est sur qu'il n'y a pas de doublon
  ;; et qu'ils sont dans l'ordre
  (mapcar (lambda (state)
	    (cons state (funcall f state)))
	  (contents container)))

(defmethod duplicate-states ((container container))
  (create-states-mapping container #'duplicate-state))

;;; nouvelle version
(defmethod duplicate-state ((casted-state casted-state))
  (let ((ncs (cast-state (state casted-state))))
    (setf (state-number ncs) (state-number casted-state))
    ncs))

(defmethod duplicate-automaton ((automaton table-automaton))
  (with-new-transitions
    (let* ((*states-mapping* (duplicate-states (get-states automaton)))
	   (finalstates (apply-states-mapping (get-finalstates automaton)))
	   (tr (apply-states-mapping (transitions-of automaton))))
      (setf (transitions-table *global-transitions*) (transitions-table tr))
      (let ((a
	     (make-automaton
	      *global-transitions*
	      :finalstates finalstates
	      :name (name automaton)
	      :signature (copy-signature (signature automaton)))))
	(when (puits-state-boundp automaton)
	  (setf (get-puits-state a)
		(apply-states-mapping (get-puits-state automaton))))
	(when (equivalence-classes-boundp automaton)
	  (setf (get-equivalence-classes a)
		(apply-states-mapping (get-equivalence-classes automaton))))
	(when (slot-boundp automaton 'reduced)
	  (setf (get-reduced a) (get-reduced automaton)))
	(when (slot-boundp automaton 'coreduced)
	  (setf (get-reduced a) (get-coreduced automaton)))
	(when (slot-boundp automaton 'complete)
	  (setf (get-complete a) (get-complete automaton)))
	(when (slot-boundp automaton 'complete)
	  (setf (get-complete a) (get-complete automaton)))
	(when (slot-boundp automaton 'epsilon)
	  (setf (get-epsilon a) (get-epsilon automaton)))
	a))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *skim* t)

(defgeneric skim-state (state))

(defgeneric nskim-automaton (automaton))

(defmethod skim-state ((casted-state casted-state))
  (setf (slot-value casted-state 'state) (make-instance 'abstract-state)))

(defmethod nskim-automaton ((a table-automaton))
  (number-states a)
  (mapcar #'skim-state (contents (get-states a)))
  a)
