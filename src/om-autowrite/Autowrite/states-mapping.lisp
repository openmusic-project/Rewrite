(in-package :nautowrite)

(defvar *states-mapping*)
(defvar *bijective-mapping* nil)

;; when apply-states-mapping is applied the assoc-list
;; *states-mapping* must be set

(defun bijective-mapping-p (states-mapping)
  (let ((targets (mapcar #'cdr states-mapping)))
    (= (length targets) (length (symbol-remove-duplicates targets)))))

(defgeneric apply-states-mapping (object-with-states))

(defmethod apply-states-mapping :around ((object-with-states t))
  (if (endp *states-mapping*)
      object-with-states
      (call-next-method)))

(defmethod apply-states-mapping ((o t))
  o)

(defmethod apply-states-mapping ((l list))
  (mapcar
   (lambda (object) (apply-states-mapping object))
   l))

(defmethod apply-states-mapping ((state casted-state))
  (let ((found (assoc state *states-mapping*)))
;;    (unless found (format *trace-output* "Warning state ~A not found in apply-states-mapping ~%" state))
    (if found
	(cdr found)
	state)))
	
(defmethod apply-states-mapping ((term term))
  (build-term 
   (root term)
   (apply-states-mapping (arg term))))

(defmethod apply-states-mapping ((container container))
  (make-container (apply-states-mapping (contents container))))

(defmethod apply-states-mapping ((container ordered-container))
  (make-ordered-container (apply-states-mapping (contents container))))

(defmethod apply-states-mapping ((automaton table-automaton))
  (napply-states-mapping (duplicate-automaton automaton)))

(defmethod napply-states-mapping ((automaton table-automaton))
  (setf (get-finalstates automaton)
	(apply-states-mapping (get-finalstates automaton)))
  (setf (transitions-of automaton)
	(apply-states-mapping (transitions-of automaton)))
  (when (puits-state-boundp automaton)
    (setf (get-puits-state automaton)
	  (apply-states-mapping (get-puits-state automaton))))
  (when (equivalence-classes-boundp automaton)
    (setf (get-equivalence-classes automaton)
	  (apply-states-mapping (equivalence-classes automaton))))
  (unless (bijective-mapping-p *states-mapping*))
  (set-reduced-unknown automaton)
  (set-complete-unknown automaton)
  (set-epsilon-unknown automaton)
  automaton)

