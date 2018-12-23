(in-package :nautowrite)
(defvar *limit-states-value* 10)

(defclass table-automaton (abstract-automaton)
  ((finalstates :initarg :finalstates
		:initform (make-empty-ordered-container) :accessor get-finalstates)
   (puits-state :accessor get-puits-state)
   (equivalence-classes :accessor get-equivalence-classes)
   (reduced  :accessor get-reduced :initarg :reduced)
   (coreduced  :accessor get-coreduced)
   (complete :accessor get-complete)
   (epsilon :accessor get-epsilon)))

(defmethod automaton-name ((f table-automaton))
  (format nil "tb-~A" (name f)))

(defmethod print-object :after ((a table-automaton) stream)
  (show-automaton-characteristics a :stream stream))

(defmethod get-states ((automaton table-automaton))
  (get-states (transitions-of automaton)))

(defmethod make-automaton
    ((transitions transitions)
     &key
     name
     (signature nil)
     finalstates
     (reduced '?? supplied-p-reduced)
     (complete '?? supplied-p-complete)
     (epsilon '?? supplied-p-epsilon))
  (when finalstates
    (assert (ordered-container-p finalstates)))
  (let ((a
	 (make-instance 'table-automaton
			:name name
			:signature (or signature (signature-from transitions))
			:finalstates finalstates
			:transitions transitions
			)))
    (when supplied-p-reduced (setf (get-reduced a) reduced))
    (when supplied-p-complete (setf (get-complete a) complete))
    (when supplied-p-epsilon
      (setf (get-epsilon a) epsilon)
      (when epsilon
	(assert (not (deterministic-p a)))))
    a))

(defmethod final-state-fun ((automaton table-automaton))
  (lambda (casted-state)
    (and
     (container-member casted-state (get-finalstates automaton))
     t)))

(defmethod puits-state-fun ((automaton table-automaton))
  (lambda (casted-state)
    (eq casted-state (puits-state automaton))))

(defun make-empty-automaton ()
  (make-automaton (make-empty-transitions) :name "empty automaton"))

(defmethod equivalence-classes-boundp ((automaton table-automaton))
   (slot-boundp automaton 'equivalence-classes))

(defmethod puits-state-boundp ((automaton table-automaton))
   (slot-boundp automaton 'puits-state))

(defmethod unset-equivalence-classes ((automaton table-automaton))
  (slot-makunbound automaton 'equivalence-classes))

(defmethod set-complete-unknown ((automaton table-automaton))
  (slot-makunbound automaton 'complete))

(defmethod set-epsilon-unknown ((automaton table-automaton))
  (slot-makunbound automaton 'epsilon))

(defmethod unset-puits-state ((automaton table-automaton))
  (slot-makunbound automaton 'puits-state))

(defmethod puits-state ((automaton table-automaton))
  (when (complete-p automaton)
    (unless (slot-boundp automaton 'puits-state)
      (setf (get-puits-state automaton) (find-puits-state automaton)))
    (get-puits-state automaton)))

(defmethod set-reduced-unknown ((automaton table-automaton))
  (slot-makunbound automaton 'reduced))

(defmethod show-states ((states container) stream)
  (setf states (contents states))
  (let ((n (length states)))
    (if (and (not *loadable*) (> n *limit-states-value*))
	(progn
	  (display-sequence (nthcdr *limit-states-value* states) stream)
	  (format stream "... ~A more" (- n *limit-states-value*)))
	(display-sequence states stream))))
 
(defmethod show :before ((a table-automaton) &optional (stream t))
  (format stream "Automaton "))
  
(defmethod show :after ((a table-automaton) &optional (stream t))
  (when *loadable*
    (setf a (duplicate-automaton a))
    (number-states  a))
  (format stream "~%States ")
  (show-states (get-states a) stream)
  (format stream "~%")
  (format stream "Final States ")
  (show-states (get-finalstates a) stream)
  (format stream "~%")
  (format stream "Transitions~%")
  (show (transitions-of a) stream))

(defmethod equivalence-classes ((automaton table-automaton))
  (unless (slot-boundp automaton 'equivalence-classes)
    (setf (get-equivalence-classes automaton)
	  (equivalence-classes-automaton automaton)))
  (get-equivalence-classes automaton))

(defmethod get-equivalence-classes :before ((automaton table-automaton))
  (assert (deterministic-p automaton)))
  
(defmethod reduced-p ((automaton table-automaton))
;;   (when (slot-boundp automaton 'reduced)
;;     (assert (eq (get-reduced automaton)
;; 		(reduced-automaton-p automaton))))
  (unless (slot-boundp automaton 'reduced)
    (setf (get-reduced automaton)
	  (reduced-automaton-p automaton)))
  (get-reduced automaton))

;; (defmethod coreduced-p ((automaton table-automaton))
;; ;;   (when (slot-boundp automaton 'coreduced)
;; ;;     (assert (eq (get-coreduced automaton)
;; ;; 		(coreduced-automaton-p automaton))))
;;   (unless (slot-boundp automaton 'coreduced)
;;     (setf (get-coreduced automaton)
;; 	  (coreduced-automaton-p automaton)))
;;   (get-coreduced automaton))


(defmethod complete-p ((automaton table-automaton))
;;  (when (slot-boundp automaton 'complete)
;;    (assert (eq (get-complete automaton)
;;		(complete-automaton-p automaton))))
  (unless (slot-boundp automaton 'complete)
    (setf (get-complete automaton)
	  (complete-automaton-p automaton)))
  (get-complete automaton))

; destructive
(defun number-states (automaton &optional (start 0))
  (let ((states (contents (get-states automaton))))
    (let ((n (1- start)))
      (dolist (s states n)
	(setf (state-number s) (incf n)))))
  automaton)

; destructive
(defgeneric name-states (automaton)
  (:documentation "toggle numbered states to named states"))

(defmethod name-states ((automaton table-automaton))
  (let ((states (contents (get-states automaton))))
    (when states
      (let ((first (first states)))
;;	(unless (state-number first)
;;	  (format *error-output* "Warning: States not numbered"))
	(unless (state first)
	   (format *error-output* "Warning: States have no name"))
	(when (and (state-number first) (state first))
	    (mapc (lambda (s)
		    (setf (state-number s) nil))
		  states)))
      automaton)))

(defun states-named-p (automaton)
  (let ((states (contents (get-states automaton))))
    (or (endp states) (null (state-number (car states))))))

(defgeneric states-numbered-p (automaton)
  (:documentation "true if states are numbered"))

(defmethod states-numbered-p ((automaton table-automaton))
  (let ((states (contents (get-states automaton))))
    (or (endp states) (state-number (car states)))))

(defmethod transitions-list ((automaton table-automaton))
  (transitions-list (transitions-of automaton)))

(defmethod show-automaton-characteristics :after ((aut abstract-automaton) &key (stream t) (newline t))
    (when newline (format stream "~%")))

(defmethod show-automaton-characteristics ((aut abstract-automaton) &key (stream t) (newline t))
  (declare (ignore aut) (ignore stream) (ignore newline)))

(defmethod show-automaton-characteristics  ((aut table-automaton) &key (stream t) (newline t))
  (declare (ignore newline))
  (when aut
    (format stream " ~D states" (container-size (get-states aut)))
    (format stream " ~D rules~:[~;*~]" 
	    (number-of-transitions (transitions-of aut)) *commutative-symbols*)))

(defmethod check-states-integrity ((automaton table-automaton))
  (null
   (set-difference 
    (contents (get-states-from (transitions-of automaton)))
    (get-states automaton))))

(defmethod right-handsides ((automaton table-automaton))
  (right-handsides (transitions-of automaton)))

(defmethod automaton-emptiness ((automaton table-automaton))
  (let ((aut (reduce-automaton automaton)))
    (or (empty-container-p (get-finalstates aut))
	(values nil (non-emptiness-witness automaton)))))

(defparameter *save-automata* nil)

(defmethod save-the-automaton ((a abstract-automaton))
  (save-the-automaton (compile-automaton a)))

(defmethod save-the-automaton ((a table-automaton))
  (when *save-automata*
    (let ((path (format nil "Data/~A.txt" (name a))))
      (with-open-file
	  (out path :direction :output :if-exists :supersede)
	(format out "Ops ")
	(format out "~A~%"(signature a))
	(let ((*loadable* t))
	  (show a out)))))
  a)
 
; destructive
(defmethod nindex-states ((automaton table-automaton) (n integer))
  (let ((casted-states (contents (get-states automaton))))
    (dolist (casted-state casted-states n)
      (nindex-state casted-state n)))
  automaton)

(defmethod index-states ((automaton table-automaton) (n integer))
  (nindex-states (duplicate-automaton automaton) n))

; destructive
(defun nindex-states-automata (automata &optional (n -1))
  (loop
    for automaton in automata
    do (nindex-states automaton (incf n))
    finally (return n)))

(defgeneric rename-and-save (name automaton))

(defmethod rename-and-save (name (a table-automaton))
  (rename-object a name)
  (save-the-automaton a))
 
(defmethod casted-p ((f table-automaton)) t)
