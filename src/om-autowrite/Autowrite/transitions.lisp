(in-package :nautowrite)
(defvar *loadable* nil)

;; explicit transitions (represented in extension)
;;; in a transitions-table, the value is always a container of states
;;; accessible from the corresponding lh (the key).
;;; in a dtransitions-table the value is a state

(defvar *limit-transitions* t)
(defvar *limit-transitions-value* 100)

(defun toggle-limit-transitions ()
  (setf *limit-transitions* (not *limit-transitions*)))

(defvar *global-transitions*)

(defmacro with-new-transitions (&body body)
  `(let* ((*global-transitions* (make-empty-transitions))
	  (*automaton-states-table* (states-table *global-transitions*)))
     ,@body))

(defun set-new-transitions ()
  (setf *global-transitions* (make-empty-transitions))
  (setf *automaton-states-table* (states-table *global-transitions*)))

(defvar *transitions-type* 'sym-transitions)

(defun state-or-container-p (target)
  (or (state-p target) (container-p target)))

(defgeneric number-of-transitions (transitions)
  (:documentation ""))

(defgeneric transitions-access (key transitions)
  (:documentation ""))

(defgeneric transition-set (key rh transitions)
  (:documentation ""))

(defgeneric transitions-access-modc (key transitions)
  (:documentation ""))

(defgeneric transitions-table (transitions))

(defclass transitions (abstract-transitions cast-states-mixin)
  ((transitions-table :initarg :transitions-table :accessor transitions-table)))

(defgeneric remove-those-with-symbols (symbols transitions)
  (:documentation "return TRANSITIONS without the ones SYMBOL at the root of the left-handside"))

(defgeneric transitions-list (transitions)
  (:documentation "return a list of individual transitions from transitions"))

(defgeneric display-transitions (transitions &optional stream)
  (:documentation "displays transitions in any form"))

(defgeneric make-transitions (transitions transitions-table)
  (:documentation "return an object transitions containing the rules in TRANSITIONS"))

(defgeneric arg-modc (root arg))

(defgeneric key-modc (root arg))

(defgeneric display-transitions-list (transitions &optional stream))

(defgeneric add-transition-to-transitions (key rh transitions)
  (:documentation "adds the transition KEY -> RH to TRANSITIONS returns T if something new"))

(defgeneric add-transitions-to-transitions (new-transitions transitions)
  (:documentation "adds theNEWTRANSITION to TRANSITIONS"))

(defgeneric add-dtransition-to-transitions (key rh transitions)
  (:documentation "adds the dtransition KEY -> RH to TRANSITIONS returns T if something new"))

(defgeneric key-value-list (transitions)
  (:documentation "only for deterministic transitions"))

(defgeneric make-empty-simple-transitions ())

(defgeneric cright-handsides (transitions))

(defgeneric transition-set (key rh transitions))

(defgeneric get-states-from (transitions)
  (:documentation "obtain the states of the transitions"))

(defgeneric get-states-from (transition)
  (:documentation "obtain the states of a transition"))

(defgeneric nfilter-with-states (accessible-states transitions)
  (:documentation
   "destructive remove transitions whose left-handside has a not accessible state"))

(defgeneric table-filter-with-states (accessible-states transitions))
(defgeneric table-apply-signature-mapping (transitions))

(defmethod nfilter-with-states ((accessible-states ordered-container) (transitions transitions))
  (setf (transitions-table transitions)
	(table-filter-with-states accessible-states transitions))
  (nfilter-states-table accessible-states (states-table transitions)))
  
(defmethod napply-signature-mapping ((transitions transitions))
  (setf
   (transitions-table transitions)
   (table-apply-signature-mapping transitions)))

(defgeneric complete-transitions (puits signature afa transitions &key deterministic)
  (:documentation "destructive completion of transitions"))

(defmethod fill-states-table ((transitions transitions))
  (let ((*automaton-states-table* (states-table transitions)))
    (when (zerop (hash-table-count *automaton-states-table*))
      (let ((casted-states (get-states-from transitions)))
	(mapc (lambda (casted-state)
		(let ((key (state-key (state casted-state))))
		  (put-state-in-automaton-states-table key casted-state)))
	      (contents casted-states))))
    (states-table transitions)))

(defmethod get-states ((transitions transitions))
;;  (reset-states-table transitions)
  (when (zerop (hash-table-count (states-table transitions)))
    (fill-states-table transitions))
  (let ((states
	 (states-from-states-table (states-table transitions))))
    (when *debug*
      (let ((states2 (get-states-from transitions)))
	(unless (= (container-size states) (container-size states2))
	  (warn "states-table and transitions incoherent"))))
    states))

;; specialized version when transitions are stored
(defmethod apply-transition-function ((root abstract-arity-symbol) (states list) (transitions transitions))
  (transitions-access (key-modc root states) transitions))

(defmethod apply-transition-function
    ((root unranked-symbol) (states list) (transitions transitions))
  (transitions-access (key-modc root states) transitions))

(defmethod transitions-fun ((transitions transitions))
  (lambda (root states)
    (apply-transition-function root states transitions)))

(defmethod transition-function-set ((root abstract-symbol) (states list) rh (transitions transitions))
  (transition-set (key-modc root states) rh transitions))

(defmethod arg-modc ((root abstract-symbol) (arg (eql nil)))
  (declare (ignore root))
  nil)

(defmethod arg-modc ((root abstract-symbol) arg)
  (declare (ignore root))
  arg)

(defmethod arg-modc ((root commutative-symbol-mixin) arg)
  (if (and arg (casted-state-p (first arg)))
      (sort-casted-states (copy-list arg))
      arg))

(defmethod key-modc (root arg)
  (cons root (arg-modc root arg)))

(defmethod right-handsides ((transitions list))
  (my-delete-duplicates (mapcar #'right-handside transitions)))

(defmethod display-transitions-list ((transitions list) &optional (stream t))
  (mapc (lambda (transition)
	  (format stream "~A~%" transition))
	transitions)
  nil)

(defmethod show :around ((transitions transitions) &optional (stream t))
  (let ((len (number-of-transitions transitions)))
    (if (and (not *loadable*) *limit-transitions* (> len *limit-transitions-value*))
	(format stream "~A" len)
	(call-next-method))))

(defmethod add-transitions-to-transitions ((newt transitions) (transitions transitions))
  (let ((key-value-list (key-value-list newt)))
    (dolist (kv key-value-list transitions)
      (add-transition-to-transitions (car kv) (cadr kv) transitions))))

(defun make-empty-table (transitions-type)
  (if (eq transitions-type 'sym-transitions)
      (make-empty-dag-table)
      (make-empty-simple-table)))
      
(defmethod make-empty-transitions (&optional (transitions-type *transitions-type*))
  (make-instance transitions-type :transitions-table (make-empty-table transitions-type)))

(defmethod make-transitions (transitions-type (transitions-table hash-table))
  (make-instance transitions-type :transitions-table transitions-table))

(defmethod add-transition-to-transitions ((key list) (target (eql nil)) (transitions transitions)))

(defmethod add-transition-to-transitions ((key list) target (transitions transitions))
;;  (format *error-output* "add-transition-to-transitions target container ~A -> ~A ~%" key target)
  (unless (casted-target-p target)
    (setf target (cast-target target)))
  (let* ((oldtarget (apply-transition-function (car key) (cdr key) transitions))
	 (newtarget (target-union oldtarget target)))
    (transition-function-set (car key) (cdr key) newtarget transitions)
    (> (target-size newtarget) (target-size oldtarget))))

;; adds f(q1, ..., qn) -> q rule replacing previous such rules
(defmethod add-dtransition-to-transitions (key (rh abstract-state) (transitions transitions))
  (add-dtransition-to-transitions key (cast-state rh) transitions))

(defmethod add-dtransition-to-transitions (key (rh container) (transitions transitions))
;;    (format *error-output* "add-dtransition-to-transitions key:~A  rh:~A~%" key rh)
  (add-dtransition-to-transitions key (cast-state (make-gstate rh)) transitions))

(defmethod add-dtransition-to-transitions (key (rh (eql nil)) (transitions transitions)))

(defmethod add-dtransition-to-transitions (key (rh casted-state) (transitions transitions))
  (let ((oldstate (apply-transition-function (car key) (cdr key) transitions)))
;;    (format *error-output* "add-dtransition-to-transitions key:~A oldstate:~A rh:~A~%" key oldstate rh)
    (unless (eq oldstate rh)
      (transition-function-set (car key) (cdr key) rh transitions)
      (return-from add-dtransition-to-transitions t))))

(defmethod disjoint-union-transitions-gen ((transitionss list))
  (let ((new-transitions (make-empty-transitions)))
    (mapc
     (lambda (transitions)
       (add-transitions-to-transitions transitions new-transitions))
     transitionss)
    (fill-states-table new-transitions)
    new-transitions))

;; vaut pour le cas deterministe aussi car (contents states) -> (state)

(defmethod right-handsides ((transitions transitions))
  (contents (cright-handsides transitions)))

(defmethod make-transitions-from-list ((transitions list))
  (let ((newtransitions (make-empty-transitions)))
    (dolist (transition transitions newtransitions)
      (let ((lh (left-handside transition)))
;;	(format *error-output* "~A -> ~A~%" lh (right-handside transition))
	(add-transition-to-transitions
	 (flat-term-to-key lh)
	 (right-handside transition)
	 newtransitions)))))

(defmethod symbols-from ((transitions transitions))
  (symbols-from (remove-if 'state-p (left-handsides transitions))))

;; destructive
(defmethod complete-transitions (puits signature afa (transitions transitions)
				 &key (deterministic t))
  (mapc
   (lambda (symbol)
     (let ((afai (aref afa (arity symbol))))
       (mapc
	(lambda (arg)
	  (let ((target
		 (apply-transition-function
		  symbol
		  arg
		  transitions)))
	    (when (target-empty-p target)
	      (let ((puits-target
		     (if deterministic
			 puits
			 (make-container-from-state puits))))
		(transition-function-set symbol arg  puits-target transitions)))))
	afai)))
   (signature-symbols signature)))

(defmethod commutative-transitions-p ((transitions transitions))
  (> (number-of-transitions
      (uncommutative transitions))
     (number-of-transitions transitions)))

(defgeneric uncommutative-for-equivalence-classes (transitions))