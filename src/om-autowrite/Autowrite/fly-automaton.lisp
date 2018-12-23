(in-package :nautowrite)

(defclass fly-automaton (abstract-automaton)
  ((final-state-fun :initarg :final-state-fun :reader final-state-fun)
   (puits-state-fun :initarg :puits-state-fun :reader puits-state-fun)
   (complete :initarg :complete :reader complete-p)))

(defclass fly-cast-automaton (fly-automaton) ())
(defclass fly-casting-automaton (fly-cast-automaton) ())
(defclass fly-table-automaton (fly-cast-automaton) ())

(defmethod nreduce-automaton ((fly-automaton fly-automaton))
  fly-automaton)

(defmethod automaton-name ((f fly-casting-automaton))
  (format nil "~A-casting" (name f)))

(defmethod casted-p ((f fly-automaton))
  (fly-casted-transitions-p (transitions-of f)))

(defmethod casted-p ((f fly-casting-automaton)) t)

(defmethod final-state-p ((casted-state casted-state) (f fly-casting-automaton))
;;  (format *error-output* "final-state-p casted-state fly-casting-automaton ~A ~A~%" casted-state f)
  (funcall (final-state-fun f) (state casted-state)))

(defmethod puits-state-p ((casted-state casted-state) (f fly-casting-automaton))
  (funcall (puits-state-fun f) (state casted-state)))

(defmethod final-state-p ((indexed-state indexed-state) (f fly-automaton))
;;  (format *error-output* "final-state-p indexed-state fly-automaton ~A ~A~%" indexed-state f)
  (final-state-p (state indexed-state) f))

(defmethod puits-state-p ((indexed-state indexed-state) (f fly-automaton))
  (puits-state-p (state indexed-state) f))

(defmethod puits-state-p ((casted-state casted-state) (f fly-casting-automaton))
  (funcall (puits-state-fun f) (state casted-state)))

(defparameter *cast* nil)

(defun make-fly-automaton
    (signature tfun
     &key
     (final-state-fun #'state-final-p)
     (puits-state-fun #'state-puits-p)
     name
     (deterministic t)
     (complete nil)
     (cast *cast*))
  (make-instance
   (if cast 'fly-casting-automaton 'fly-automaton)
   :name name
   :signature signature
   :transitions (make-fly-transitions tfun cast :deterministic deterministic)
   :complete complete
   :final-state-fun final-state-fun
   :puits-state-fun puits-state-fun))
    
(defgeneric cast-automaton (a))
(defmethod cast-automaton ((f fly-cast-automaton)) f)

(defmethod cast-automaton ((f fly-automaton))
  (make-fly-automaton
   (signature f) (tfun (transitions-of f))
   :name (name f)
   :final-state-fun (final-state-fun f)
   :puits-state-fun (puits-state-fun f)
   :complete (complete-p f)
   :deterministic (deterministic-p f)
   :cast t))

(defmethod cast-automaton ((a table-automaton)) a)

(defmethod reduced-p ((fly-automaton fly-automaton))
  (declare (ignore fly-automaton)) t)

(defmethod complement-final-state-fun ((f fly-automaton))
  (assert (deterministic-p f))
  (lambda (state)
;;    (print 'coucou)
    (not (final-state-p state f))))

(defmethod det-final-state-fun ((f fly-automaton))
  (lambda (gstate) ;; gstate or nil
    (when gstate
      (assert (typep gstate 'gstate))
      (and
       (some
	(lambda (state) (final-state-p state f))
	(contents (container gstate)))
       t))))

(defmethod determinize-automaton ((f fly-automaton) &key (fly t))
  (when (deterministic-p f)
    (return-from determinize-automaton f))
  (let
      ((df
	(make-fly-automaton
	 (signature f)
	 (det-transitions-fun (transitions-of f))
	 :complete (complete-p f)
	 :final-state-fun (det-final-state-fun f)
	 :name (format nil "~A-det" (automaton-name f)))))
    (if fly df (compile-automaton f))))

(defmethod complete-automaton ((f fly-table-automaton))
  (assert (complete-p f))
  (to-fly-automaton (compile-automaton f)))

(defmethod complete-automaton ((f fly-automaton))
  (when (complete-p f)
    (return-from complete-automaton f))
  (make-instance
   (type-of f)
   :signature  (signature f)
   :transitions (complete-fly-transitions (transitions-of f))
   :complete t
   :final-state-fun (final-state-fun f)
   :name (automaton-name f)))

(defmethod complement-automaton ((f fly-automaton))
  (setf f (complete-automaton (determinize-automaton f)))
  (make-instance
   (type-of f)
   :signature (signature f)
   :transitions (duplicate-transitions (transitions-of f))
   :complete t
   :final-state-fun (complement-final-state-fun f)
   :puits-state-fun (lambda (state) (declare (ignore state)) nil)
   :name (complement-automaton-name (automaton-name f))))

(defmethod product-automaton ((f1 fly-automaton) (f2 fly-automaton))
  (make-fly-automaton
   (merge-signature (signature f1) (signature f2))
   (product-transitions-fun (transitions-of f1) (transitions-of f2))
   :complete (and (complete-p f1) (complete-p f2))
   :final-state-fun (lambda (state) (declare (ignore state)) nil)
   :deterministic (and (deterministic-p f1) (deterministic-p f2))
   :name (format nil "X~A-~A" (automaton-name f1) (automaton-name f2))))

(defmethod product-automaton-gen (&rest automata)
  (make-fly-automaton
   (apply #'merge-signature (mapcar #'signature automata))
   (apply #'product-transitions-fun-gen
	  (mapcar #'transitions-of automata))
   :complete (every #'complete-p automata)
   :final-state-fun (lambda (state) (declare (ignore state)) nil)
   :puits-state-fun (lambda (state) (declare (ignore state)) nil)
   :deterministic (every #'deterministic-p automata)
   :name (format nil "X(~A)" (mapcar #'automaton-name automata))))

(defmethod and-finalpstate-fun ((aut1 abstract-automaton) (aut2 abstract-automaton))
  (lambda (pstate)
    (and
     pstate
     (and (final-state-p (first (tuple pstate)) aut1)
	  (final-state-p (second (tuple pstate)) aut2)
	  t))))

(defmethod and-finalpstate-fun-gen (automata)
  (lambda (pstate)
    (and
     pstate
     (every (lambda (state automaton)
	      (final-state-p state automaton))
	    (tuple pstate)
	    automata))))

(defmethod or-finalpstate-fun-gen (automata)
  (lambda (pstate)
    (and
     pstate
     (some (lambda (state automaton)
	     (final-state-p state automaton))
	   (tuple pstate)
	   automata))))

(defmethod or-finalpstate-fun ((aut1 abstract-automaton) (aut2 abstract-automaton))
  (lambda (pstate)
    (and
     pstate
     (or (final-state-p (first (tuple pstate)) aut1)
	 (final-state-p (second (tuple pstate)) aut2))
     t)))

(defmethod intersection-automaton-compatible ((f1 fly-automaton) (f2 fly-automaton))
  (let ((f (product-automaton f1 f2)))
    (setf (slot-value f 'final-state-fun)
	  (and-finalpstate-fun f1 f2))
    (setf (name f) (format nil "I(~A ~A)" (automaton-name f1) (automaton-name f2)))
    f))

(defmethod intersection-automaton-compatible-gen (&rest automata)
  (let ((pautomaton (apply #'product-automaton-gen automata)))
    (setf (slot-value pautomaton 'final-state-fun)
	  (and-finalpstate-fun-gen automata))
    (setf (name pautomaton) (format nil "I~A"
				     (mapcar #'automaton-name automata)))
    pautomaton))

 (defmethod disjoint-union-automaton ((f1 fly-automaton) (f2 fly-automaton))
   (let ((if1 (index-states f1 0)) ;; fait une copie
	 (if2 (index-states f2 1)));; fait une copie
    (make-fly-automaton
     (signature f1)
     (lambda (root states)
       (target-union
        (apply-transition-function-gft 
	 root states (transitions-of if1))
	(apply-transition-function-gft 
          root states (transitions-of if2))))
     :deterministic nil ;; sauf peut-etre s'il sn'ont pas de constancte commune
     :name (format nil "U(~A ~A)" (automaton-name f1) (automaton-name f2))
     :final-state-fun
     (lambda (state)
       (or (final-state-p state if1)
	   (final-state-p state if2)))
     :complete (or (complete-p f1) (complete-p f2)))))

(defmethod union-automaton ((f1 fly-automaton) (f2 fly-automaton))
  :documentation "automata must have compatible signatures"
  (setf f1 (complete-automaton f1))
  (setf f2 (complete-automaton f2))
  (let ((f (product-automaton f1 f2)))
    (setf (slot-value f 'final-state-fun)
	  (or-finalpstate-fun f1 f2))
    (setf (name f) 
	  (format nil "Up(~A ~A)" (automaton-name f1) (automaton-name f2)))
    f))

(defmethod union-automaton-compatible-gen (&rest automata)
  (let ((pautomaton (apply #'product-automaton-gen
			   (mapcar #' complete-automaton automata))))
    (setf (slot-value pautomaton 'final-state-fun)
	  (or-finalpstate-fun-gen automata))
    (setf (name pautomaton ) (format nil "U~A"
				     (mapcar #'automaton-name automata)))
    pautomaton))

(defmethod apply-signature-mapping ((f fly-automaton))
  (make-instance
   (type-of f)
   :transitions (apply-signature-mapping (transitions-of f))
   :final-state-fun (slot-value f 'final-state-fun)
   :puits-state-fun (slot-value f 'puits-state-fun)
   :signature (apply-signature-mapping (signature f))
   :complete nil ;;????
   :name (format nil "fly-~A" (compose-name "asm" (automaton-name f)))))

(defmethod nautomaton-without-symbols ((symbols list) (fly-automaton fly-automaton))
  (setf (signature fly-automaton) (remove-symbols-from-signature symbols (signature fly-automaton)))
  fly-automaton)

(defmethod simplify-automaton ((automaton fly-automaton))
  automaton)

(defmethod simplify-automaton ((automaton fly-table-automaton))
  (let ((a (compile-automaton automaton)))
    (to-fly-automaton (simplify-automaton a))))

(defmethod skim-automaton ((a fly-automaton))
  a)

(defmethod nindex-states ((f fly-automaton) (index integer))
  (nindex-fly-transitions (transitions-of f) index)
  f)

(defmethod add-empty-symbol ((f fly-automaton))
  (let ((nf (duplicate-automaton f)))
    (setf (signature nf)
	  (adjoin-symbol-to-signature *empty-symbol* (signature nf)))
    nf))

(defmethod duplicate-automaton ((f fly-automaton))
  (make-instance
   (type-of f)
   :transitions (duplicate-transitions (slot-value f 'transitions))
   :final-state-fun (slot-value f 'final-state-fun)
   :puits-state-fun (slot-value f 'puits-state-fun)
   :complete (slot-value f 'complete)
   :name (slot-value f 'name)
   :signature (slot-value f 'signature)))

(defmethod index-states ((f fly-automaton) (index integer))
  (nindex-states (duplicate-automaton f) index))

(defmethod reset-states-table ((f fly-casting-automaton))
  (reset-states-table (transitions-of f)))

(defmethod states-table ((f fly-casting-automaton))
  (states-table (transitions-of f)))

(defmethod biggest-state ((f fly-casting-automaton))
  (biggest-state (states-table (transitions-of f))))