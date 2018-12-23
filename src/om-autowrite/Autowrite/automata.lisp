(in-package :nautowrite)
;;; some particular useful automata for TRSs

(defun matching-transitions (terms)
;;  (mapcar
  (mapc
   (lambda (term)
     (let ((qkey (make-qkey term)))
       (add-transition-to-transitions
	qkey
	(make-casted-term-state term)
	*global-transitions*)))
     terms))

(defun propagation-transitions (signature)
  (mapc
   (lambda (x)
     (add-transition-to-transitions
      (cons x (casted-var-states (arity x)))
      (casted-var-state)
      *global-transitions*))
   (signature-symbols signature)))

(defgeneric make-qkey (term))

(defmethod make-qkey ((term var))
  (make-casted-term-state term))

(defmethod make-qkey ((term term))
  (let ((linear-term (linearize term)))
    (cons
     (root linear-term)
     (make-casted-term-states (arg linear-term)))))

(defun casted-var-states (a)
  (make-list a :initial-element (casted-var-state)))

(defun make-matching-automaton (lhs signature &key (strict nil) (finalstates nil) (subterms t))
  (with-new-transitions
    (let* ((sub (if strict
		    (strict-subterms-list lhs)
		    (subterms-list lhs)))
	   (fs (and finalstates
		    (make-casted-term-states
		     (if subterms sub lhs)))))

      (matching-transitions sub)
      (propagation-transitions signature)
    (make-automaton
       *global-transitions*
       :name "matching"
       :signature signature
       :finalstates (make-ordered-container fs)))))

(defgeneric  make-automaton-universal (signature)
  (:documentation "return automaton recognizing all ground terms built with SIGNATURE"))

(defmethod make-automaton-universal ((signature signature))
  (with-new-transitions
    (propagation-transitions signature)
    (make-automaton *global-transitions*
		    :name "universal"
		    :signature (copy-signature signature)
		    :finalstates (get-states
				  *global-transitions*))))

(defun make-redex-automaton (lhs signature)
  (let ((aut-redex (make-matching-automaton lhs signature :finalstates t :subterms nil)))
    (rename-object aut-redex "redex")
    aut-redex))

(defun make-reducible-automaton (lhs signature)
  (with-new-transitions
    (reducible-transitions (non-constant-signature signature))
    (redex-transitions lhs nil)
    (propagation-transitions signature)
    (matching-transitions (subterms-list lhs))
    (make-automaton
     *global-transitions*
     :name "reducible"
     :signature signature
     :finalstates (make-container-from-state (casted-r-state)))))

