(in-package :nautowrite)

(defun  place-r-for-arity  (arr)
  (do ((l nil (cons
	       (append (casted-var-states (1- i))
		       (list (casted-r-state))
		       (casted-var-states (- arr i)))
	       l))
       (i 1 (1+ i)))
      ((> i arr) l)))

(defun redex-transitions (lhs &optional (redex t))
  (mapc
   (lambda (term)
     (let ((qkey  (make-qkey term)))
       (add-transition-to-transitions
	qkey
	(r-state)
	*global-transitions*)))
   lhs)
  (when redex
    (mapc
     (lambda (term)
       (let ((qkey (make-qkey term)))
	 (add-transition-to-transitions
	  qkey (l-state) *global-transitions*)))
     lhs)))

(defun reducible-transitions (ncsignature)
  (mapc
   (lambda (sym)
     (mapc
      (lambda (arg)
	(add-transition-to-transitions
	 (cons sym arg)
	 (r-state)
	 *global-transitions*))
      (place-r-for-arity (arity sym))))
   (signature-symbols ncsignature)))

(defun make-b-automaton (lhs signature &key (bullet nil) (extra nil) (finalstates nil))
  (when extra
    (setf signature (add-extra-symbol signature)))
  (when bullet
    (setf signature (add-bullet-symbol signature)))
  (with-new-transitions
    (redex-transitions lhs)
    (reducible-transitions (non-constant-signature signature))
    (propagation-transitions signature)
    (matching-transitions (strict-subterms-list lhs))
    (make-automaton
     *global-transitions*
     :name "aut-b"
     :signature signature
     :finalstates (make-ordered-container (and finalstates (list (casted-r-state)))))))

