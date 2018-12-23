(in-package :nautowrite)

(defun rank (x l)
  (dotimes (i (length l) -1)
    (when (eq x (pop l))
	(return i))))

(defgeneric find-dt (index-point schemes)
  (:documentation "differenciating term at INDEX-POINT according to SCHEMES"))

(defmethod find-dt ((i index-point) schemes)
  (let* ((subschemes (remove-if-not #'subscheme-p (strict-subterms-list schemes)))
	 (subscheme (find-if (lambda (x) (constructor-symbols-p x (dsymbols schemes))) subschemes))
	 (ssn (+ (length schemes) (rank subscheme subschemes))))
    (do* ((s i (delta k s))
	  (k (root (aref (prefixes (index s)) ssn)) (root (aref (prefixes (index s)) ssn))))
	 ((not (delta k s)) (extension (prefix s) (pos (index s)) k)))))

(defgeneric transform-fb-rules (differenciating-term rules &optional elimination)
  (:documentation ""))

(defgeneric rules-with-elimination (dterm rules)
  (:documentation "differenciating term, rules"))

(defmethod rules-with-elimination ((dt term) (rules rules))
  (let* ((omegas (omega-positions dt))
	 (lomegas (length omegas))
	 (sym (make-aux-symbol lomegas))
	 (dtt (term-from-scheme dt))
	 (newrules (list
		    (make-rule dtt
			       (term-from-scheme
				(build-term sym (make-list lomegas :initial-element (omega-term))))))))
    (dolist (rule (rules-list rules) (make-rules newrules))
      (let* ((lh (left-handside rule))
	     (rh (right-handside rule))
	     (l (scheme-from-term lh))
	     (sl (remove-if-not (lambda (x) (le dt x)) (subterms l))))
	(if (endp sl)
	    (push (make-rule (my-copy-term lh) (my-copy-term rh)) newrules)
	    (dolist (u (non-omega-positions l) (reverse newrules))
	      (let ((st (term-at-position lh u)))
		(when (le dt (scheme-from-term st))
		  (push
		   (make-rule
		    (replace-term-at-position
		     lh u
		     (build-term sym
				 (mapcar (lambda (omega)
					   (term-at-position st omega))
					 omegas)))
		    rh)
		   newrules)))))))))

(defmethod rules-without-elimination ((dt term) (rules rules))
  (let* ((sym (make-aux-symbol (arity (root dt))))
	 (dtt (term-from-scheme dt))
	 (newrules (list (make-rule dtt (build-term sym (arg dtt))))))
    (dolist (rule (rules-list rules) (make-rules (reverse newrules)))
      (let* ((lh (left-handside rule))
	     (rh (right-handside rule))
	     (l (scheme-from-term lh))
	     (sl (remove-if-not (lambda (x) (le dt x)) (subterms l))))
	(if (endp sl)
	    (push (make-rule (my-copy-term lh) (my-copy-term rh)) newrules)
	    (dolist (u (non-omega-positions l) newrules)
	      (let ((st (term-at-position lh u)))
		(when (le dt (scheme-from-term st))
		  (push 
		   (make-rule (my-copy-term (replace-term-at-position lh u (build-term sym (arg st))))
			      (my-copy-term rh))
		   newrules)))))))))

(defmethod transform-fb-rules ((dt term) (rules rules) &optional (elimination nil))
  (if elimination
      (rules-with-elimination dt rules)
      (rules-without-elimination dt rules)))

(defgeneric transform-fb (rules &optional elimination)
  (:documentation "transforms forward-branching RULES to contructor rules"))

(defmethod transform-fb ((rules rules) &optional (elimination nil))
  (let* ((schemes (schemes-from-terms (left-handsides rules)))
	 (*dsymbols* (dsymbols schemes))
	 (so (forward-branching-index-tree schemes)))
    (unless so
      (return-from transform-fb))
    (let* ((it (make-index-tree so))
	   (failure-states (make-empty-ordered-container))
	   (*automaton-states-table* (states-table (transitions-of it))))
      (mapc (lambda (s)
	      (let ((fs (phi (state s))))
		(when fs
		  (container-nunion failure-states (cast-state fs) ))))
	    (contents (get-states it)))
      (if (null (cdr (contents failure-states)))
	  rules
	  (transform-fb 
	   (transform-fb-rules (find-dt so schemes) rules elimination)
	   elimination)))))

(defgeneric salinier (rules &optional elimination)
  (:documentation "tranforms forward-branching rules to constructor fb rules"))

(defmethod salinier ((rules rules) &optional (elimination nil))
  (reset-aux-symbols)
  (transform-fb rules elimination))

(defmethod salinier ((sys trs) &optional (elimination nil))
  (let ((newrules (salinier (get-rules sys) elimination)))
    (make-trs (compose-name (name sys) "-constructor")
	      newrules
	      (merge-signature (signature sys) (signature-from newrules)))))
