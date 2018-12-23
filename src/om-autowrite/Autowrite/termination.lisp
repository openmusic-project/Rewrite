(in-package :nautowrite)
(defgeneric make-automaton-context (gterm signature)
  (:documentation ""))

(defgeneric inverse-seqsys (trs)
  (:documentation ""))
(defgeneric constant-instances (term csign)
  (:documentation ""))
(defgeneric constant-instances-list (terms csign)
  (:documentation ""))
(defgeneric instances (term terms)
  (:documentation ""))
(defgeneric instances-list (terms sterms)
  (:documentation ""))
(defgeneric check-rule-substi-termination (lh rh itrs)
  (:documentation ""))
(defgeneric check-rule-termination (rule itrs slh all)
  (:documentation ""))
(defgeneric growing-terminating (trs)
  (:documentation ""))
(defgeneric check-rule-growing-termination (rule trs slg)
  (:documentation ""))
(defgeneric check-rule-substi-growing-termination (lhg rhg trs)
  (:documentation ""))
(defgeneric inverse-growing-terminating (trs)
  (:documentation ""))

(defmethod inverse-seqsys ((trs seqsys))
  (let ((rules (inverse-rules (get-rules trs))))
    (make-instance 'seqsys
		   :name (concatenate 'string (name trs) "(-1)")
		   :rules rules
		   :extra (get-extra trs)
		   :lhs (left-handsides rules)
		   :signature (copy-signature (signature trs)))))

(defgeneric  make-automaton-context (ground-term signature)
  (:documentation "build automaton recognizing terms of the form C[ground-term]"))

(defmethod make-automaton-context ((gt term) (signature signature))
  (assert (ground gt))
  (with-new-transitions
    (let ((qkey (make-qkey gt)))
     (add-transition-to-transitions qkey (r-state) *global-transitions*)
      (matching-transitions (subterms gt))
      (redex-transitions (list gt) nil)
      (reducible-transitions (non-constant-signature signature)))
    (make-automaton
     *global-transitions*
     :name "context-l"
     :signature (copy-signature signature)
     :finalstates (make-container-from-state (casted-r-state)))))

(defmethod constant-instances ((term term) (csign signature))
  (let ((vars (vars-of term)))
    (mapcar (lambda (substi)
	     (apply-substitution term substi))
	    (g-gen-substitution vars (mapcar #'build-term (signature-symbols csign))))))

(defmethod constant-instances-list ((terms list) (csign signature))
    (remove-duplicates
     (mappend (lambda (term) (constant-instances term csign))
	     terms)
     :test #'compare-object))
				      

(defmethod instances ((term term) (terms list))
  (let ((vars (vars-of term)))
    (mapcar (lambda (substi)
	     (apply-substitution term substi))
	    (g-gen-substitution vars terms))))

(defmethod instances-list ((terms list) (sterms list))
    (remove-duplicates
     (mappend (lambda (term) (instances term sterms))
	     terms)
     :test #'compare-object))
				      
(defmethod check-rule-substi-termination ((lh term) (rh term) (itrs seqsys))
  (seqsys-change-aut-t
   itrs
   (aut-termset (make-termset
		 "rsubst" 
		 (list rh))))
  (let ((aut1 (seqsys-aut-c-t itrs))
	(aut2 (make-automaton-context
	       lh
	       (add-bullet-symbol (signature itrs)))))
    (intersection-emptiness aut1
			    aut2)))

;; faut-il mettre aussi un rh qui est une constante?
(defmethod check-rule-termination ((rule rule) (itrs seqsys) (slh list) (all list))
  (block nil
    (let* ((rh (right-handside rule))
	   (lh (left-handside rule))
	   (vrh (vars-of rh))
	   (vlh (my-setdifference (vars-of lh) vrh))
	   (substis (g-gen-substitution vrh all))
	   (substils (g-gen-substitution vlh slh)))
      (dolist (sub substis (values t nil))
	(dolist (subl substils)
	  (multiple-value-bind (res witness)
	      (check-rule-substi-growing-termination
	       (apply-substitution (apply-substitution lh sub) subl)
	       (apply-substitution rh sub)
	       itrs)
	    (unless res (return (values res witness)))))))))

(defmethod inverse-growing-terminating ((trs seqsys))
  (let ((itrs (inverse-seqsys trs)))
    (assert (growing (get-rules itrs)))
    (let* ((lhs (get-lhs trs))
 	   (srh (nreverse (strict-subterms-list (right-handsides (get-rules trs)))))
	   (slh (nreverse (strict-subterms-list lhs :var t)))
	   (slhg (constant-instances-list slh (add-bullet-symbol (constant-signature (signature trs)))))
	   (all  (my-union srh slhg)))
      (dolist (rule (rules-list (get-rules trs)) (values t nil))
	(multiple-value-bind (res witness)
	    (check-rule-termination rule itrs srh all)
	  (unless res (return-from inverse-growing-terminating (values res witness))))))))

(defmethod check-rule-substi-growing-termination ((lhg term) (rhg term) (trs seqsys))
  (seqsys-change-aut-t
   trs
   (make-automaton-context lhg (add-bullet-symbol (signature trs))))
  (let ((aut (seqsys-aut-c-t trs)))
    (not (recognized-p rhg  aut))))

(defmethod check-rule-growing-termination ((rule rule) (trs seqsys) (slg list))
  (block nil
    (let* ((rh (right-handside rule))
	   (lh (left-handside rule))
	   (vlh (vars-of lh))
	   (substis (g-gen-substitution vlh slg)))
      (dolist (sub substis (values t nil))
	(let ((lhg (apply-substitution lh sub))
	      (rhg (apply-substitution rh sub)))
	  (unless
	      (check-rule-substi-growing-termination lhg rhg trs)
	    (return lhg)))))))

(defmethod growing-terminating ((trs seqsys))
  (assert (growing trs))
  (let* ((rules (get-rules trs))
	 (lhs (get-lhs trs))
	 (slg (instances-list (strict-subterms-list lhs :var t) (list (bullet-term)))))
    (dolist (rule (rules-list rules) (values t nil))
      (multiple-value-bind (res witness)
	  (check-rule-growing-termination rule trs slg)
	(unless res (return-from growing-terminating (values res witness)))))))

