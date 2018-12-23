(in-package :nautowrite)

(defmethod show-state-term-table ((table hash-table))
  (format *trace-output* "count ~A ~%" (hash-table-count table))
  (maphash
   (lambda (state term)
     (format *trace-output* "~A : ~A~%" state term))
   table))

(defmethod term-from-state-term-table ((table hash-table) (state abstract-state))
  (let ((flat-term (gethash state table)))
    (if (constant flat-term)
	flat-term
	(build-term
	 (root flat-term)
	 (mapcar (lambda (q)
		   (term-from-state-term-table table q))
		 (arg flat-term))))))

(defmethod state-term-table (stop-fun (automaton abstract-automaton))
  (assert (casted-p automaton))
  (let* ((transitions (transitions-of automaton))
	 (signature (signature automaton))
	 (csign (constant-signature signature))
	 (ncsign (non-constant-signature signature))
	 (ncsym (signature-symbols ncsign))
	 (amax (max-arity ncsign))
	 (table (make-hash-table :test #'eq))
	 (states nil)
	 (newstates nil))
    (with-states-table (transitions-of automaton)
    (loop
      for sym in (signature-symbols csign)
      do (let ((target (apply-transition-function-gft sym '() transitions)))
	   (loop
	     for q in (contents target)
	     do (progn
		  (setf (gethash q table) (build-term sym))
		  (setf newstates (target-union q newstates))))))
    (labels ((add (sym arg)
	       (loop
		 for q in (contents
			    (apply-transition-function-gft
				  sym
				  arg
				  transitions))
		 do (progn
		      (let ((old (gethash q table)))
			(unless old
			  (setf (gethash q table) (build-term sym arg))
			  (setf newstates (target-union q newstates)))))))
	     (fs-foundp ()
	       (find-if
		(lambda (state)
		  (funcall stop-fun state))
		(contents newstates))))
      (loop
	for sym in (signature-symbols csign)
	do (add sym nil))
      (loop
	 for fs = (fs-foundp) then (fs-foundp)
	 for oldstates = (contents states) then (contents states)
;;	 do (show-state-term-table table)
;;	 do (format *trace-output* "keys ~A~%" (list-keys table))
;;	 do (format *trace-output* "keys ~A~%" (list-values table))
	 when fs
	 do (return-from state-term-table (values table fs))
;;	 do (format *trace-output* "count ~A~%" (hash-table-count table))
	 do (setf states (target-union states newstates))
	 until (target-empty-p newstates)
	 do (setf newstates nil)
	 do (let ((afa (arrange-for-arities-and-filter
			(contents states) amax oldstates ncsign)))
	      (loop
		 for sym in ncsym
		 do (loop
		       for arg in (aref afa (arity sym))
		       do (add sym arg))
		   ))
	 finally (return (values table nil)))))))

(defmethod term-to-given-state ((abstract-automaton abstract-automaton) (state abstract-state))
  (unless (casted-p abstract-automaton)
    (setf abstract-automaton (cast-automaton abstract-automaton)))
  (multiple-value-bind (table term)
      (state-term-table (lambda (s) (eq s state)) abstract-automaton)
    (declare (ignore table))
    term))
