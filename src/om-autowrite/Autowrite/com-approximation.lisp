(in-package :nautowrite)

(defmacro update-approximation (approximation)
  `(progn 
    (setf *approximation* ,approximation)
    (when (trs *spec*)
      (seqsys-change-approx (trs *spec*) ,approximation))))

(defun wn-inclusion (&key (bullet nil))
  (let ((aut-c-extra (seqsys-aut-c (trs *spec*) :bullet bullet :extra t)))
    (multiple-value-bind (res term)
	(inclusion-automaton
	 (automaton-without-symbol *extra-symbol* aut-c-extra)
	 (seqsys-aut-c (trs *spec*) :bullet bullet))
	(if res
	    (if bullet
		(format *output-stream* "WNo(S,G,F) == WNo(S,F)")
	    (format *output-stream* "WN(S,G,F) == WN(S,F)"))
	    (progn
	    (if bullet
		(format *output-stream*
			"WNo(S,G,F) not included in WNo(S,F)")
		(format *output-stream*
			"WN(S,G,F) not included in WN(S,F)"))
	    (format *output-stream* " witnessed by ~A" term))))))

(defun approx-closed ()
  (if (rules-closed-p (get-approx-rules (trs *spec*)))
      (format *output-stream* "approx closed~%")
      (format *output-stream* "approx not closed~%")))

(defun arbitrary ()
  (let ((trs (trs *spec*)))
    (when trs
      (or
       (find-if #'arbitrary-rule-p (rules-list (get-approx-rules trs)))
       (and
	(find-if #'collapsing-rule-p (rules-list (get-approx-rules trs)))
	(progn
	  (seqsys-change-aut-t
	   trs
	   (aut-termset (make-termset "Extra" (list (extra-term)))))
	  (let* ((aut1  (automaton-without-symbol
			 *extra-symbol*
			 (seqsys-aut-c-t trs)))
		 (aut2
		  (let ((signature (signature aut1))
			(lhs  (get-lhs trs)))
		    (make-b-automaton lhs signature 4 :finalstates t))))
	    (prog1
		(multiple-value-bind (res witness)
		    (intersection-emptiness aut1 aut2)
		  (and (not res) witness))
	      (seqsys-change-aut-t trs nil)))))))))

(defun approx-arbitrary ()
  (let ((arbitrary (arbitrary)))
    (format *output-stream* "approx trs ")
    (if arbitrary
	(format *output-stream*
		"arbitrary witnessed by ~A" arbitrary)
        (format *output-stream* "not arbitrary"))))
