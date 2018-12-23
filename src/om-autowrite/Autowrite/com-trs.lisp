(in-package :nautowrite)

(defmacro trs-check-property (property)
  `(progn
    (when (trs *spec*)
      (format
       *output-stream*
       "~A is ~A~A~%"
       (name (trs *spec*))
       (if (,property (get-rules (trs *spec*))) "" "not ")
       (string-downcase (symbol-name ',property))))))

(defun display-sequential (res sys &key (extra nil))
  (format
   *output-stream*
   "trs ~A ~Ain CBN~A-~A"
   (name sys)
   (if res "" "not ")
   (if extra "@" "")
   (get-approx-name (seqsys-approx sys))))

(defun display-sequentiality-results (sys aut-d &key extra)
  (let ((res (empty-container-p (get-finalstates aut-d))))
    (display-sequential res sys :extra extra)
    (unless res
      (format *output-stream* " free term: ~A" (non-emptiness-witness aut-d)))))

(defun try-to-check-seq-with-properties (ctx)
  (cond
    ((and
      (orthogonal-p (left-handsides (get-rules ctx)))
      (rules-closed-p (get-approx-rules ctx)))
     (values t t))
    (t (values nil ctx))))

(defun trs-check-collapsing (sys)
  (when sys
    (let ((collapsing (collapsing-p (get-rules sys))))
      (format *output-stream* "~A is " (name sys))
      (if collapsing
	  (format *output-stream*
		  "collapsing witnessed by ~A~%" collapsing)
	  (format *output-stream* "not collapsing~%")))))

(defun trs-check-constructor (sys)
  (when sys
    (multiple-value-bind  (constructor witness)
	(constructor-p (get-rules sys))
      (format *output-stream* "~A is " (name sys))
      (if constructor
	  (format *output-stream* "constructor~%")
	  (format *output-stream* "not constructor witnessed by ~A~%" witness)))))

(defun trs-check-forward-branching (sys &key (def nil))
  (when sys
    (multiple-value-bind (fb witness)
	(if def
	    (forward-branching-def sys)
	    (forward-branching-index-tree sys))
      (format *output-stream* "~A " (name sys))
      (if fb
	  (format *output-stream* "is forward-branching~%")
	  (format *output-stream* "not forward-branching ~A~%" witness)))))

(defmethod salinier ((sys seqsys) &optional (elimination nil))
  (make-seqsys-from-trs (call-next-method sys elimination)))


(defun trs-check-orthogonal (sys)
  (when (trs *spec*)
    (let ((orthogonal (and (not (overlapp  (get-lhs sys)))
			   (left-linear (get-rules sys)))))
      (format *output-stream* "~A is~:[ not~;~] orthogonal~%" (name sys) orthogonal))))

(defun trs-check-growing (sys)
  (when (trs *spec*)
    (let ((growing (growing sys)))
      (format *output-stream* "~A is~:[ not~;~] growing~%" (name sys) growing))))

(defun trs-check-overlapping (sys)
  (when (trs *spec*)
    (let ((overlapp (overlapp (get-lhs sys))))
      (format *output-stream*
	      "~A is~A overlapping" (name sys) (if overlapp "" " not"))
      (if overlapp
	  (format *output-stream* " witnessed by ~A~%" overlapp)
	  (format *output-stream* "~%")))))

(defun list-trss ()
  (list-keys (table (trss *spec*))))

(defun set-current-trs (trs)
  (setf (trs *spec*) trs)
  (add-current-trs))

(defun seqsys-from-rules (name rules)
  (make-seqsys name rules :signature (signature *spec*)))

(defun nf-empty ()
  (when (trs *spec*)
    (multiple-value-bind (res term)
	(automaton-emptiness (seqsys-aut-nf (trs *spec*)))
      (if res
	  (format *output-stream* "NF empty")
	  (progn
	    (format *output-stream* "NF not empty witnessed by ~A" term))))))

(defun enf-empty ()
  (when (trs *spec*)
    (multiple-value-bind (res term)
	(inclusion-automaton
	 (seqsys-aut-nf (trs *spec*))
	 (make-matching-automaton (get-lhs (trs *spec*)) (seqsys-signature (trs *spec*)) :strict t :finalstates t))
      (if res
	  (format *output-stream* "ENF empty")
	  (format *output-stream* "ENF not empty witnessed by ~A" term)))))
