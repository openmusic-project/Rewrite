(in-package :gautowrite)

(defun process-approx-arbitrary ()
  (when (trs *spec*)
    (with-left-linearity (trs *spec*)
      (avec-temps-to-output-stream
       (in-process
	(approx-arbitrary)
       "ARBITRARY")))))

(defun process-wn-inclusion (&key (bullet nil))
  (when (trs *spec*)
    (with-left-linearity (trs *spec*)
      (in-process-with-time (wn-inclusion :bullet bullet) "WN-INCLUSION"))))

(defun process-determinize ()
  (in-process-with-time (determinize) "Determinize"))

(defun process-reduce ()
  (in-process-with-time (my-reduce) "Reduce"))

(defun process-complete ()
  (in-process-with-time (complete) "Complete"))

(defun process-complement ()
  (in-process-with-time (complementation) "Complement"))

(defun process-automaton-d (sys &key (deterministic t) (extra nil) (partial nil))
  (when sys
    (in-process-with-time (automaton-d sys
				       :deterministic deterministic
				       :extra extra
				       :partial partial)
			  (if extra
			      (if partial
				  "Automaton D Extra Partial"
				  "Automaton D Extra")
			      (if partial
				  "Automaton D Partial"
				  "Automaton D")))))

(defun process-automaton-c (sys &key (deterministic t) (bullet nil) (extra nil))
  (when sys
    (in-process-with-time
     (automaton-c sys :deterministic deterministic :bullet bullet :extra extra)
     (if extra
	 (if bullet
	     "C-NFo@"
	     "C-NF@")
	 (if bullet
	     "C-NFo"
	     "C-NF")))))

(defun process-automaton-c-s (trs termset &key (deterministic t))
  (when (and trs termset)
    (in-process-with-time
     (automaton-c-s trs termset :deterministic deterministic) "Automaton C-S")))

(defun process-automaton-c-a (trs automaton &key (deterministic t))
  (when (and trs automaton)
    (in-process-with-time
     (automaton-c-a trs automaton :deterministic deterministic)
     "Automaton C-A")))

(defun process-minimize ()
  (in-process-with-time (minimize) "Minimize"))

(defun process-simplify ()
  (in-process-with-time (simplify) "Simplify"))

(defun process-apply-reduction (&key (needed nil))
  (in-process
   (apply-reduction :needed needed) "Reduction"))

(defun process-apply-reduction-nf (&key needed)
  (in-process-with-time (apply-reduction-nf :needed needed) "Normalization"))

(defun process-normalizable (term sys &key (extra nil))
  (with-left-linearity (trs *spec*)
    (in-process-with-time (normalizable term sys :extra extra) "Normalizable")))

(defun process-term-needed-redexes (term sys &key (extra nil))
  (when term
    (with-left-linearity sys
      (in-process-with-time
         (progn
	   (term-needed-redexes term sys :extra extra)
	   (format *output-stream* "~%"))
       "Needed redexes"))))

(defun process-accessibility ()
  (when (trs *spec*)
    (with-left-linearity (trs *spec*)
      (in-process-with-time (accessibility) "ACCESSIBILITY"))))

(defun process-sequentiality (sys &key deterministic (extra nil) (automata t))
  (when sys
    (with-left-linearity sys
      (in-process-with-time
       (sequentiality sys
		      :deterministic deterministic
		      :extra extra :automata automata)
       "Call-by-Need"))))

(defun process-growing-terminating-trs (sys)
  (when sys
    (in-process-with-time
     (multiple-value-bind (res witness)
	 (growing-terminating sys)
       (format *output-stream* "~A is ~:[not ~;~]terminating~%"
	       (name sys)
	       res)
       (unless res (format *output-stream* "witnessed by ~A" witness)))
    "Growing Termination")))

(defun process-inverse-growing-terminating-trs (sys)
  (when sys
    (in-process-with-time
     (multiple-value-bind (res witness)
	 (inverse-growing-terminating sys)
       (format *output-stream* "~A is ~:[not ~;~]terminating~%"
	       (name sys)
	       res)
       (unless res (format *output-stream* "witnessed by ~A" witness)))
    "Inverse Growing Termination")))

(defun process-nf-empty ()
  (when (trs *spec*)
    (with-left-linearity (trs *spec*)
      (in-process-with-time (nf-empty) "NF-EMPTY"))))

(defun process-enf-empty ()
  (when (trs *spec*)
      (with-left-linearity (trs *spec*)
	(in-process-with-time (enf-empty) "ENF-EMPTY"))))

