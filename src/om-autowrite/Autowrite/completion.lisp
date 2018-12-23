(in-package :gautowrite)

(defun suggest-arg (filename)
  (let ((str (file-namestring filename)))
    (if (member #\. (coerce str 'list))
	(concatenate 'string  (pathname-name filename) "." (pathname-type filename))
	(file-namestring filename))))

(define-presentation-method accept ((type data-file-name) stream
                    (view textual-view)
                    &key)
  (let* ((data-pattern (concatenate 'string *data-directory* (file-namestring pattern)))
	 (pathnames (directory data-pattern)))
    (handler-case
	(let ((filename 
	       (completing-from-suggestions
		(stream)
		(loop
		 for file in pathnames
		 do (suggest (suggest-arg file) file))
		)))
	  filename)
      (simple-parse-error ()
	  (prog1 nil
	    (format *output-stream* "no match~%")))
      (simple-completion-error ()
	  (prog1 nil
	    (format *output-stream* "no match~%")))
      (all-errors ()
	  (prog1 nil
	    (format *output-stream* "error"))))))

(define-presentation-type spec (&optional (pattern "*.*"))
)

(define-presentation-method accept ((type spec) stream
                                     (view textual-view)
                                     &key)
  (handler-case
     (let ((spec 
	    (completing-from-suggestions
	     (stream)
	     (loop
	      for spec in (list-values (table *specs*))
	      do
	      (suggest (name spec) spec)))))
       spec)
     (simple-completion-error ()
       (prog1 nil
	 (format *output-stream* "no match~%")))
     (all-errors ()
       (prog1 nil
	 (format *output-stream* "error")))))

(define-presentation-method accept ((type automaton) stream
                                     (view textual-view)
                                     &key)
  (handler-case 
     (completing-from-suggestions (stream)
       (loop
           for automaton in (list-values (table (automata *spec*)))
           do (suggest (name automaton) automaton)))
     (simple-completion-error ()
       (prog1 nil
	 (format *output-stream* "no match~%")))
     (all-errors ()
       (prog1 nil
	 (format *output-stream* "error")))))

(define-presentation-method accept ((type tautomaton) stream
                                     (view textual-view)
                                     &key)
  (handler-case 
     (completing-from-suggestions (stream)
       (loop
           for tautomaton in (list-values (table (tautomata *spec*)))
           do (suggest (name tautomaton) tautomaton)))
     (simple-completion-error ()
       (prog1 nil
	 (format *output-stream* "no match~%")))
     (all-errors ()
       (prog1 nil
	 (format *output-stream* "error")))))

(define-presentation-method accept ((type trs) stream
                                     (view textual-view)
                                     &key)
  (handler-case 
     (completing-from-suggestions (stream)
       (loop
           for trs in (list-values (table (trss *spec*)))
           do (suggest (name trs) trs)))
     (simple-completion-error ()
       (prog1 nil
	 (format *output-stream* "no match~%")))
     (all-errors ()
       (prog1 nil
	 (format *output-stream* "error")))))


(define-presentation-method accept ((type termset) stream
                                     (view textual-view)
                                     &key)
  (handler-case 
     (completing-from-suggestions (stream)
       (loop
           for termset in (list-values (termsets *spec*))
           do (suggest (name termset) termset)))
     (simple-completion-error ()
       (prog1 nil
	 (format *output-stream* "no match~%")))
     (all-errors ()
       (prog1 nil
	 (format *output-stream* "error")))))

(define-presentation-method accept ((type term) stream
                                     (view textual-view)
                                     &key)
  (handler-case 
     (completing-from-suggestions (stream)
       (loop
           for term in (list-values (table (terms *spec*)))
           do (suggest (name term) term)))
     (simple-completion-error ()
       (prog1 nil
	 (format *output-stream* "no match~%")))
     (all-errors ()
       (prog1 nil
	 (format *output-stream* "error")))))

