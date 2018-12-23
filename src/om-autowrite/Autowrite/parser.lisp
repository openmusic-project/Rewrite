(in-package :nautowrite)
;; <spec> ::= <ops> [<vars>] <truc>*
;; <ops> ::= Ops <declaration>*
;; <vars> ::= Vars <var>*
;; <var> ::= <memorized-name>
;; <declaration> ::= <memorized-name> [: <integer>]
;; <truc> ::= <trs> | <automaton> | <termset> | <termdef>
;; <trs> ::= TRS <name> <rule>*
;; <rule> ::= <term> -> <term>
;; <set> ::= Termset <name> <term>*
;; <termdef> ::= Term <term>
;; <term> ::= <memorized-name> [<arglist>]
;; <flat-term> ::= <memorized-name> [<state arglist>]
;; <arglist> ::= ( <arg> <args> )
;; <args> ::= <empty> | , <arg> <args>
;; <automaton> ::= Automaton <name> [<states>] [<description>] [<final states>] [<transitions>] 
;; <tautomaton> ::= Tautomaton <name> [<automata>]  tuple d'automates
;; <automata> ::= Automata <name>*
;; <memorized-name> := <name>
;; <name> ::= <symbol> | <string>

;; <states> ::= States <state name>*
;; <state name> ::= <memorized-name>
;; <description> ::= Description <state description>*
;; <state description> ::= <memorized-name> : <string>
;; <final states> ::= Final States <state name>*
;; <transisions> ::= Transitions <transition>*
;; <transition> ::= <term state> -> <state>
;; <state> ::= <memorized-name>
;; <term state> ::= <state> | <memorized-name> [<state arglist>]
;; <state arglist> ::= ( <state> <state args> )
;; <state args> ::= <empty> | , <state> <state args>
;; classes : kwd sym separator string

;; <trs-vars> ::= empty | [ <var> <suite-var> ]
;; <suite-var> ::= <empty> | , <var>

;; constants : +sep-open-par+ +sep-closed-par+ +sep-colon+ +sep-arrow+ +sep-comma+ +sep-open-bracket+ +sep-closed-bracket+

(define-condition autowrite-parse-error (error)
  ((token :reader token :initform "no token")
   (message :reader message :initform "no message")))

(define-condition my-keyword-expected-error (autowrite-parse-error) ())

(define-condition symbol-or-string-expected-error (autowrite-parse-error) ())
(define-condition integer-expected-error (autowrite-parse-error) ())
(define-condition colon-expected-error (autowrite-parse-error) ())
(define-condition comma-expected-error (autowrite-parse-error) ())
(define-condition arrow-expected-error (autowrite-parse-error) ())
(define-condition closed-par-expected-error (autowrite-parse-error) ())
(define-condition closed-bracket-expected-error (autowrite-parse-error) ())
(define-condition open-sbracket-expected-error (autowrite-parse-error) ())
(define-condition closed-sbracket-expected-error (autowrite-parse-error) ())
(define-condition string-expected-error (autowrite-parse-error) ())
(define-condition name-expected-error (autowrite-parse-error) ())
(define-condition flat-term-expected-error (autowrite-parse-error) ())
(define-condition arity-error (autowrite-parse-error) ())
(define-condition heterogeneous-states-error (autowrite-parse-error) ())

(defvar *vars* nil "variable symbols")
(defvar *symbols* nil "non variable symbols")
(defvar *automata* nil "defined automata")

(defvar *current-token* nil)
(defvar *previous-token* nil)
(defvar *add-symbols* nil)

(defun init-parser ()
  (init-symbols)
  (setf *current-token* nil)
  (setf *previous-token* nil)
  (setf *automata* nil)
  (setf *vars* nil))

(defmacro lexmem (stream)
  `(progn
    (setf *previous-token* *current-token*)
    (setf *current-token* (lex ,stream))
    *previous-token*))

(defun my-keyword-p (name)
  (and (eq (type-of *current-token*) 'kwd)
       (string-equal (name *current-token*) name)))

(defun integer-p (token)
  (integerp (name token)))

(defun sym-p ()
  (eq (type-of *current-token*) 'sym))

(defun symbol-or-string-p ()
  (or (sym-p)
      (stringp *current-token*)))

(defun vbits-p ()
  (eq (type-of *current-token*) 'vbits))

(defun separator-p ()
  (eq (type-of *current-token*) 'sep))

(defun keyword-p ()
  (eq (type-of *current-token*) 'kwd))

(defun keyword-or-separator-p ()
  (or (keyword-p) (separator-p)))

(defun token-or-name (token)
  (if (eq (type-of token) 'sym)
      (name token)
      token))

(defun signal-autowrite-error (error-type message)
  (let ((name (token-or-name *current-token*))
	(pname (token-or-name *previous-token*)))
    (format *output-stream* "~A, ~A: ~A~%" pname name message)
    (signal error-type :token name :message message)))

(defun scan-int (stream)
  (if (integer-p *current-token*)
      (name (lexmem stream))
      (signal-autowrite-error 'integer-expected-error "integer expected")))

(defun parse-int (stream)
  (lexmem stream)
  (scan-int stream))

(defun parse-colon (stream)
  (or (eq +sep-colon+ (lexmem stream))
      (signal-autowrite-error 'colon-expected-error "colon expected")))

(defun parse-comma (stream)
  (or (eq +sep-comma+ (lexmem stream))
      (signal-autowrite-error 'comma-expected-error "comma expected")))

(defun parse-string (stream)
  (if (stringp *current-token*)
      (lexmem stream)
      (signal-autowrite-error 'string-expected-error "string expected")))

(defun parse-arrow (stream)
  (or (eq +sep-arrow+ (lexmem stream))
      (signal-autowrite-error 'arrow-expected-error "arrow expected")))

(defun parse-closed-par (stream)
  (or (eq +sep-closed-par+ (lexmem stream))
      (if (null *current-token*)
	  (format *output-stream* "Warning: missing closing parenthesis~%")
	  (signal-autowrite-error 'closed-par-expected-error "closed-par expected"))))

(defun parse-closed-bracket (stream)
  (or (eq +sep-closed-bracket+ (lexmem stream))
      (if (null *current-token*)
	  (format *output-stream* "Warning: missing closing bracket~%")
	  (signal-autowrite-error 'closed-bracket-expected-error "closed-bracket expected"))))

(defun scan-name (stream)
  (if (symbol-or-string-p)
      (if 
       (eq (type-of *current-token*) 'sym)
       (format nil "~A" (name (lexmem stream)))
       (lexmem stream))
      (signal-autowrite-error 'symbol-or-string-expected-error "symbol or string")))

(defun scan-memorized-name (stream)
  (make-name (scan-name stream)))

;;; faire un parse string-name qui ne s'interesse pas a memoriser le name
;;; mais lis soit un symbole soit une chaine

(defun parse-name (stream)
  (lexmem stream)
  (scan-name stream))

(defun parse-memorized-name (stream)
  (lexmem stream)
  (scan-memorized-name stream))

(defun bits-from-chars (chars)
  (mapcar
   (lambda (bit) (parse-integer (make-string 1 :initial-element bit)))
   chars))

(defun scan-declaration (stream)
  (let ((name (scan-memorized-name stream))
	(arity 0)
	(vbits nil)
	(commutative nil))
    (when (vbits-p)
      (setf vbits (vbits *current-token*))
      (lexmem stream))
    (when (eq +sep-colon+ *current-token*)
      (setf arity (parse-int stream))
      (when (and (or (minusp arity) (plusp arity)) vbits)
	(signal-autowrite-error 'autowrite-error "symbol with vbits with arity > 1 or -1")))
    (when (eq +sep-colon+ *current-token*)
      (setf commutative (not (zerop (parse-int stream)))))
    (if (zerop arity)
	(if vbits
	    (make-vbits-constant-symbol name (bits-from-chars vbits))
	    (make-constant-symbol name))
	(if (minusp arity)
	    (make-unranked-symbol name commutative)
	    (if commutative
		(make-commutative-symbol name arity)
		(make-parity-symbol name arity))))))

(defun parse-declaration (stream)
  (lexmem stream)
  (scan-declaration stream))

(defun parse-symbols (stream)
  (lexmem stream)
  (unless (my-keyword-p "Ops")
    (signal-autowrite-error 'my-keyword-expected-error  "keyword Ops expected"))
  (cons (parse-declaration stream)
	(loop
	   until (keyword-p)
	   collect (scan-declaration stream))))

(defun parse-var (stream)
  (make-var (scan-memorized-name stream)))

(defun parse-vars (stream)
  (let ((vars nil))
    (do ()
	((not (symbol-or-string-p)) (nreverse vars))
      (pushnew (parse-var stream) vars)
      )))

(defun parse-arglist (stream)
  (let ((arglist nil))
    (do ()
	((not (symbol-or-string-p)) (nreverse arglist))
      (push (scan-term stream) arglist)
      (when (eq *current-token* +sep-comma+)
	  (lexmem stream)))))

(defun scan-term (stream)
  (let* ((root-name (scan-memorized-name stream))
	 (var (find-var-from-name root-name *vars*)))
    (or
     var
     (let ((vbits nil)
	   (arg nil))
       (when (vbits-p)
	 (setf vbits (bits-from-chars (vbits *current-token*)))
	 (lexmem stream))
       (let ((root 
	      (find-symbol-from-name root-name vbits)))
;;	   (format t "root ~A~%" root)
	   (when (or root *add-symbols*)
	     (when (eq *current-token* +sep-open-par+)
	       (lexmem stream)
	       (setf arg (parse-arglist stream))
	       (parse-closed-par stream)))
	   (if root
	       (progn
		 (check-arity arg root)
		 (build-term root arg))
	       (if *add-symbols*
		   (build-term
		    (make-arity-symbol root-name (length arg))
		    arg)
		   (signal-autowrite-error
		    'symbol-or-string-expected-error "not a defined symbol"))))))))


(defun parse-term (stream)
  (lexmem stream)
  (scan-term stream))

(defun scan-termset (stream)
  (let ((termset nil))
    (do ()
	((not (symbol-or-string-p)) (nreverse termset))
      (push (scan-term stream) termset))))
 
(defun parse-termset (stream)
  (lexmem stream)
  (scan-termset stream))

(defun parse-named-termset (stream)
  (lexmem stream)
  (if (symbol-or-string-p)
      (let ((name (scan-name stream)))
	(when (find-symbol-from-name name)
	  (format *output-stream* "Warning ~A also declared as a symbol~%" name)
	  (format *output-stream* "possibly missing termset name ~A ~%" name))
	(make-termset name (scan-termset stream)))
      (signal-autowrite-error 'name-expected-error "name expected")))

(defvar *warn-already-defined-state* t)

(defun scan-name-state (stream)
  (let* ((name (scan-memorized-name stream))
	 (state (make-named-state name)))
    (when (eq *current-token* +sep-colon+)
      (parse-colon stream)
      (scan-int stream))
    (unless state
      (signal-autowrite-error 'flat-term-expected-error
			      (format nil "undefined state ~A" name)))
    state))

(defun parse-name-state (stream)
  (lexmem stream)
  (parse-name-state stream))

(defun scan-pstate (stream)
;;  (format *output-stream* "scan-pstate ~A ~%" *current-token*)
  (prog1 
      (make-tuple-state (parse-states stream))
    (unless (eq +sep-closed-sbracket+ (lexmem stream))
      (signal-autowrite-error 'closed-sbracket-expected-error "closed-bracket-expected"))))

(defun scan-state (stream)
;;  (format *output-stream* "scan-state *current-token*~A~%" *current-token*)
  (cast-state
   (if (eq +sep-open-sbracket+ *current-token*)
       (scan-pstate stream)
       (scan-name-state stream))))

(defun parse-state (stream)
  (lexmem stream)
  (scan-state stream))

(defun check-arity (arg root)
  (let ((arity (arity root)))
    (unless (minusp arity)
      (unless (= (length arg) arity)
	(format
	 *output-stream*
	 "list of arguments ~A incompatible with arity of ~A:~A~%" arg root arity)
	(signal-autowrite-error
	 'arity-error
	 (format
	  nil
	  "list of arguments ~A incompatible with arity of ~A~%" arg root))))))
    
(defun scan-flat-term (stream)
  (if (symbol-or-string-p)
      (let ((rootname (scan-memorized-name stream))
	    (vbits nil))
	(when (vbits-p)
	  (setf vbits (bits-from-chars (vbits *current-token*)))
	  (lexmem stream))
	(let ((root (find-symbol-from-name rootname vbits)))
	  (if root ;; we have a symbol here
	      (let ((arg nil))
		(when (eq +sep-open-par+ *current-token*)
		  (setf arg (parse-states stream))
		  (parse-closed-par stream)
		  (check-arity arg root))
		(make-flat-term root arg))
	    (make-named-state rootname))))))

(defun parse-flat-term (stream)
  (lexmem stream)
  (scan-flat-term stream))

(defun same-type-of-states-p (states)
  (and
   states
   (let ((type (type-of (car states))))
     (every (lambda (state) (eq (type-of state) type))
	    (cdr states)))))

(defun check-states (states)
  (unless (same-type-of-states-p states)
    (signal-autowrite-error 'heterogeneous-states-error "Error: heterogenous states")))

(defun scan-states (stream)
;;  (format *output-stream* "scan-states *current-token* ~A ~%"*current-token*)
  (do ((states nil))
      ((not (or (eq *current-token* +sep-open-sbracket+) (symbol-or-string-p)))
       (prog1
	   (nreverse states)
	 (check-states states)))
    (push (scan-state stream) states)
    (when (eq *current-token* +sep-comma+)
      (parse-comma stream))))

(defun parse-states (stream)
;;  (format *output-stream* "parse-states *current-token* ~A ~%"*current-token*)
  (lexmem stream)
  (scan-states stream))

(defun parse-transition (stream)
  (let ((lh (scan-flat-term stream)))
    (parse-arrow stream)
    (add-transition-to-transitions
     (flat-term-to-key lh) (scan-state stream) *global-transitions*)))

(defun scan-transitions (stream)
  (do ()
      ((not (symbol-or-string-p)))
    (parse-transition stream))
  *global-transitions*)

(defun parse-transitions (stream)
;;  (format *output-stream* "parse-transitions *current-token* ~A ~%"*current-token*)
  (lexmem stream)
  (scan-transitions stream))

(defun parse-description (stream)
  (cons 
   (scan-memorized-name stream)
   (progn
     (parse-colon stream)
     (parse-string stream))))

(defun parse-descriptions (stream)
  (lexmem stream)
  (do ((descriptions '()))
      ((not (symbol-or-string-p)) (nreverse descriptions))
    (push (parse-description stream) descriptions)))

(defun scan-automaton (stream)
  (lexmem stream)
  (if (symbol-or-string-p)
      (with-new-transitions
	(let ((name (name (lexmem stream)))
	      (final nil)
	      (transitions nil)
	      (prior-transitions nil))
	  (declare (ignore prior-transitions))
	  (when (my-keyword-p "States")
	    (parse-states stream))
	  (when (my-keyword-p "Description")
	    (parse-descriptions stream))
	  (let ((*warn-already-defined-state* nil))
	    (when (my-keyword-p "Final")
	      (lexmem stream)
	      (setf final (parse-states stream)))
	    (when (my-keyword-p "Prior")
	      (lexmem stream)
	      (scan-transitions stream))
	    (setf transitions (parse-transitions stream)))
	  ;;	    (setf transitions (epsilon-closure (parse-transitions stream)))
	  ;;	  (format t "finalstates ~A" final)
	  (make-automaton transitions
			  :name name
			  :signature (make-signature (all-symbols))
			  :finalstates (make-ordered-container final))))
      (signal-autowrite-error 'name-expected-error "name expected")))

(defun parse-automaton (stream)
  (lexmem stream)
  (scan-automaton stream))

(defun parse-autname (stream)
;; add checks whether automata exists?
  (parse-name stream))

(defun scan-autname (stream)
  (scan-name stream))

(defun scan-autnames (stream)
  (do ((automata nil))
      ((not (symbol-or-string-p)) (nreverse automata))
    (push (scan-autname stream) automata)))

(defun parse-autnames (stream)
  (lexmem stream)
  (scan-autnames stream))

(defun parse-automata (stream)
  (if (my-keyword-p "Automata")
      (parse-autnames stream)
      (signal-autowrite-error 'my-keyword-expected-error "keyword Automata expected")))

(defun scan-tautomaton (stream)
  (lexmem stream)
  (if (symbol-or-string-p)
      (let*
	  ((name (name (lexmem stream)))
	   (autnames (parse-automata stream))
	   (automata
	    (mapcar
	     (lambda (aname)
	       (let ((automaton (find aname *automata* :key #'name :test #'equal)))
		 (or automaton
		     (signal-autowrite-error
		      'autowrite-parse-error
		      (format nil "~A Undefined automaton in Tautomaton ~A~%" aname name)))))
	       autnames)))
	(make-tautomaton name automata))
      (signal-autowrite-error 'name-expected-error "name expected")))

(defun parse-tautomaton (stream)
  (lexmem stream)
  (scan-tautomaton stream))

(defun scan-rule (stream)
  (let ((lh (scan-term stream)))
    (parse-arrow stream)
    (let ((rh (scan-term stream)))
      (make-rule lh rh))))

(defun parse-rule (stream)
  (lexmem stream)
  (scan-rule stream))

(defun scan-rules (stream)
  (do ((rules '()))
      ((not (symbol-or-string-p)) (make-rules (nreverse rules)))
    (push (scan-rule stream) rules)))

(defun parse-rules (stream)
  (lexmem stream)
  (scan-rules stream))

(defun scan-trs (stream)
  (if (symbol-or-string-p)
      (make-trs (name (lexmem stream))
		(scan-rules stream)
		(make-signature (all-symbols)))
      (signal-autowrite-error 'name-expected-error "name expected")))

(defun parse-trs (stream)
  (lexmem stream)
  (scan-trs stream))

(defun simple-spec (stream)
  (let ((*previous-token* nil)
	(*current-token* nil)
	(tokens nil))
    (lexmem stream)
    (do ()
	((null *current-token*) (nreverse tokens))
      (push (lexmem stream) tokens))))

(defun parse-spec (stream)
  (init-parser)
  (let (
	(termsets '())
	(tautomata ())
	(terms ())
	(trss ()))
    (parse-symbols stream)
    (when *current-token*
      (when (my-keyword-p "Vars")
	(lexmem stream)
	(setf *vars* (parse-vars stream))))
    (do ()
	((or (my-keyword-p "Eof")
	     (not (eq (type-of *current-token*) 'kwd)))
	 (list (make-signature (all-symbols))
	       *vars*
	       (nreverse trss)
	       (nreverse *automata*)
	       (nreverse tautomata)
		 (nreverse termsets)
		 (nreverse terms)))
	;;      (format *output-stream* "current token ~A~%" *current-token*)
	(cond
	  ((my-keyword-p "TRS") (push (parse-trs stream) trss))
	  ((my-keyword-p "Automaton") (push (scan-automaton stream) *automata*))
	  ((my-keyword-p "Tautomaton") (push (scan-tautomaton stream) tautomata))
	  ((my-keyword-p "Termset") (push (parse-named-termset stream) termsets))
	  ((my-keyword-p "Term") (push (parse-term stream) terms))
	  (t (error 'autowrite-parse-error :token *current-token* "unknown keyword"))))
	))

(defun parse-trs-vars (stream)
  (let ((vars '()))
    (lexmem stream)
    (when (eq +sep-open-bracket+ *current-token*)
      (lexmem stream)
      (do ()
	  ((not (symbol-or-string-p)))
	(push (parse-var stream) vars)
	(when (eq *current-token* +sep-comma+)
	  (lexmem stream)))
      (parse-closed-bracket stream))
    vars))

(defun parse-trs-spec (stream)
  (init-parser)
  (let* ((*vars* (parse-trs-vars stream))
	 (*add-symbols* t)
	 (rules (scan-rules stream))
	 (signature (make-signature (all-symbols)))
	 (trs (make-trs "R" rules signature)))
    (list signature *vars* (list trs) '() '())))

(defun parse-aut-spec (stream)
  (init-parser)
  (let ((*add-symbols* t)
	(aut (parse-automaton stream)))
    (list (all-symbols) () () (list aut) '())))
