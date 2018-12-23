;; -*- Mode: Lisp; Package: GAUTOWRITE -*-

;;;  (c) copyright 2001 by
;;;           Irène Durand (idurand@labri.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;(setf interface:*interface-style* :tty)
#+cmu
(setf ext:*gc-verbose* nil)

(in-package :gautowrite)

(defconstant +largeur-texte+ (1+ (length "Nagaya-Toyama")))
(defvar *processname-pane*)
(defvar *jacquemard* nil "toggles between Jacquemard and Toyama'algorithm")

(defun run-test (name)
  (loop for port in climi::*all-ports*
	do (destroy-port port))
  (setq climi::*all-ports* nil)
  (let ((frame (make-application-frame name)))
	(run-frame-top-level frame)))

(defun gautowrite ()
  (init-autowrite)
  (run-test 'autowrite)
  0)

(make-command-table
 'file-command-table
 :errorp nil
 :menu '(
	 ("Load Specification (.txt)" :command com-load-spec)
	 ("Change Data Directory" :command com-change-data-directory)
	 ("Default Data Directory" :command com-default-data-directory)
	 ("Load aut specification (.aut)" :command com-load-aut-spec)
	 ("Load TRS specification (.tes)" :command com-load-trs-spec)
	 ("Save Specification" :command com-save-spec)
	 ("Save Specification Readable" :command com-save-spec-readable)
	 ("Retrieve Specification" :command com-retrieve-spec)
	 ("Show Signature" :command com-show-signature)
	 ("Show Variables" :command com-show-variables)
	 ("Show Data Directory" :command com-show-data-directory)
	 ("Clear all" :command com-clear-all)
	 ("Quit" :command com-quit)))

(make-command-table 'approx-trs-command-table
		    :errorp nil
		    :menu '(("Approximation" :menu approximation-command-table)
                            ("Call-by-need " :command com-call-by-need)
                            ("Call-by-need@" :command com-call-by-need-extra)
                            ("WN(S,G,F) == WN(S,F)" :command com-wn)
                            ("WNo(S,G,F) == WNo(S,F)" :command com-wn-bullet)
			    ("Arbitrary" :command com-arbitrary)
		    ))
(make-command-table 'trs-command-table
		    :errorp nil
		    :menu '(
			    ("Retrieve trs" :command com-retrieve-trs)
			    ("Leftlinearity" :command com-leftlinearity)
                            ("Overlapping" :command com-overlapping)
                            ("Orthogonal" :command com-orthogonal)
                            ("Collapsing" :command com-collapsing)
                            ("Growing" :command com-growing)
                            ("Constructor" :command com-constructor)
                            ("Forward-branching-def" :command com-forward-branching-def)
                            ("Forward-branching" :command com-forward-branching)
                            ("FB-to-constructor" :command com-fb-to-constructor)
                            ("FB-to-constructor-bis" :command com-fb-to-constructor-bis)
                            ("Size trs" :command com-size-trs)
                            ("NF EMPTY?" :command com-nf-empty)
                            ("ENF EMPTY?" :command com-enf-empty)
			    ("Inverse trs" :command com-inverse-trs)
			    ("Load trs" :command com-load-trs)
			    ("Read trs" :command com-read-trs)
			    ("Clear trs" :command com-clear-trs)
			    ))

(make-command-table 'term-command-table
		    :errorp nil
		    :menu '(
			    ("Retrieve term" :command com-retrieve-term)
;			    ("Ground" :command com-ground)
			    ("Redexes" :command com-redexes)
			    ("Needed redexes" :command com-needed-redexes)
			    ("Needed redexes@" :command com-needed-redexes-extra)
			    ("Normalizable" :command com-normalizable)
			    ("Normalizable@" :command com-normalizable-extra)
                            ("Regonizability" :command com-recognize-term)
			    ("Compute states" :command com-compute-target)
			    ("Compute state" :command com-compute-state)
			    ("Load term" :command com-load-term)
			    ("Read term" :command com-read-term)
			    ("Clear term" :command com-clear-term)
			    ))

(make-command-table
 'termset-command-table
 :errorp nil
 :menu '(
	 ("Retrieve termset" :command com-retrieve-termset)
	 ("Clear termset" :command com-clear-termset)
	 ("Accessibility" :command com-accessibility)
	 ("Termset automaton" :command com-termset-automaton)
	 ("Load termset" :command com-load-termset)
	 ("Read termset" :command com-read-termset)
	 ))

(make-command-table
 'reduction-command-table
 :errorp nil
 :menu '(
        ("Needed reduction step" :command com-needed-reduction-step)
        ("Needed reduction to nf" :command com-needed-reduction-to-nf)
	 ("leftmost outermost step" :command com-leftmost-outermost-step)
	 ("parallel outermost step" :command com-parallel-outermost-step)
	 ("reduction to nf" :command com-reduction-to-nf)
	 ))

(make-command-table
 'approximation-command-table
 :errorp nil
 :menu '(
	 ("Strong" :command com-approximation-strong)
	 ("NV" :command com-approximation-nv)
	 ("Linear growing" :command com-approximation-linear-growing)
 ("Growing" :command com-approximation-growing)))

(make-command-table
 'menubar-command-table
 :errorp nil
 :menu
 '(
   ("File " :menu file-command-table)
   ("Trs " :menu trs-command-table)
   ("Approximation " :menu approx-trs-command-table)
   ("Term " :menu term-command-table)
   ("Termset " :menu termset-command-table)
   ("Reduction " :menu reduction-command-table)
   ("TRS Automata " :menu trs-automata-command-table)
   ("Automata " :menu automata-command-table)
   ("Automaton " :menu automaton-command-table)
   ("Tautomaton " :menu tautomaton-command-table)
   )
 )

(make-command-table 'trs-automata-command-table
		    :errorp nil
		    :menu '(
			    ("NF automaton" :command com-nf-automaton)
			    ("NFo automaton" :command com-nf-bullet-automaton)
			    ("Redex automaton" :command com-redex-automaton)
			    ("Reducible automaton" :command com-reducible-automaton)
			    ("Automaton C NF" :command com-automaton-c-nf)
			    ("Automaton C NFo" :command com-automaton-c-nf-bullet)
			    ("Automaton C NFo Extra" :command com-automaton-c-nf-bullet-extra)
			    ("Automaton C S" :command com-automaton-c-s)
			    ("Automaton C A" :command com-automaton-c-a)
			    ("Automaton D" :command com-automaton-d)
			    ("Automaton D Extra" :command com-automaton-d-extra)
			    ("Automaton D Partial" :command com-automaton-d-partial)
			    ("Automaton D Extra Partial" :command com-automaton-d-extra-partial)
			    ))

(make-command-table 'automata-command-table
		    :errorp nil
		    :menu '(
                            ("Complete automaton" :command com-complete-automaton)
                            ("Complement automaton" :command com-complement-automaton)
                            ("Reduce automaton" :command com-reduce-automaton)
                            ("Determinize automaton" :command com-determinize-automaton)
                            ("Minimize automaton" :command com-minimize-automaton)
                            ("Simplify automaton" :command com-simplify-automaton)
                            ("Intersect automaton" :command com-intersect-automaton)
                            ("Union automaton" :command com-union-automaton)
                            ("Duplicate automaton" :command com-duplicate-automaton)
;                            ("Epsilon Closure" :command com-epsilon-closure)
			    ))

(make-command-table
 'automaton-command-table
 :errorp nil
 :menu '(
	 ("Retrieve automaton" :command com-retrieve-automaton)
	 ("Rename automaton" :command com-rename-automaton)
	 ("Number states" :command com-number-states)
	 ("Name states" :command com-name-states)
	 ("Show automaton" :command com-show-automaton)
	 ("Show automaton signature" :command com-show-automaton-signature)
	 ("Is deterministic?" :command com-is-deterministic)
	 ("Is complete?" :command com-is-complete)
	 ("Is minimal?" :command com-is-minimal)
	 ("Is simplified?" :command com-is-simplified)
	 ("Is empty?" :command com-is-empty)
	 ("Equivalence Classes?" :command com-equivalence-classes)
	 ("Equality automaton?" :command com-equality-automaton)
	 ("Inclusion automaton?" :command com-inclusion-automaton)
	 ("Empty Intersection?" :command com-empty-intersection)
	 ("Load automaton" :command com-load-automaton)
	 ("Clear automaton" :command com-clear-automaton)
	 ("Save automaton" :command com-save-automaton)
			    ))
(make-command-table
 'tautomaton-command-table
 :errorp nil
 :menu '(
	 ("Retrieve tautomaton" :command com-retrieve-tautomaton)
	 ("Rename tautomaton" :command com-rename-tautomaton)
	 ("Add automaton to tautomaton?" :command com-add-automaton-to-tautomaton)
	 ("Tregonizability" :command com-trecognize-term)
	 ("Tcompute-states" :command com-tcompute-target)
	 ("Tcompute-state" :command com-tcompute-state)
	 ("Tintersection" :command com-tintersection)
	 ("Tunion" :command com-tunion)))

(defun algo-name ()
  (normalize-string
   (if *jacquemard*
       "Jacquemard "
       "Nagaya-Toyama")
   +largeur-texte+))

(defun process-name ()
  (normalize-string
   (if *with-processes*
       "processes"
       "no processes")
   +largeur-texte+))

(defmethod display-processname :around ((name string))
  (if *processname-pane*
      (setf (gadget-value *processname-pane*)
	    (normalize-string name +largeur-texte+))
      (format t "~A" name)))

;;  (result-pane
;;    (make-pane 'clim-stream-pane
;;               :name 'result-pane))
;;	       :text-style (make-text-style  :fixed :roman :small))

(define-application-frame autowrite () ()
  (:panes
   (result-pane
    (make-pane 'clim-stream-pane
               :name 'result-pane))
   (trs-pane
    (make-pane 'clim-stream-pane
	       :name 'trs-pane))
   (approx-pane
    (make-pane 'clim-stream-pane
	       :name 'approx-pane))
   (automaton-pane
    (make-pane 'clim-stream-pane
	       :name 'automaton-pane))
   (tautomaton-pane
    (make-pane 'clim-stream-pane
	       :name 'tautomaton-pane))
   (termset-pane
    (make-pane 'clim-stream-pane
	       :name 'termset-pane))
   (term-pane
    (make-pane 'clim-stream-pane
	       :name 'term-pane))
  (interactor-pane
    :interactor)
   (menu-bar
    (climi::make-menu-bar
     'menubar-command-table))
   (quit
    :push-button
    :label "Quit"
    :activate-callback #'(lambda (x)
			   (declare (ignore x))
			   (com-quit)))
;;    (stop
;;     :push-button
;;     :label "Stop"
;;     :activate-callback #'(lambda (x)
;; 			   (declare (ignore x))
;; 			   (com-stop)))
   (limit-transitions
    :toggle-button
    :label "Limit transitions"
    :indicator-type :one-of
    :value *limit-transitions*
    :value-changed-callback
    (lambda (gadget value)
      (declare (ignore gadget value))
      (com-toggle-limit-transitions)))
   (algo
    :push-button
    :label "algo"
    :activate-callback #'(lambda (x)
			   (declare (ignore x))
			   (com-algo)))
;;    (process
;;     :push-button
;;     :label "process"
;;     :activate-callback #'(lambda (x)
;; 			   (declare (ignore x))
;; 			   (com-process)))
   (clear
    :push-button
    :label "clear" ;:max-width 100 :max-height 100
    :activate-callback #'(lambda (x)
			   (declare (ignore x))
			   (com-clear)))
   (algo-pane
    :text-field
    :value (algo-name)
    :editable-p nil)
   (processname-pane
    :text-field
    :value (normalize-string " " +largeur-texte+))
   )
(:layouts
   (default
       (vertically ()
	menu-bar
	(horizontally ()
	  (vertically ()
	    (horizontally ()
	      (vertically ()
		(horizontally (:height 100) interactor-pane)
		(horizontally ()
		  (scrolling (:height 150) trs-pane)
		  (scrolling (:height 150) approx-pane))
		(horizontally ()
		  (scrolling (:height 150) term-pane)
		  (scrolling (:height 70) termset-pane))
		(scrolling (:height 150) result-pane))))
	  (vertically ()
		      (scrolling (:width 500) automaton-pane)
		      tautomaton-pane)
	  )
	(horizontally ()
	  quit clear
;;	  stop
	  +fill+ algo algo-pane +fill+ limit-transitions processname-pane)))))

(define-condition all-errors (error)
  ())

(defmethod default-frame-top-level :before
    ((frame application-frame)
     &key (command-parser 'command-line-command-parser)
	  (command-unparser 'command-line-command-unparser)
	  (partial-command-parser
	   'command-line-read-remaining-arguments-for-partial-command)
	  (prompt "command: "))
  (declare (ignore command-parser))
  (declare (ignore command-unparser))
  (declare (ignore partial-command-parser))
  (declare (ignore prompt))
  (setf *output-stream*
	(climi::find-pane-named *application-frame* 'result-pane)
	*processname-pane*
	(climi::find-pane-named *application-frame* 'processname-pane)
	*automaton-pane*
	(climi::find-pane-named *application-frame* 'automaton-pane)
	*tautomaton-pane*
	(climi::find-pane-named *application-frame* 'tautomaton-pane)
	*term-pane*
	(climi::find-pane-named *application-frame* 'term-pane)
	*termset-pane*
	(climi::find-pane-named *application-frame* 'termset-pane)
	*approx-pane*
	(climi::find-pane-named *application-frame* 'approx-pane)
	*trs-pane*
	(climi::find-pane-named *application-frame* 'trs-pane))
  (com-init-algo))

(defun check-trs ()
  (or (trs *spec*)
    (format *output-stream* "No current TRS~%")))

(defun check-automaton ()
  (or (automaton *spec*)
    (format *output-stream* "No current Automaton~%")))

(defun check-tautomaton ()
  (or (tautomaton *spec*)
    (format *output-stream* "No current TAutomaton~%")))

(defun check-termset ()
  (or (termset *spec*)
    (format *output-stream* "No current Termset~%")))

(defun check-term ()
  (or (term *spec*)
    (format *output-stream* "No current Term~%")))

(defun check-for-reduction ()
  (and (check-trs) (check-term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-autowrite-command (com-toggle-limit-transitions :name t) ()
  (toggle-limit-transitions)
  (display-current-spec))

(define-autowrite-command (com-show-data-directory :name t) ()
  (format *output-stream* "Data directory is ~A~%" *data-directory*))

(define-autowrite-command (com-change-data-directory :name t) ()
   (let ((pathname (accept 'pathname :prompt "data directory?"
 			  :default (initial-data-directory)
 			  :default-type 'pathname
 			  :insert-default t)))
    (if (and pathname (probe-file pathname))
	(setf *data-directory* (namestring (probe-file pathname)))
	(progn
	  (format *output-stream* "no such directory~%")
	  (setf *data-directory* (initial-data-directory))))
    (format *output-stream* "Data directory is ~A~%" *data-directory*)))

(define-autowrite-command (com-default-data-directory :name t) ()
  (setf *data-directory* (initial-data-directory))
  (format *output-stream* "Data directory is ~A~%" *data-directory*))

(define-autowrite-command (com-load-spec :name t) ()
  (let ((name (accept '(data-file-name "*.txt")
		      :prompt "spec filename"
		      :default nil
		      :display-default nil
		      )))
     (when name
       (let ((spec (read-spec-from-path name)))
	 (when spec
 	  (com-clear-all)
 	  (set-and-display-current-spec spec)
 	  )))))

(define-autowrite-command (com-save-spec :name t) ()
  (let ((name (accept 'string :prompt "spec filename"
		      :default "save")))
        (let ((*loadable* t)
	      (filename (concatenate 'string name ".txt")))
	  (if (save-spec filename)
	      (format *output-stream* "Specification saved~%")
	      (format *output-stream* "unable to open ~A~%" filename)))))

(define-autowrite-command (com-save-spec-readable :name t) ()
  (let ((*limit-transitions* nil)
	(name (accept 'string :prompt "spec filename"
		      :default "save")))
        (let ((*loadable* nil))
	  (save-spec (concatenate 'string name ".txt")))))

(define-autowrite-command (com-retrieve-spec :name t) ()
  (let ((spec 
	 (accept 'spec
		 :prompt "spec name")))
    (when spec
      (set-and-display-current-spec spec))))

(define-autowrite-command (com-load-trs-spec :name t) ()
  (let ((name (accept '(data-file-name "*.tes")
		      :prompt "trs spec filename"
		      :default nil
		      :display-default nil
		      )))
    (when name
      (let ((spec (load-file-from-absolute-filename name #'load-trs-spec)))
	(when spec
	  (com-clear-all)
	  (set-and-display-current-spec (build-spec (file-namestring name) spec))
	  )))))

(define-autowrite-command (com-load-aut-spec :name t) ()
  (let ((name (accept '(data-file-name "*.aut")
		      :prompt "trs spec filename"
		      :default nil
		      :display-default nil
		      )))
    (when name
      (let ((spec (load-file-from-absolute-filename name #'load-aut-spec)))
	(when spec
	  (com-clear-all)
	  (set-and-display-current-spec (build-spec (file-namestring name) spec))
	  )))))

(define-autowrite-command (com-show-signature :name t) ()
  (when *spec*
    (format *output-stream* "Current signature: ~A~%" (signature *spec*))))

(define-autowrite-command (com-show-variables :name t) ()
  (when *spec*
    (format *output-stream* "Current variables: ")
    (display-sequence (vars *spec*) *output-stream*))
    (format *output-stream* "~%"))

(define-autowrite-command (com-load-trs :name t) ()
  (let ((name (accept '(data-file-name)
		      :prompt "trs filename"
		      :default nil
		      :display-default nil
		      )))
    (when name
      (let ((rules (load-file-from-absolute-filename name #'read-rules)))
	(if rules
	    (set-and-display-current-trs (seqsys-from-rules name rules))
	    (format *output-stream* "no rules~%"))))))

(define-autowrite-command (com-read-trs :name t) ()
  (let ((name (accept 'string :prompt "trs name")))
    (when name
      (let* ((str (concatenate 'string (accept 'string :prompt "rules") " "))
	     (rules (with-input-from-string (foo str)
		       (read-rules foo))))
    	(if rules
	    (set-and-display-current-trs (seqsys-from-rules name rules))
	    (format *output-stream* "no rules~%"))))))

(define-autowrite-command (com-retrieve-trs :name t) ()
  (let ((trs 
	 (accept 'trs
		 :prompt "trs name"
		 )))
    (when trs
      (set-and-display-current-trs trs))))

(define-autowrite-command (com-clear-trs :name t) ()
  (clear-current-trs)
  (window-clear *trs-pane*)
  (window-clear *approx-pane*)
  (com-clear))

(define-autowrite-command (com-quit :name t) ()
  (setf *output-stream* t)
  (frame-exit *application-frame*))

(define-autowrite-command (com-load-termset :name t) ()
  (let ((name (accept 'string
		      :prompt "termset filename"
		      :default nil
		      :display-default nil
		      )))
    (when name
      (let ((termset (load-file-from-absolute-filename (absolute-filename name) #'read-termset)))
	(when termset
	  (load-termset name termset))))))

(define-autowrite-command (com-read-termset :name t) ()
  (let ((name (accept 'string
		      :prompt "name"
		      )))
    (when name
      (let ((str (accept 'string
			 :prompt "termset"
			 )))
	(when str
	    (let ((termset (with-input-from-string (foo str)
		       (read-termset foo))))
	      (when termset
		(load-termset name termset))))))))

(define-autowrite-command (com-retrieve-termset :name t) ()
   (let ((termset 
	 (accept 'termset
		 :prompt "termset name"
		 )))
    (when termset
      (set-and-display-current-termset termset))))

(define-autowrite-command (com-clear-termset :name t) ()
  (clear-current-termset))

(define-autowrite-command (com-clear-term :name t) ()
  (clear-current-term))

(define-autowrite-command (com-retrieve-term :name t) ()
  (let ((term 
	 (accept 'term
		 :prompt "term"
		 )))
    (when term
      (set-and-display-current-term term))))

(define-autowrite-command (com-load-term :name t) ()
  (let ((name (accept '(data-file-name)
		      :prompt "term filename"
		      :default nil
		      :display-default nil
		      )))
    (when name
      (let ((term (load-file-from-absolute-filename name #'read-term)))
	(when term
	  (load-term term))))))

(define-autowrite-command (com-read-term :name t) ()
;  (let ((stream *output-stream*))
;    (with-text-style (stream '(:serif nil :huge))
;      (write-string "com-read-term~%" stream))
;    )
  (let ((term-string (accept 'string
			     :prompt "term"
			     )))
    (when term-string
      (load-term
       (with-input-from-string (foo term-string)
	 (read-term foo))))))

(define-autowrite-command (com-redexes :name t) ()
  (when (check-term)
    (if (seqsys-merge-signature (trs *spec*) (signature-from (term *spec*)))
	(let ((rp (redex-positions (term *spec*) (trs *spec*))))
	  (if (null rp)
	      (format *output-stream* "normal form ~%")
	      (format *output-stream* "redex positions: ~A~%"
		      (positions-to-string rp))))
	(format *output-stream* "incompatible signatures~%"))))

(define-autowrite-command (com-needed-redexes :name t) () 
  (when (check-for-reduction)
    (process-term-needed-redexes (term *spec*) (trs *spec*))))

(define-autowrite-command (com-needed-redexes-extra :name t) ()
  (when (check-for-reduction)
    (process-term-needed-redexes (term *spec*) (trs *spec*) :extra t)))

(define-autowrite-command (com-normalizable :name t) ()
  (when (check-for-reduction)
    (process-normalizable (term *spec*) (trs *spec*))))

(define-autowrite-command (com-normalizable-extra :name t) ()
  (when (check-for-reduction)
    (process-normalizable (term *spec*) (trs *spec*) :extra t)))

(define-autowrite-command (com-accessibility :name t) ()
  (when (and (check-term) (check-termset))
    (if (linear-terms-p (get-terms (termset *spec*)))
	(process-accessibility)
	(format *output-stream*
		"method not applicable to non linear termset~%"))))

(define-autowrite-command (com-needed-reduction-step :name t) ()
  (when (check-for-reduction)
    (process-apply-reduction :needed t)))

(define-autowrite-command (com-leftmost-outermost-step :name t) ()
  (when (check-for-reduction)
    (apply-reduction :needed nil)))

(define-autowrite-command (com-parallel-outermost-step :name t) ()
  (when (check-for-reduction)
    (apply-reduction :needed nil :parallel t)))

(define-autowrite-command (com-needed-reduction-to-nf :name t) ()
  (when (check-for-reduction)
    (process-apply-reduction-nf :needed t)))

(define-autowrite-command (com-reduction-to-nf :name t) ()
  (when (check-for-reduction)
    (process-apply-reduction-nf :needed nil)))

(define-autowrite-command (com-compute-target :name t) ()
  (when (and (check-term) (check-automaton))
    (if (ground (term *spec*))
	(format *output-stream* "~A reduces to state(s):~A~%"
		(term *spec*)
		(compute-target
		 (term *spec*)
		 (automaton *spec*)))
	(format *output-stream* "current term must be ground~%"))))

(define-autowrite-command (com-compute-state :name t) ()
  (when (and (check-term) (check-automaton))
    (if (ground (term *spec*))
	(format *output-stream* "~A reduces to :~A~%"
		(term *spec*)
		(compute-state
		 (term *spec*)
		 (automaton *spec*)))
	(format *output-stream* "current term must be ground~%"))))


(define-autowrite-command (com-ground :name t) ()
  (term-check-property ground))

(define-autowrite-command (com-leftlinearity :name t) ()
  (trs-check-property left-linear))

(define-autowrite-command (com-overlapping :name t) ()
  (trs-check-overlapping (trs *spec*)))

(define-autowrite-command (com-orthogonal :name t) ()
  (trs-check-orthogonal (trs *spec*)))

(define-autowrite-command (com-growing :name t) ()
  (trs-check-growing (trs *spec*)))

(define-autowrite-command (com-collapsing :name t) ()
  (trs-check-collapsing (trs *spec*)))

(define-autowrite-command (com-constructor :name t) ()
  (trs-check-constructor (trs *spec*)))

(define-autowrite-command (com-forward-branching :name t) ()
  (trs-check-forward-branching (trs *spec*)))

(define-autowrite-command (com-forward-branching-def :name t) ()
  (trs-check-forward-branching (trs *spec*) :def t))

(define-autowrite-command (com-fb-to-constructor :name t) ()
  (trs-fb-to-constructor (trs *spec*)))

(define-autowrite-command (com-fb-to-constructor-bis :name t) ()
  (trs-fb-to-constructor (trs *spec*) t))

(define-autowrite-command (com-size-trs :name t) ()
  (when (trs *spec*)
    (format *output-stream*
	    "The size of trs ~A is ~A~%"
	    (name (trs *spec*))
	    (size (trs *spec*)))))

(define-autowrite-command (com-arbitrary :name t) ()
  (process-approx-arbitrary))

(define-autowrite-command (com-call-by-need :name t) ()
  (when (trs *spec*)
    (process-sequentiality (trs *spec*) :deterministic (not *jacquemard*))))

(define-autowrite-command (com-call-by-need-extra :name t) ()
  (when (trs *spec*)
    (process-sequentiality (trs *spec*) :deterministic (not *jacquemard*) :extra t)))

(define-autowrite-command (com-wn :name t) ()
  (process-wn-inclusion))

(define-autowrite-command (com-wn-bullet :name t) ()
  (process-wn-inclusion :bullet t))

(define-autowrite-command (com-nf-empty :name t) ()
  (process-nf-empty))

(define-autowrite-command (com-enf-empty :name t) ()
  (process-enf-empty))

(define-autowrite-command (com-algo :name t) ()
  (setf *jacquemard* (not *jacquemard*))
  (setf (gadget-value
	 (climi::find-pane-named *application-frame* 'algo-pane)) (algo-name)))

(define-autowrite-command (com-init-algo :name t) ()
  (setf (gadget-value
	 (climi::find-pane-named *application-frame* 'algo-pane)) (algo-name)))

;; (define-autowrite-command (com-process :name t) ()
;;   (setf *with-processes* (not *with-processes*))
;;   (setf (gadget-value
;; 	 (climi::find-pane-named *application-frame* 'processname-pane)) (process-name)))

(define-autowrite-command (com-approximation-strong :name t) ()
  (update-approximation 'strong-approximation)
  (display-current-approximation))

(define-autowrite-command (com-approximation-nv :name t) ()
  (update-approximation 'nv-approximation)
  (display-current-approximation))

(define-autowrite-command (com-approximation-growing :name t) ()
  (update-approximation 'growing-approximation)
  (display-current-approximation))

(define-autowrite-command (com-approximation-linear-growing :name t) ()
  (update-approximation 'linear-growing-approximation)
  (display-current-approximation))

(define-autowrite-command (com-clear :name t) ()
  (window-clear *output-stream*))

(define-autowrite-command (com-stop :name t) ()
  (when *aux-process*
    (clim-sys::destroy-process *aux-process*)
    (format *output-stream* "Process stopped~%")
    ))

(define-autowrite-command (com-recognize-term :name t) ()
  (block nil
    (when (and (check-automaton) (check-term))
      (let ((signature-term (signature-from (term *spec*))))
	(unless (included-signature-p signature-term (signature (automaton *spec*)))
	  (format *output-stream*
		  "term signature not included in automaton signature~%")
	  (format *output-stream*
		  "you may add a symbol to the termset signature with the command Add Symbol~%")
	  (return))
      (format *output-stream*
	      "~A is ~Arecognized by ~A~%"
	      (term *spec*)
	      (if (recognized-p (term *spec*)
				(automaton *spec*)) "" "not ")
	      (name (automaton *spec*)))))))

(define-autowrite-command (com-trecognize-term :name t) ()
  (block nil
    (when (and (check-tautomaton) (check-term))
      (let ((signature-term (signature-from (term *spec*))))
	(unless (included-signature-p signature-term (signature (tautomaton *spec*)))
	  (format *output-stream*
		  "term signature not included in automaton signature~%")
	  (format *output-stream*
		  "you may add a symbol to the termset signature with the command Add Symbol~%")
	  (return))
      (format *output-stream*
	      "~A is ~Arecognized by ~A~%"
	      (term *spec*)
	      (if (recognized-p (term *spec*)
				(tautomaton *spec*)) "" "not ")
	      (name (tautomaton *spec*)))))))

(define-autowrite-command (com-tintersection :name t) ()
  (when (tautomaton *spec*)
    (set-and-display-current-automaton
     (intersection-automata (automata (tautomaton *spec*))))))

(define-autowrite-command (com-tunion :name t) ()
  (when (tautomaton *spec*)
    (set-and-display-current-automaton
     (union-automata (automata (tautomaton *spec*))))))

(define-autowrite-command (com-tcompute-state :name t) ()
  (block nil
    (when (and (check-tautomaton) (check-term))
      (let ((signature-term (signature-from (term *spec*))))
	(unless (included-signature-p signature-term (signature (tautomaton *spec*)))
	  (format *output-stream*
		  "term signature not included in automaton signature~%")
	  (format *output-stream*
		  "you may add a symbol to the termset signature with the command Add Symbol~%")
	  (return))
      (format *output-stream* 
	      "~A treduces to ~A~%"
	      (term *spec*)
	      (tcompute-target (term *spec*) (tautomaton *spec*)))))))

(define-autowrite-command (com-tcompute-target :name t) ()
  (block nil
    (when (and (check-tautomaton) (check-term))
      (let ((signature-term (signature-from (term *spec*))))
	(unless (included-signature-p signature-term (signature (tautomaton *spec*)))
	  (format *output-stream*
		  "term signature not included in automaton signature~%")
	  (format *output-stream*
		  "you may add a symbol to the termset signature with the command Add Symbol~%")
	  (return))
      (format *output-stream* 
	      "~A treduces to ~A~%"
	      (term *spec*)
	      (tcompute-target (term *spec*) (tautomaton *spec*)))))))

(define-autowrite-command (com-load-automaton :name t) ()
  (let ((name (accept '(data-file-name "*.aut")
		      :prompt "spec filename"
		      :default nil
		      :display-default nil
		      )))
    (format *error-output* "~A~%" name)
    (when name
      (let ((automaton (load-file-from-absolute-filename name #'read-automaton)))
	(when automaton
	  (set-and-display-current-automaton automaton))))))

(define-autowrite-command (com-add-automaton-to-tautomaton :name t) ()
  (when (and (automaton *spec*) (tautomaton *spec*))
    (add-automaton-to-tautomaton (automaton *spec*) (tautomaton *spec*))
    (display-current-tautomaton)))

(define-autowrite-command (com-show-automaton :name t) ()
  (let ((automaton (automaton *spec*)))
    (when automaton
      (show-automaton automaton :newline t))))

(define-autowrite-command (com-show-automaton-signature :name t) ()
  (when (automaton *spec*)
    (format *output-stream* "~A~%" (signature (automaton *spec*)))))

(define-autowrite-command (com-number-states :name t) ()
  (when (automaton *spec*)
    (number-states (automaton *spec*))
    (display-current-automaton)))

(define-autowrite-command (com-name-states :name t) ()
  (when (automaton *spec*)
    (name-states (automaton *spec*))
    (display-current-automaton)))

(define-autowrite-command (com-retrieve-automaton :name t) ()
  (let ((automaton 
	 (accept 'automaton
;;		 :default nil
;;		 :display-default nil
		 :prompt "automaton name"
		 )))
    (when automaton
      (set-and-display-current-automaton automaton))))

(define-autowrite-command (com-rename-automaton :name t) ()
  (let ((automaton (automaton *spec*)))
    (when automaton
      (let ((name 
	     (accept 'string
		     ;;		 :default nil :display-default nil
		     :prompt "automaton new name")))
	(when name
	  (remove-automaton automaton *spec*)
	  (set-and-display-current-automaton
	   (rename-object automaton name)))))))

(define-autowrite-command (com-rename-tautomaton :name t) ()
  (let ((tautomaton (tautomaton *spec*)))
    (when tautomaton
      (let ((name 
	     (accept 'string
		     ;;		 :default nil :display-default nil
		     :prompt "tautomaton new name")))
	(when name
	  (remove-tautomaton tautomaton *spec*)
	  (set-and-display-current-tautomaton
	   (rename-object tautomaton name)))))))

(define-autowrite-command (com-retrieve-tautomaton :name t) ()
  (let ((tautomaton 
	 (accept 'tautomaton
;;		 :default nil :display-default nil
		 :prompt "tautomaton name"
		 )))
    (when tautomaton
      (set-and-display-current-tautomaton tautomaton))))

(define-autowrite-command (com-is-empty :name t) ()
  (when (automaton *spec*)
    (automaton-empty (automaton *spec*))))

(define-autowrite-command (com-is-deterministic :name t) ()
  (when (automaton *spec*)
    (format *output-stream* "~A is ~:[not ~;~]deterministic~%"
	    (name (automaton *spec*))
	    (deterministic-p (automaton *spec*)))))

(define-autowrite-command (com-is-minimal :name t) ()
  (when (automaton *spec*)
    (if (deterministic-p (automaton *spec*))
	(format *output-stream* "~A is ~:[not ~;~]minimal~%"
		(name (automaton *spec*))
		(minimal-p (automaton *spec*)))
	(format *output-stream* "~A should be determinized first~%" (name (automaton *spec*))))))

(define-autowrite-command (com-is-simplified :name t) ()
  (when (automaton *spec*)
    (if (deterministic-p (automaton *spec*))
	(format *output-stream* "~A is ~:[not ~;~]simplified~%"
		(name (automaton *spec*))
		(simplified-p (automaton *spec*)))
	(format *output-stream* "~A should be determinized first~%" (name (automaton *spec*))))))

(define-autowrite-command (com-equivalence-classes :name t) ()
  (let ((a (automaton *spec*)))
    (when a
      (if (and (reduced-p a) (deterministic-p a))
	  (format *output-stream* "Equivalence classes of ~A: ~A~%"
		  (name (automaton *spec*))
		  (equivalence-classes (automaton *spec*)))
	  (format *output-stream*
		  "~A should be reduced and determinized first~%" (name a))))))

(define-autowrite-command (com-is-complete :name t) ()
  (when (automaton *spec*)
    (format *output-stream* "~A is ~:[not ~;~]complete~%"
	    (name (automaton *spec*))
	    (complete-p (automaton *spec*)))))

(define-autowrite-command (com-intersect-automaton :name t) ()
  (when (automaton *spec*)
    (let ((aut2 (accept 'automaton
			:prompt "intersection with automaton")))
      (when aut2
	(set-and-display-current-automaton
	 (intersection-automaton (automaton *spec*) aut2))))))

(define-autowrite-command (com-inclusion-automaton :name t) ()
  (when (automaton *spec*)
    (let ((aut2 (accept 'automaton
;;			:default nil :display-default nil
			:prompt "inclusion in automaton"
			)))
      (when aut2
	  (multiple-value-bind (res term)
	      (inclusion-automaton (automaton *spec*) aut2)
	    (if res
		(format *output-stream* "L(~A) included in L(~A)~%"
			(name (automaton *spec*))
			(name aut2))
		(format *output-stream* "L(~A) not included in L(~A) witnessed by ~A~%"
			(name (automaton *spec*))
			(name aut2) term)))))))

(define-autowrite-command (com-equality-automaton :name t) ()
  (let ((aut2 (accept 'automaton
;;		      :default nil :display-default nil
		      :prompt "equality with automaton"
		      )))
    (when aut2
      (multiple-value-bind (res term)
	  (equality-automaton (automaton *spec*) aut2)
	(if res
	    (format *output-stream* "L(~A) = L(~A) ~%"
		    (name (automaton *spec*))
		    (name aut2))
	    (format *output-stream* "L(~A) not equal to L(~A) witnessed by ~A~%"
		    (name (automaton *spec*))
		    (name aut2) term))))))

(define-autowrite-command (com-empty-intersection :name t) ()
  (let ((a (automaton *spec*)))
    (when a
      (let ((aut2 (accept 'automaton
			  :prompt "emptiness of intersection with automaton")))
	(when aut2
	  (multiple-value-bind (res term)
	      (intersection-emptiness a aut2)
	    (if res
		(format *output-stream*
			"L(~A) inter L(~A) is empty ~%"
			(name a)
			(name aut2))
		(format *output-stream*
			"L(~A) inter L(~A) not empty witnessed by ~A~%"
			(name a)
			(name aut2) term))))))))

(define-autowrite-command (com-union-automaton :name t) ()
  (when (automaton *spec*)
    (let ((aut2 (accept 'automaton
;;			:default nil :display-default nil
			:prompt "union with automaton"
			)))
      (when aut2
	(set-and-display-current-automaton
	 (union-automaton (automaton *spec*) aut2))))))

(define-autowrite-command (com-complement-automaton :name t) ()
  (when (automaton *spec*)
    (process-complement)))

(define-autowrite-command (com-complete-automaton :name t) ()
  (when (automaton *spec*)
    (if (complete-p (automaton *spec*))
	(format *output-stream* "~A already complete~%" (name (automaton *spec*)))
	(process-complete))))

(define-autowrite-command (com-nf-automaton :name t) ()
  (when (trs *spec*)
    (let ((aut (seqsys-aut-nf (trs *spec*))))
      (set-and-display-current-automaton aut))))

(define-autowrite-command (com-nf-bullet-automaton :name t) ()
  (when (trs *spec*)
    (let ((aut (seqsys-aut-nf-bullet (trs *spec*))))
      (set-and-display-current-automaton aut))))

(define-autowrite-command (com-redex-automaton :name t) ()
  (when (trs *spec*)
    (let ((aut (make-redex-automaton
		(get-lhs (trs *spec*))
		(signature (trs *spec*)))))
    (set-and-display-current-automaton aut))))

(define-autowrite-command (com-reducible-automaton :name t) ()
  (when (trs *spec*)
    (let ((aut (make-reducible-automaton
		(get-lhs (trs *spec*))
		(signature (trs *spec*)))))
      (set-and-display-current-automaton aut))))

(define-autowrite-command (com-automaton-c-nf :name t) ()
  (when (trs *spec*)
    (process-automaton-c (trs *spec*) :deterministic (not *jacquemard*))))

(define-autowrite-command (com-automaton-c-nf-bullet :name t) ()
  (process-automaton-c (trs *spec*) :deterministic (not *jacquemard*) :bullet t))

(define-autowrite-command (com-automaton-c-nf-bullet-extra :name t) ()
  (process-automaton-c (trs *spec*)
		       :deterministic (not *jacquemard*) :bullet t :extra t))

(define-autowrite-command (com-automaton-c-s :name t) ()
  (when (and (trs *spec*) (termset *spec*))
    (process-automaton-c-s (trs *spec*) (termset *spec*)
			   :deterministic (not *jacquemard*))))

(define-autowrite-command (com-automaton-c-a :name t) ()
  (when (and (trs *spec*) (automaton *spec*))
    (process-automaton-c-a (trs *spec*) (automaton *spec*))))

(define-autowrite-command (com-automaton-d-extra :name t) ()
  (when (trs *spec*)
    (process-automaton-d(trs *spec*)
			:deterministic (not *jacquemard*)
			:extra t)))

(define-autowrite-command (com-automaton-d :name t) ()
  (when (trs *spec*)
    (process-automaton-d (trs *spec*)  :deterministic (not *jacquemard*))))

(define-autowrite-command (com-automaton-d-extra-partial :name t) ()
  (when (trs *spec*)
    (process-automaton-d (trs *spec*) :deterministic (not *jacquemard*) :extra t :partial t)))

(define-autowrite-command (com-automaton-d-partial :name t) ()
  (when (trs *spec*)
    (process-automaton-d (trs *spec*)  :deterministic (not *jacquemard*) :partial t)))

(define-autowrite-command (com-termset-automaton :name t) ()
  (when (termset *spec*)
    (let ((aut (aut-termset (termset *spec*))))
    (set-and-display-current-automaton aut))))

(define-autowrite-command (com-reduce-automaton :name t) ()
  (when (automaton *spec*)
    (if (reduced-p (automaton *spec*))
	(format *output-stream* "~A already reduced~%"
		(name (automaton *spec*)))
	(process-reduce))))

(define-autowrite-command (com-duplicate-automaton :name t) ()
  (let ((a (automaton *spec*)))
    (when a
      (let ((aut (duplicate-automaton a)))
	(rename-object aut (compose-name "copy-of-" (name a)))
	(set-and-display-current-automaton aut)))))

(define-autowrite-command (com-determinize-automaton :name t) ()
  (when (automaton *spec*)
    (if (deterministic-p (automaton *spec*))
	(format *output-stream* "~A already deterministic~%"
		(name (automaton *spec*)))
	(process-determinize))))

(define-autowrite-command (com-minimize-automaton :name t) ()
  (when (automaton *spec*)
    (if (deterministic-p (automaton *spec*))
	(if (minimal-p (automaton *spec*))
	    (format *output-stream* "~A already minimal~%"
		    (name (automaton *spec*)))
	    (process-minimize))
	 (format *output-stream* "~A should be determinized first~%"
		    (name (automaton *spec*))))))

(define-autowrite-command (com-simplify-automaton :name t) ()
  (when (automaton *spec*)
    (if (simplified-p (automaton *spec*))
	(format *output-stream* "~A already simplified~%"
		(name (automaton *spec*)))
	(process-simplify))))

(define-autowrite-command (com-epsilon-closure :name t) ()
  (when (automaton *spec*)
    (let ((aut (epsilon-closure-automaton
		(automaton *spec*))))
    (set-and-display-current-automaton aut))))

(define-autowrite-command (com-save-automaton :name t) ()
  (when (automaton *spec*)
    (let ((*limit-transitions* nil))
      (write-object-to-file (automaton *spec*)
			    (name (automaton *spec*))
			    #'show))))

(define-autowrite-command (com-clear-automaton :name t) ()
  (clear-current-automaton)
  (window-clear *automaton-pane*))

(define-autowrite-command (com-clear-all :name t) ()
  (set-and-display-current-spec (get-spec "empty")))

(define-autowrite-command (com-terminating-trs :name t) ()
  (let ((trs (trs *spec*)))
    (when trs 
      (let* ((rules (get-rules trs))
	     (rule (find-if (lambda (r) (var-p (left-handside r))) (rules-list rules))))
	(cond
	  (rule
	   (format *output-stream* "~A is ~:[not ~;~]terminating~%" (name trs) rule))
	  ((growing rules) (process-growing-terminating-trs trs))
	  ((growing (inverse-rules rules)) (process-inverse-growing-terminating-trs trs))
	  (t (format *output-stream* "method applicable only to growing or inverse-growing TRSs~%")))))))

(define-autowrite-command (com-inverse-trs :name t) ()
  (when (trs *spec*)
    (set-and-display-current-trs (inverse-seqsys (trs *spec*)))))
;;;;; End of Commands
