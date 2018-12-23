;; -*- Mode: Lisp; Package: AUTOWRITE -*-

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


;; faire que les ) trailing (ou manquantes) ne bloquent pas l'analyse
;; tuer les processus avant de tout quitter

(in-package :gautowrite)

;(defun get-absolute-filename-from-browser ()
;  (car (clim-demo::file-selector-test)))

(defmethod process-yield ())

(defmethod show-automaton ((automaton table-automaton) &key (newline nil))
  (format *output-stream* "~A " (name automaton))
  (show-automaton-characteristics
   automaton
   :stream *output-stream*
   :newline newline))

(defun display-current-automaton ()
  (window-clear *automaton-pane*)
  (let ((automaton (automaton *spec*)))
    (when automaton
      (show automaton *automaton-pane*)
      (show-automaton automaton :newline t))))

(defun display-current-tautomaton ()
  (window-clear *tautomaton-pane*)
  (when (tautomaton *spec*)
    (show (tautomaton *spec*) *tautomaton-pane*)))

(defun display-current-trs ()
  (window-clear *trs-pane*)
  (when (trs *spec*)
    (display-trs (trs *spec*) *trs-pane*)))

(defun display-term (term &key (prefix "CURRENT TERM") (clear t))
  (when clear (window-clear *term-pane*))
  (when term
    (format *term-pane* "~A~% ~A~%" prefix term)))

(defun display-tautomaton (tautomaton &key (prefix "CURRENT TAUTOMATON") (clear t))
  (when clear (window-clear *tautomaton-pane*))
  (when tautomaton
    (format *tautomaton-pane* "~A~% ~A~% " prefix tautomaton)))

(defun display-current-term ()
  (display-term (term *spec*)))

(defun display-current-termset ()
  (window-clear *termset-pane*)
  (when (termset *spec*)
	(display-termset (termset *spec*) *termset-pane*)))

(defun display-current-spec ()
  (when *spec*
    (display-current-trs)
    (display-current-approximation)
    (display-current-automaton)
    (display-current-tautomaton)
    (display-current-termset)
    (display-current-term)
    (window-clear *output-stream*)))

(defun set-and-display-current-term (term)
  (set-current-term term)
  (display-current-term))

(defun load-term (new-term)
  (when new-term
    (setf (previous-term *spec*) nil)
    (set-and-display-current-term new-term)))

(defun clear-current-term ()
  (setf (term *spec*) nil)
  (window-clear *term-pane*))

(defun set-and-display-current-termset (termset)
  (set-current-termset termset)
  (display-current-termset))

(defun clear-current-termset ()
  (setf (termset *spec*) nil)
  (window-clear *termset-pane*))

;;; termset

(defun load-termset (name new-termset)
  (when new-termset
    (let ((termset (build-termset name new-termset)))
      (set-and-display-current-termset termset))))

;;; trs

(defun sequentiality (sys &key (deterministic t) (extra nil) (automata t))
  (block nil
    (unless automata
      (multiple-value-bind (succeeded res)
	  (try-to-check-seq-with-properties sys)
	(when succeeded
	  (display-sequential res sys :extra extra)
	  (return))))
    (let (aut-c aut-d)
      (avec-temps-to-output-stream
       (progn
	 (setf aut-c
	       (seqsys-aut-c sys :extra extra :bullet t
			     :deterministic deterministic))
	 (show-automaton
	  aut-c
	  :newline nil)))
      (avec-temps-to-output-stream
       (progn
	 (setf aut-d
	       (seqsys-aut-d
		sys
		:deterministic deterministic
		:partial t
		:extra extra))
	 (show-automaton aut-d :newline nil)))
    (display-sequentiality-results sys aut-d :extra extra))))

(defun trs-fb-to-constructor (sys &optional (elimination nil))
  (when sys
    (cond
      ((constructor-p (get-rules sys))
       (format *output-stream* "~A is already constructor~%" (name sys)))
      ((not (forward-branching-index-tree sys))
       (format *output-stream* "~A is not forward-branching~%" (name sys)))
      (t (set-and-display-current-trs (salinier sys elimination))))))

;;; automaton
(defgeneric set-and-display-current-automaton (automaton))
(defmethod set-and-display-current-automaton ((automaton abstract-automaton))
;;  (format *output-stream* "~A~%" automaton)
  (set-current-automaton automaton)
  (display-current-automaton))
  
(defun set-and-display-current-tautomaton (tautomaton)
  (set-current-tautomaton tautomaton)
  (display-current-tautomaton))
  
;;; reduction
(defun apply-reduction (&key needed (parallel nil))
  (when (and (term *spec*) (trs *spec*))
    (if (seqsys-merge-signature (trs *spec*) (signature-from (term *spec*)))
	(let ((rp (redex-positions
		   (term *spec*)
		   (trs *spec*))))
	  (if (endp rp)
	      (format *output-stream* "already in normal form~%")
	      (let ((nrp (if needed
			     (needed-redex-positions
			      (term *spec*)
			      (trs *spec*)
			      :extra (in-signature-extra
				      (signature-from
				       (term *spec*))))
			     (outer-positions rp))))
		(if (endp nrp)
		    (format *output-stream* "no needed-redex~%")
		    (progn
		      (if parallel
			  (parallel-reduction-step nrp)
			  (reduction-step (car nrp)))
		      (window-clear *term-pane*)
		      (display-term
		       (previous-term *spec*) :prefix "PREVIOUS TERM")
		      (display-term (term *spec*) :clear nil))
		))))
	(format *error-output* "incompatible signatures~%"))))

(defun apply-reduction-nf (&key (needed nil))
  (when (and (term *spec*) (trs *spec*))
    (if (seqsys-normal-form (term *spec*) (trs *spec*))
	(format *output-stream* "already in normal form~%")
	(progn
	  (let* ((initial-term (term *spec*))
		 (fun
		  (if needed
		      (lambda (term trs)
			(needed-redex-positions
			 term
			 trs
			 :extra
			 (in-signature-extra
				 (signature-from initial-term))))
		      #'outermost-redex-positions)))
	    (do
	     ((nrp
	       (funcall fun (term *spec*) (trs *spec*))
	       (funcall fun (term *spec*) (trs *spec*))))
	     ((or (endp nrp) (compare-object (term *spec*)
					     (previous-term *spec*))) t)
	      (if needed
		  (reduction-step (car nrp) :tr t)
		  (parallel-reduction-step nrp :tr t))
	      (process-yield))
	    (when
		(and
		 (redex-positions (term *spec*) (trs *spec*))
		   (not (contains-bullet-p (term *spec*))))
		(if
		  (compare-object (term *spec*) (previous-term *spec*))
		 (format
		  *output-stream*
		  "looping computation~%")
		 (format
		  *output-stream*
		  "no needed redex~%")))
	      (window-clear *term-pane*)
	      (display-term initial-term :prefix "INITIAL TERM")
	      (display-term (term *spec*) :prefix "FINAL TERM" :clear nil))))))

(defun clear-current-automaton ()
  (setf (automaton *spec*) nil))

(defun clear-current-trs ()
  (setf (trs *spec*) nil))

(defun complementation ()
  (let ((aut (complement-automaton (automaton *spec*))))
    (set-and-display-current-automaton aut)))

(defun complete ()
  (let ((aut (complete-automaton (automaton *spec*))))
    (set-and-display-current-automaton aut)))

(defun minimize ()
  (let ((aut (minimize-automaton (automaton *spec*))))
    (set-and-display-current-automaton aut)))

(defun simplify ()
  (let ((aut (simplify-automaton (automaton *spec*))))
    (set-and-display-current-automaton aut)))

(defun determinize ()
  (let ((aut (determinize-automaton (automaton *spec*))))
    (set-and-display-current-automaton aut)))

(defun my-reduce ()
  (let ((aut (reduce-automaton (automaton *spec*))))
    (set-and-display-current-automaton aut)))

(defun set-and-display-current-trs (trs)
  (set-current-trs trs)
  (display-current-trs)
  (display-current-approximation))


(defun set-and-display-current-spec (spec)
  (set-current-spec spec)
  (display-current-spec))

(defun automaton-d (sys &key (deterministic t) (extra nil) (partial nil))
  (when sys
    (let ((aut (seqsys-aut-d sys :deterministic deterministic :extra extra :partial partial)))
      (set-and-display-current-automaton aut))))

(defun automaton-d-extra (sys)
  (automaton-d sys :extra t))

(defun automaton-c (sys &key (deterministic t) (bullet nil) (extra nil))
  (when sys
    (set-and-display-current-automaton
     (seqsys-aut-c sys
		   :bullet bullet
		   :extra extra
		   :deterministic deterministic))))

(defun automaton-c-s (trs termset &key (deterministic t))
  (when (and trs termset)
    (seqsys-change-aut-t trs (aut-termset termset))
    (set-and-display-current-automaton
     (seqsys-aut-c trs :deterministic deterministic))))

(defun automaton-c-a (trs automaton &key (deterministic t))
  (when (and trs automaton)
    (seqsys-change-aut-t trs automaton)
    (set-and-display-current-automaton
     (seqsys-aut-c-t trs :deterministic deterministic))))
