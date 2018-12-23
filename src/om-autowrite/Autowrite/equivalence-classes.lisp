(in-package :nautowrite)
;;; avec transitions
;;; avec tableau de blocs
;;; avec bit-vectors ou containers selon la variable *with-bv*
;;; standard minimization algorithm in O(N²)

(defvar *eqclasses*)
(defvar *eqindex*)

(defmethod set-eqclass (i eqclass)
  (assert (not (empty-sstates-p eqclass)))
  (setf (aref *eqclasses* i) eqclass))

(defmethod add-eqclass (eqclass)
  (assert (not (empty-sstates-p eqclass)))
  (setf (aref *eqclasses* *eqindex*) eqclass)
  (incf *eqindex*))

(defun in-the-same-eqclass (q1 q2)
  (or
   (eq q1 q2)
   (and
    q1 ;; permet de gerer le cas ou l automate est incomplet
    q2
    (loop
       for i from 0 below *eqindex*
       when (and (in-sstates q1 (aref *eqclasses* i))
		 (in-sstates q2 (aref *eqclasses* i)))
       do (return-from in-the-same-eqclass t)))))

;; si on suit les memes arguments on arrive dans des etats d'une meme classe
(defmethod possibly-equivalent-to-class (q newclass (transitions transitions))
  (loop
     for qp in (sstates-contents newclass)
     do (unless
	    (possibly-equivalent-to-state
	     (sstates-realstate q)
	     (sstates-realstate qp) transitions)
	  (return-from possibly-equivalent-to-class nil))
     finally (return t)))

(defun print-eqclasses (&optional (stream t))
  (loop
     for i from 0 below *eqindex*
;;     do (format stream "~A " (aref *eqclasses* i)))
     do (print-sstates (aref *eqclasses* i)))
  (format stream "~%"))

(defgeneric partition-eqclass (eqclass transitions)
  (:documentation ""))

(defmethod partition-eqclass (eqclass (transitions transitions))
;;   (format *trace-output* "partition-eqclass " )
;;   (print-sstates eqclass *trace-output*) 
;;   (format *trace-output* "~%" )
;; (print-eqclasses *trace-output*)
  (loop
     with partition = nil
     for q in (sstates-contents eqclass)
     do (let ((ncl (find-if
		    (lambda (newclass)
		      (possibly-equivalent-to-class q newclass transitions))
		    partition)))
	  (if ncl
	      (setf ncl (sstates-adjoin q ncl))
	      (push (make-sstates-from-state q) partition)))
;;	  (format *error-output* "q~A partition ~A ~%" q partition)
     finally (return partition)))

;;;; il faut enlever des classes celle qu'on traite et mettre a la place les newclasses qui lui correspondent
(defgeneric refine-partition (transitions)
  (:documentation "replace each eqclass by its refined set of eqclasses"))

(defmethod refine-partition ((transitions transitions))
;;  (print-eqclasses)
  (dotimes (i *eqindex*)
    (let ((eqclass (aref *eqclasses* i)))
      (unless (sstates-singleton-p eqclass))
	(let ((partition (partition-eqclass eqclass transitions)))
	  (set-eqclass i (first partition))
	  (when (cdr partition)
	    (dolist (c (cdr partition))
	      (add-eqclass c)))))))

(defmethod compute-equivalence-classes (finalstates nonfinalstates transitions)
  (add-eqclass finalstates)
;;  (format *trace-output* "nonfinal ~A ~%" nonfinalstates)
  (unless (empty-sstates-p nonfinalstates)
    (add-eqclass nonfinalstates))
  (do ((old-index (- *eqindex* 1)))
      ((= *eqindex* old-index) (loop for i from 0 below *eqindex*  collect (aref *eqclasses* i)))
    (setf old-index *eqindex*)
    (refine-partition (uncommutative-for-equivalence-classes transitions))))

(defmethod equivalence-classes-automaton ((automaton table-automaton))
  (assert (and (reduced-p automaton)
	       (deterministic-p automaton)
;;	       (complete-p automaton)
	       ))
;;  (unless (complete-p automaton)
;;    (format *error-output* "equivalence classes of uncomplete automaton~%"))
  (let ((states-named (states-named-p automaton)))
    (number-states automaton)
    (with-states-vector (get-states automaton)
      (let* ((finalstates (make-sstates (get-finalstates automaton)))
	     (states (make-sstates (get-states automaton)))
	     (nonfinalstates (sstates-difference states finalstates)))
	(when (empty-sstates-p finalstates)
	  (return-from equivalence-classes-automaton
	    (list (sstates-to-container nonfinalstates))))
	(when (and (complete-p automaton) (empty-sstates-p nonfinalstates))
	  (return-from equivalence-classes-automaton (list (sstates-to-container finalstates))))
	;;    (format *error-output* "equivalence classes ~%")
	(let*
	    ((*eqclasses* (make-array *states-vector-len*))
	     (*eqindex* 0)
	     (transitions (transitions-of automaton))
	     (equivalence-classes
	      (compute-equivalence-classes finalstates nonfinalstates transitions))
	     (containers (mapcar #'sstates-to-container equivalence-classes)))
	  (when states-named (name-states automaton))
	  containers)))))
