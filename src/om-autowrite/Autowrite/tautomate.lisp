(in-package :nautowrite)

(defclass tautomata ()
  ((tautomaton-table :initform (make-hash-table :test #'equal)
		     :accessor table)))

(defun make-tautomata ()
  (make-instance 'tautomata))

(defclass tautomaton (abstract-automaton)
  ((automata :accessor automata :initarg :automata)
   (fun :initarg :fun :reader tautomaton-fun)))

(defun make-tautomaton (name automata &optional (fun #'intersection-automaton-compatible-gen))
  (assert (every (lambda (a) (typep a 'abstract-automaton)) automata))
  (let ((signature (reduce #'merge-signature (mapcar #'signature automata))))
    (if signature
	(make-instance 'tautomaton :name name :automata automata :signature signature :fun fun)
	(format *error-output* "Incompatible signatures"))))

;;; faire une classe state-term qui hérite à la fois de state et term et faire que tous
;;; les defun qui s'appliquent à un term soient maintenant des méthodes sur la classe state-term

(defmethod show :after ((tautomaton tautomaton) &optional (stream t))
  (format stream ": ")
  (display-sequence (automata tautomaton) stream))

(defgeneric add-automaton-to-tautomaton (automaton tautomaton))

(defmethod add-automaton-to-tautomaton ((automaton abstract-automaton) (tautomaton tautomaton))
  (let ((automata (automata tautomaton)))
    (unless (member automaton automata)
      (let ((signature (merge-signature (signature tautomaton) (signature automaton))))
	(if signature
	    (setf (automata tautomaton)
		  (append automata (list automaton))
		  (signature tautomaton)
		  signature)
	    (format *error-output* "Incompatible signatures"))))))

(defmethod recognized-p ((term term) (ta tautomaton))
  (every
   (lambda (automaton) (recognized-p term automaton))
   (automata ta)))

(defgeneric tcompute-target (term tautomaton))

(defmethod tcompute-target (term (tautomaton tautomaton))
  (mapcar
   (lambda (automaton)
     (compute-target term automaton))  
   (automata tautomaton)))

(defgeneric projection (tautomaton i)
  (:documentation "i-th projection of tuple of automata TAUTOMATON"))

(defmethod projection ((tautomaton tautomaton) (i integer))
  (assert (< i (length (automata tautomaton))))
  (nth i (automata tautomaton)))

(defmethod op-automata-list ((automata list) &key (op 'intersection-automaton)
			     (cop 'simplify-automaton))
  (assert automata)
  (let ((a (funcall cop (car automata))))
    (mapc
     (lambda (ap)
       (setf a (funcall op a (funcall cop ap)))
       (setf a (funcall cop a))
       (show-automaton-characteristics a :stream t))
     (cdr automata))
     (format *output-stream*" ~A~%" a)
    a))

(defmethod min-union-automata ((automata list))
  (op-automata-list automata :op #'union-automaton :cop #'simplify-automaton))

(defmethod min-intersection-automata ((automata list))
  (op-automata-list automata :op #'intersection-automaton :cop #'simplify-automaton))

(defmethod intersection-automata ((automata list))
  (op-automata-list automata :op #'intersection-automaton :cop #'identity))

(defmethod union-automata ((automata list))
  (op-automata-list automata :op #'union-automaton :cop #'identity))
