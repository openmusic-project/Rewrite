(in-package :nautowrite)

(defun get-automaton (name)
  (gethash name (table (automata *spec*))))

(defun get-tautomaton (name)
  (gethash name (table (tautomata *spec*))))

(defgeneric set-current-automaton (automaton))

(defmethod set-current-automaton ((automaton abstract-automaton))
  (setf (automaton *spec*) automaton)
  (add-current-automaton))

(defun set-current-tautomaton (tautomaton)
  (setf (tautomaton *spec*) tautomaton)
  (add-current-tautomaton))

(defun list-automata ()
  (list-keys (table (automata *spec*))))

(defgeneric automaton-empty (automaton))

(defmethod automaton-empty ((automaton abstract-automaton))
  (multiple-value-bind (res term)
      (automaton-emptiness automaton)
    (format *output-stream* "L(~A) " (automaton-name automaton))
    (if res
	(format *output-stream* "empty~%")
	(format *output-stream* "not empty witnessed by ~A~%" term))))
