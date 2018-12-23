(in-package :nautowrite)

(defmethod representative-state ((state casted-state) (classes list))
  (cons state
	(car (contents
	      (find-if
	       (lambda (class) (container-member state class))
	       classes)))))

(defmethod representative-states ((states list) (classes list))
  (mapcar (lambda (state)
	    (representative-state state classes))
	  states))

(defmethod nquotient-automaton ((automaton table-automaton) (representative-states list))
  (let ((numbered (states-numbered-p automaton)))
    (unless numbered (number-states automaton))
    (let ((*states-mapping* representative-states))
      (napply-states-mapping automaton))
    (unless numbered (name-states automaton))
    automaton))
