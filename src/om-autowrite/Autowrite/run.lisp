(in-package :nautowrite)

(defclass run-context ()
  ((done :initform nil)
   (term :initarg term :reader term)
   (automaton :initarg :automaton :reader automaton)
   (state-size-max :accessor state-size-max :initform 0)
   (result :initform nil :reader run-result)))

(defun make-run-context (term automaton)
  (make-instance
   'run
   :term term
   :automaton automaton))

(defmethod compute-run ((run-context run-context))
  (let ((*in-context*
