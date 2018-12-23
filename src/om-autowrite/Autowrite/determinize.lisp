(in-package :nautowrite)

(defmethod deterministic-automaton-p ((automaton abstract-automaton))
  (deterministic-p (transitions-of automaton)))

(defmethod determinize-automaton ((automaton table-automaton) &key (fly nil))
  :documentation "returns A if deterministic or the determinized of A otherwise"
  (if (deterministic-p automaton)
      (if fly (to-fly-automaton automaton) automaton)
      (let ((df (determinize-automaton (to-fly-automaton automaton))))
	(if fly df (compile-automaton df)))))


