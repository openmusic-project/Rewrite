(in-package :nautowrite)
 
(defmethod make-transition ((ip1 index-point) (ip2 (eql nil)))
  (make-instance 'transition :lh ip1 :rh ip2))

(defclass index-tree (table-automaton)
  ((initial-index-point :initform nil :initarg :initial-index-point :reader initial-index-point)
   (failure-transitions :initform nil
			:initarg :failure-transitions
			:accessor failure-transitions)))

(defmethod show :after ((a index-tree) &optional (stream t))
  (format stream "Failure transitions~% ~A~%" (failure-transitions a)))

(defgeneric transition-from-delta-pair (s p))

(defmethod transition-from-delta-pair ((s index-point) (pair cons))
  (add-transition-to-transitions
   (list (car pair) (cast-state s))
   (cdr pair)
   *global-transitions*))

(defgeneric transitions-from-index-point (index-point)
  (:documentation "list of the transistions outgoing INDEX-POINT"))
  
(defmethod transitions-from-index-point ((s index-point))
  (mapc
   (lambda (pair)
     (transition-from-delta-pair s pair))
   (delta-alist s))
  (mapc #'transitions-from-index-point (mapcar #'cdr (delta-alist s))))

(defgeneric make-index-tree (index-point)
  (:documentation "index-tree outgoing INDEX-POINT"))

(defmethod make-index-tree ((s index-point))
  (with-new-transitions
    (cast-state s)
    (transitions-from-index-point s)
    (let* ((states (get-states-from *global-transitions*))
	   (finalstates (container-remove-if-not (lambda (ip) (endp (delta-alist (state ip)))) states))
	   (failure-transitions '()))
      (mapc
       (lambda (s)
	 (let ((failstate (phi (state s))))
	   (when failstate
	     (push (make-transition s (cast-state failstate)) failure-transitions))))
       (contents states))
      (make-instance 'index-tree
		     :name "fb-index-tree"
		     :transitions *global-transitions*
		     :finalstates finalstates
		     :initial-index-point (cast-state s)
		     :failure-transitions (nreverse failure-transitions)
		     :reduced t))))

(defmethod get-states ((it index-tree))
  (container-adjoin (initial-index-point it) (call-next-method)))
  
