(in-package :nautowrite)

(defvar *states-vector*)
(defvar *states-vector-len*)

(defgeneric expand-bv (bit-vector)
  (:documentation "transforms bit-vector to list of states according to *states-vector*"))

(defmethod expand-bv ((bv bit-vector))
  (loop
     for i from 0 below *states-vector-len*
     unless (zerop (aref bv i))
     collect (aref *states-vector* i)))

(defmethod create-state-vector ((container container))
  (let ((states (sort (copy-list (contents container)) #'< :key #'state-number)))
    (make-array (length states) :initial-contents states)))

(defun set-states-vector (states)
  (setf *states-vector* (create-state-vector states)
	*states-vector-len* (length *states-vector*)))

(defun new-states-vector (states)
  (setf *states-vector* (create-state-vector states)
	*states-vector-len* (length *states-vector*)))

(defmacro with-states-vector (states &body body)
  `(let* ((*states-vector* (create-state-vector ,states))
	  (*states-vector-len* (length *states-vector*)))
     ,@body))

(defun make-empty-bv ()
  (make-array *states-vector-len* :element-type 'bit))

(defmethod states-to-bv ((states list))
;;  (format *trace-output* "states-to-bv ~A~%" *states-vector*)
  (let ((positions (mapcar (lambda (casted-state)
			     (state-number casted-state))
			   states)))
;;  (format *trace-output* "positions ~A~%" positions)
    (loop
       with bv = (make-empty-bv)
       for p in positions
       do (setf (aref bv p) 1)
       finally (return bv))))

(defmethod istate-to-bv ((q integer))
  (let ((bv (make-empty-bv)))
    (setf (aref bv q) 1)
    bv))

(defgeneric empty-bv-p (bit-vector)
  (:documentation "T if BIT-VECTOR contains only zeros"))

(defmethod empty-bv-p ((bv bit-vector))
  (every #'zerop bv))
