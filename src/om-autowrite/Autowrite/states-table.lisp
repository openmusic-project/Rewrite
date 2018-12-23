(in-package :nautowrite)

(defclass cast-states-mixin ()
  ((states-table :initarg :states-table :initform (make-empty-automaton-states-table) :accessor states-table)))

(defmethod nfilter-states-table ((states ordered-container) (states-table hash-table))
;;  (format *trace-output* "nfilter-states-table ~A~%" states-table)
  (maphash
   (lambda (k v)
     (unless (container-member v states)
       (remhash k states-table)))
   states-table))

(defmethod reset-states-table ((transitions cast-states-mixin))
  (clrhash (states-table transitions)))

(defun states-from-states-table (states-table)
  (make-ordered-container (list-values states-table) :clean nil :sort t))

(defmethod biggest-state ((states-table hash-table))
  (hash-reduce #'max states-table :key #'state-size :initial-value 0))