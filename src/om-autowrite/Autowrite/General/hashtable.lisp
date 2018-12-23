(in-package :general)

(defgeneric list-keys (hash-table &key fun)
  (:documentation "list the keys of a hashtable"))

(defgeneric list-values (hash-table)
  (:documentation "list the values of a hashtable"))

(defmethod list-keys ((ht hash-table) &key (fun #'identity))
  (let ((l nil))
    (maphash #'(lambda (key value) (declare (ignore value))
		       (push (funcall fun key) l))
	     ht)
    l))

(defmethod list-values ((ht hash-table))
    (let ((l nil))
      (maphash
       (lambda (key value) (declare (ignore key))
	 (push value l))
       ht)
      l))

(defmethod hash-reduce (f (ht hash-table) &key (key #'identity) (initial-value 0 supplied-p))
  (let (value)
    (with-hash-table-iterator (next ht)
      (setf value
	    (if supplied-p
		initial-value
		(multiple-value-bind (found? k v) (next)
		  (declare (ignore k))
		  (if found?
		      (funcall key v)
		      (assert "no initial value in hash-reduce")))))
      (loop
	(multiple-value-bind (found? k v) (next)
	  (declare (ignore k))
	  (unless found? (return-from hash-reduce value))
	  (setf value (funcall f value  (funcall key v))))))))
