(in-package :terms)

(defclass my-position ()
  ((path :reader path :initarg :path)))

(defun make-position (path)
  (assert (every #'integerp path))
  (make-instance 'my-position :path path))

(defgeneric empty-position-p (position)
  (:documentation "true if p is the empty position"))

(defgeneric position-prefix-p (position1 position2)
  (:documentation "true if POSITION1 is a prefix of POSITION2"))

(defgeneric position-suffix-p (position1 position2)
  (:documentation "true if POSITION1 is a suffix of POSITION2"))

(defgeneric extend-position (position integer)
  (:documentation "extends the path of the position with integer"))

(defgeneric less-or-equal-position (position1 position2)
  (:documentation "check whether position1 <= position2"))

(defgeneric sort-positions (positions)
  (:documentation "sort positions (destructive)"))

(defgeneric outer-positions (positions)
  (:documentation "return the list of outermost positions of the list POSITIONS of positions"))

(defgeneric position-to-string (position)
  (:documentation "returns POSITION as a printable string"))

(defgeneric positions-to-string (positions)
  (:documentation "returns a list of strings corresponding to the positions in  POSITIONS"))

(defmethod empty-position-p ((p my-position))
  (endp (path p)))

(defmethod print-object ((p my-position) stream)
  (if (empty-position-p p)
      (format stream "E")
      (display-sequence (path p) stream :sep ".")))

(defmethod show ((p my-position) &optional (stream t))
  (print-object p stream))

(defmethod extend-position ((p my-position) (i integer))
  (make-position
   (append (path p) (list i))))

(defmethod less-or-equal-position ((p1 my-position)(p2 my-position))
  (less-or-equal-path (path p1) (path p2)))

(defmethod sort-positions ((positions list))
  (sort positions #'less-or-equal-position))

(defmethod outer-positions ((positions list))
  (let ((sp (sort-positions (copy-list positions))))
    (and (not (null sp))
	 (cons (car sp)
		   (outer-positions
		    (if (member (car sp) (cdr sp) :test #'less-or-equal-position)
			(delete-if #'(lambda (x)
				   (less-or-equal-position (car sp) x)) (cdr sp))
			(cdr sp)))))))

(defmethod position-to-string ((p my-position))
  (if (empty-position-p p)
      "epsilon"
  (reduce #'(lambda (x y) (concatenate 'string (concatenate 'string x ".") y))
	  (mapcar (lambda (x) (format nil "~S" x)) (path p)))))

(defmethod positions-to-string ((positions list))
  (if (endp positions)
      ""
      (do ((np (mapcar #'position-to-string positions) (cdr np))
	    (res "" (concatenate 'string (concatenate 'string res (car np))  ", ")))
	   ((null (cdr np)) (concatenate 'string res (car np))))))

(defmethod position-prefix-p ((p1 my-position) (p2 my-position))
  (path-prefix-p (path p1) (path p2)))
	  
(defmethod position-suffix-p ((p1  my-position) (p2 my-position))
  (path-suffix-p p1 p2))

