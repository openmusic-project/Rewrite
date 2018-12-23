(in-package :terms)
(defgeneric path-prefix-p (path1 path2)
  (:documentation "check whether path1 is a prefix of path2"))

(defgeneric path-suffix-p (path1 path2)
  (:documentation "check whether path1 is a suffix of path2"))

(defun less-or-equal-path (p1 p2)
  (or (null p1)
      (and (not (null p2))
	   (and (= (car p1) (car p2)) (less-or-equal-path (cdr p1) (cdr p2))))))

(defun sort-paths (paths)
  (sort paths #'less-or-equal-path))

(defun outer-paths (paths)
  (let ((sp (sort-paths (copy-list paths))))
    (and (not (null sp))
	 (cons (car sp)
		   (outer-paths
		    (if (member (car sp) (cdr sp) :test #'less-or-equal-path)
			(delete-if #'(lambda (x)
				   (less-or-equal-path (car sp) x)) (cdr sp))
			(cdr sp)))))))

(defmethod path-prefix-p ((p1 list) (p2 list))
  (or (endp p1)
      (and (not (endp p2))
	   (= (car p1) (car p2))
	   (path-prefix-p (cdr p1) (cdr p2)))))
	  
(defmethod path-suffix-p ((p1 list) (p2 list))
  (path-prefix-p (reverse p1) (reverse p2)))

(defun residual-path (i path)
  (and path
       (= (car path) i)
       (list (cdr path))))
      
(defun residual-paths (i paths)
  (mappend (lambda (path)
	     (residual-path i path))
	   paths))

