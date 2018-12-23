(in-package :nautowrite)

(defun filter-afai (afai nlstates)
  (remove-if (lambda (x)
	       (every (lambda (y) (member y nlstates :test #'eq)) x)) afai))

(defun arrange-for-arities (l amax)
  (let ((arity-vector (make-array (1+ amax))))
    (progn
      (setf (aref arity-vector 0) (arrange l 0))
      (do ((i 1 (1+ i)))
	  ((= i (1+ amax)))
	(setf
	 (aref arity-vector i)
	 (arrange-list
	  l
	  (aref arity-vector (1- i)))))
      arity-vector)))


(defun filter-afa (afa states arities)
  (loop
     for i from 0 below (length afa)
     do (setf (aref afa i)
	      (and (member i arities)
		   (filter-afai (aref afa i) states))))
  afa)

(defun filter-afa-with-signature (afa states signature)
  (filter-afa afa states (signature-arities signature)))

(defun arrange-for-arities-and-filter (allstates amax oldstates signature)
  (let ((afa (arrange-for-arities allstates amax)))
    (filter-afa-with-signature afa oldstates signature)))
