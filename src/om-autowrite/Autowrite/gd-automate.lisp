(in-package :nautowrite)
(defun g-create-transitions (key bc srh prh)
  (mapc
   (lambda (x) (add-transition-to-transitions
		key
		(make-dstate srh bc x :deterministic nil) *global-transitions*))
   prh))

(defun g-adapt-p-when-redex (p1)
  (if (endp p1)
      (list *cstates*)
      (my-delete-duplicates
       (mapcar
	(lambda (x) (union-sort-casted-states *cstates* x))
	p1))))

;; rhap liste de containers 
(defun g-transform-rhap (rhap)
  (my-delete-duplicates
   (mapcar
    #'flatten-and-sort
    (cartesian-product 
     (mapcar #'not-null-powerset (mapcar #'contents rhap))))))
  
(defun flatten-and-sort (l)
  (sort-casted-states
   (my-remove-duplicates (reduce #'append l))))
