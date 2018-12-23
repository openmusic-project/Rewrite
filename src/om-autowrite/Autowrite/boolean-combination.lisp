(in-package :nautowrite)

(defun b-not (b) (if (zerop b) 1 0))
(defun b-or (b1 b2) (if (and (zerop b1) (zerop b2)) 0 1))
(defun b-and (b1 b2) (if (and (not (zerop b1)) (not (zerop b2))) 1 0))

(defun make-h-1-fun-tab (n)
  (let ((tab (make-array n)))
    (setf (aref tab 0)
	  (lambda (y-bv) (b-or (aref y-bv 0) (b-not (aref y-bv 2)))))
    tab))

(defun make-h-1-alist (h-1-fun-tab m)
  (let ((n (length h-1-fun-tab)))
    (mapcar (lambda (yl)
	      (let ((a (make-array m :element-type 'bit :initial-contents yl))
		    (b (make-array n :element-type 'bit)))
		(loop
		 for i from 0 below n
		 do (setf (aref b i) (funcall (aref h-1-fun-tab i) a)))
	      (cons a b)))
	  (cartesian-product
	   (loop
	      with bits = '(0 1)
	      repeat m
	      collect bits))))

(defun make-h-alist (h-1-alist)
  (let ((n (length (cdr (car h-1-alist)))))
    (mapcar (lambda (l)
	      (let ((a (make-array n :element-type 'bit :initial-contents l)))
		(cons a (car (rassoc a h-1-alist :test #'equal)))))
	    (cartesian-product
	     (loop
		with bits = '(0 1)
		repeat n
		collect bits)))))


      '((#*000 . #*10)
	(#*001 . #*00)
	(#*010 . #*11)
	(#*011 . #*01)
	(#*100 . #*10)
	(#*101 . #*10)
	(#*110 . #*11)
	(#*111 . #*11)))

(defun hbv (bv alist)
  (cdr (assoc bv alist :test #'equal)))

(make-h-alist (make-h-1-alist (make-h-1-fun-tab 1) 3))

(let* ((alist (make-h-alist (make-h-1-alist (make-h-1-fun-tab 1) 3)))
       (h (lambda (bv)
	    (hbv bv alist))))
  (hvprojection *a* h))
