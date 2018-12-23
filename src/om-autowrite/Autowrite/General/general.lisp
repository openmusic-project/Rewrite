(in-package :general)

(defun onep (n)
  (= 1 n))

(defun normalize-string (s n)
  (let ((l (length s)))
    (if (>= l n)
	s
	(concatenate 'string " " s (make-string (- n l) :initial-element #\space)))))

(let*
    ((counter 0)
     (chars (make-array 4 :initial-contents '("\/" "\-" "\\" "\|")))
     (tog 0)
     (len (length chars)))
  (defun next-char ()
    (setf counter (mod (1+ counter) 100000))
    (if (zerop (mod counter 1000))
	(progn
	  (setf tog (mod (1+ tog) len))
	  (values t (aref chars tog)))
      (values nil (aref chars tog)))))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys
		 )
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun arrange-list (l arr)
  (mapcan
   (lambda (x) 
       (mapcar
	(lambda (y) (cons x y)) arr))
   l))

(defun arrange (l n)
  (if (= n 0)
      (list '())
      (arrange-list l (arrange l (1- n)))))

(defun combinaison (l n) ; au moins n elements dans l
  (cond
    ((= n 0)
     (list '()))
    ((null l)
     '())
    ((= n 1)
     (mapcar #'list l))
    (t
     (nconc
      (mapcar (lambda (x) (cons (car l) x)) (combinaison (cdr l) (1- n)))
      (combinaison (cdr l) n)))))

(defun distribute (e setofsets)
  (mapcar (lambda (x) (cons e x)) setofsets))

(defun distribute-set (set setofsets)
  (mapcan (lambda (x) (distribute x setofsets)) set))

(defun cartesian-product (args)
  (if (null args)
      (list '())
      (distribute-set (car args) (cartesian-product (cdr args)))))

(defun powerset (s)
  (if (null s)
      (list '())
      (let ((p (powerset (cdr s))))
	(nconc p (distribute (car s) p)))))

(defun not-null-powerset (s)
  (delete nil (powerset s)))

(defun flatten (l)
  (cond
    ((endp l)
     '())
    ((listp (car l)) (nconc (flatten (car l)) (flatten (cdr l))))
    (t
     (cons (car l) (flatten (cdr l))))))

(defun nflatten (l)
  (cond
    ((endp l)
     l)
    ((listp (car l))
     (nconc (nflatten (car l)) (nflatten (cdr l))))
    (t
     (progn
       (setf (cdr l) (nflatten (cdr l)))
       l))))

(defun nmapcar (fn l)
  (do ((ll l (cdr ll)))
      ((endp ll) l)
    (setf (car ll) (funcall fn (car ll)))))

(defun member-sort (e l &key (key #'identity))
  ;; member dans une liste triee suivant key numerique
  (do ((l l (cdr l)))
      ((or (endp l) (> (funcall key (car l)) (funcall key e))))
    (when (= (funcall key e) (funcall key (car l)))
      (return-from member-sort l))))

(defun adjoin-sort (e l &key (key #'identity))
   (do ((l l (cdr l))
	(nl '()))
      ((or (endp l) (>= (funcall key (car l)) (funcall key e)))
       (nconc (nreverse nl)
	      (when (or (endp l) (> (funcall key (car l)) (funcall key e)))
		(list e))
	      l))
     (push (car l) nl)))

(defun union-sort (l1 l2 &key (key #'identity))
;; fusion de deux listes triees
  (do
   ((l1 l1)
    (l2 l2)
    (l nil))
   ((or (endp l1) (endp l2)) (if (endp l1)
				 (nconc (nreverse l) l2)
				 (nconc (nreverse l) l1)))
    (if (<= (funcall key (car l1)) (funcall key (car l2)))
	(progn
	  (push (car l1) l)
	  (when (= (funcall key (car l1)) (funcall key (car l2)))
	    (setf l2 (cdr l2)))
	  (setf l1 (cdr l1)))
	(progn
	  (push (car l2) l)
	  (setf l2 (cdr l2)))
	)))

(defun intersection-sort (l1 l2 &key (key #'identity))
;; intersection de deux listes triees
  (do
   ((l1 l1)
    (l2 l2)
    (l nil))
   ((or (endp l1) (endp l2)) (nreverse l))
    (if (= (funcall key (car l1)) (funcall key (car l2)))
	(progn
	  (push (car l1) l)
	  (pop l1)
	  (pop l2))
	(if (< (funcall key (car l1)) (funcall key (car l2)))
	    (pop l1)
	    (pop l2)))))

(defun sorted-p (l &key (key #'identity))
  (equal l (sort (copy-list l) #'< :key key)))

(defun difference-sort (l1 l2 &key (key #'identity))
  "difference of two sorted lists"
  (do ((l1 l1)
       (l2 l2)
       (l nil))
      ((or (endp l1) (endp l2)) (nconc (nreverse l) l1))
    (cond
      ((= (funcall key (car l1)) (funcall key (car l2)))
       (pop l1) (pop l2))
      ((< (funcall key (car l1)) (funcall key (car l2)))
       (push (car l1) l) (pop l1))
      (t (pop l2)))))

(defun suite (i j &key (build-element #'(lambda (x) x)))
  (do ((k j (1- k))
       (l nil (push (funcall build-element k) l)))
      ((< k i) l)))

(defun iota (n &optional (start 0) (step 1))
  (loop repeat n
	for i from start by step
	collect i))

(defun integer-partition (n)
  (assert (plusp n))
  (if (= n 1)
      '((1))
   (let* ((p (integer-partition (1- n)))
	  (np (mapcar (lambda (l) (cons 1 l)) p)))
     (append np (list (list n))))))

(defun ordered-equal (ol1 ol2)
  (loop
       for l1 = ol1 then (cdr l1)
       for l2 = ol2 then (cdr l2)
       when (or (endp l1) (endp l2))
	 do (return (and (endp l1) (endp l2)))
       unless (eq (car l1) (car l2))
	 do (return-from ordered-equal nil)
       finally (return t)))

(defun permutations (l)
  (if (null (cdr l))
      (list l)
      (loop
	 with len = (1- (length l))
	 for p in (permutations (cdr l))
	 nconc
	   (loop
	      for i from 0 to (length p)
	      collect (nconc
		       (butlast p (- len i))
		       (list (car l))
		       (nthcdr i p))))))

(defun remove-duplicates-except-pairs (l &key (test #'eql))
  (if (null (cdr l))
      l
      (let* ((ll (remove-duplicates-except-pairs (cdr l) :test test))
	     (count (count (car l) ll)))
	(if (> count 1)
	    ll
	    (cons (car l) ll)))))

(defun lexically-ordered-p (l1 l2 &key (test #'=) (comp #'<))
  (or (endp l1)
      (and 
       l2
       (or
	(funcall comp (car l1) (car l2))
	(and (funcall test (car l1) (car l2))
	   (lexically-ordered-p (cdr l1) (cdr l2) :test test :comp comp))))))

(defun lexically-strictly-ordered-p (l1 l2 &key (test #'=) (comp #'<))
  (and
   (not (and (= (length l1) (length l2)) (every test l1 l2)))
   (lexically-ordered-p l1 l2 :test test :comp comp)))

(defun lvbits (m)
  (cartesian-product (make-list m :initial-element '(0 1))))
