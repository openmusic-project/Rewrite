(in-package :nautowrite)

(defun x1-to-nothing-fun ()
  (lambda (vbits)
    (if (zerop (aref vbits 0))
	'()
	(list '()))))

(defmethod x1-to-nothing ((a abstract-automaton))
  (reduce-automaton
   (hvmodification a (x1-to-nothing-fun))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun x1-to-xj-fun (m j)
  (assert (<= 1 j m))
  (lambda (vbits)
    (let ((lvbits (mapcar #'make-vbits (lvbits m))))
      (mapcar
       #'make-vbits
       (remove-if-not
	(lambda (nvbits)
	  (=  (aref vbits 0) (aref nvbits (1- j))))
	lvbits)))))

(defmethod x1-to-xj ((a abstract-automaton) m j)
  (hvmodification a (x1-to-xj-fun m j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-bit (b)
  (if (zerop b) 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xi-to-cxj-fun (m i j)
  (assert (<= 1 i m))
  (assert (<= 1 j m))
  (lambda (vbits)
    (let ((nvbits (make-vbits vbits))
	  (bit (aref vbits (1- j))))
      (setf (aref nvbits (1- i)) (toggle-bit bit))
      (list nvbits))))

(defmethod xi-to-cxj ((a abstract-automaton) m i j)
  (hvmodification a (xi-to-cxj-fun m i j)))

(defun xj-to-cxj-fun (m j)
  (assert (<= 1 j m))
  (lambda (vbits)
    (let ((nvbits (make-vbits vbits))
	  (bit (aref vbits (1- j))))
      (setf (aref nvbits (1- j)) (toggle-bit bit))
      (list nvbits))))

(defmethod xj-to-cxj ((a abstract-automaton) m j)
  (xi-to-cxj a m j j))

(defmethod x1-to-cx1 ((a abstract-automaton))
  (xi-to-cxj a 1 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun union-p (bits)
  (some #'onep bits))

(defun intersection-p (bits)
  (every #'onep bits))

(defun union-bin-p (b1 b2)
  (union-p (list b1 b2)))

(defun intersection-bin-p (b1 b2)
  (intersection-p (list b1 b2)))

(defun not-bunion-p (b bits)
  (or
   (and (= 1 b) (every #'zerop bits))
   (and (zerop b) (some #'onep bits))))

(defun bunion-p (b bits)
  (or
   (and (zerop b) (every #'zerop bits))
   (and (onep b) (some #'onep bits))))

(defun bintersection-p (b bits)
  (or
   (and (zerop b) (some #'zerop bits))
   (and (= 1 b) (every #'onep bits))))

(defun not-bintersection-p (b bits)
  (or
   (and (onep b) (some #'zerop bits))
   (and (zerop b) (every #'onep bits))))

(defun not-bunion-bin-p (b b1 b2)
  (not-bunion-p b (list b1 b2)))

(defun bunion-bin-p (b b1 b2)
  (bunion-p b (list b1 b2)))

(defun bintersection-bin-p (b b1 b2)
  (bintersection-p b (list b1 b2)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun x1-to-bool-xi-fun (m bool)
  (lambda (vbits)
    (let ((lvbits (mapcar #'make-vbits (lvbits m))))
      (mapcar
       #'make-vbits
       (remove-if-not
	(lambda (nvbits)
	  (let ((b (aref vbits 0)))
	    (case bool
	      (union (bunion-p b nvbits))
	      (intersection (bintersection-p b nvbits)))))
	lvbits)))))

(defmethod x1-to-bool-xi ((a abstract-automaton) m bool)
  (hvmodification a (x1-to-bool-xi-fun m bool)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod x1-to-color-f ((s abstract-parity-symbol) k color)
  (list s))

(defmethod x1-to-color-f ((s vbits-constant-symbol) k color)
  (let ((vbit (aref (vbits s) 0))
	(colors (iota k 1))
	(name (name s)))
    (if (zerop vbit)
	(mapcar
	 (lambda (c)
	   (make-color-constant-symbol name c))
	 (remove color colors))
	(list (make-color-constant-symbol name color)))))

(defun x1-to-color-h (k color)
  (lambda (s)
    (x1-to-color-f s k color)))
 
(defmethod x1-to-color ((signed-object signed-object) k color)
  (vhomomorphism signed-object (x1-to-color-h k color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-x1-f ((s (eql *empty-symbol*)) (csign signature))
  (mapcar (lambda (cs)
	    (make-vbits-constant-symbol (name cs) (list 0)))
	  (signature-symbols csign)))

(defmethod nothing-to-x1-f ((s constant-symbol) (csign signature))
  (list (make-vbits-constant-symbol (name s) (list 1))))

(defmethod nothing-to-x1-f ((s abstract-parity-symbol) (csign signature))
  (list s))

(defmethod nothing-to-x1-h ((csign signature))
  (lambda (s)
    (nothing-to-x1-f s csign)))

(defmethod nothing-to-x1 ((signed-object signed-object))
  (vhomomorphism signed-object (nothing-to-x1-h (constant-signature signed-object))))

;; il faut que la fonction de transition d'un fly-automaton
;; sache gerer *empty-symbol*
(defmethod nothing-to-x1 ((a abstract-automaton))
  (vhomomorphism (add-empty-symbol a) (nothing-to-x1-h (constant-signature a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-random-f ((s (eql *empty-symbol*)) m)
  (list (make-zero-vbits-constant-symbol (name s) m)))

(defmethod nothing-to-random-f ((s constant-symbol) m)
  (list (make-random-vbits-constant-symbol (name s) m)))

(defmethod nothing-to-random-f ((s abstract-parity-symbol) m)
  (list s))

(defmethod nothing-to-random-h (m)
  (lambda (s)
    (nothing-to-random-f s m)))

(defmethod nothing-to-random ((signed-object signed-object) m)
  (vhomomorphism
   signed-object
   (nothing-to-random-h m)))

(defmethod nothing-to-random ((a abstract-automaton) m)
  (vhomomorphism
   (add-empty-symbol a)
   (nothing-to-random-h m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-random-color-f ((s (eql *empty-symbol*)) k)
  '())

(defmethod nothing-to-random-color-f ((s constant-symbol) k)
  (list (make-random-color-constant-symbol (name s) k)))

(defmethod nothing-to-random-color-f ((s abstract-parity-symbol) k)
  (list s))

(defmethod nothing-to-random-color-h (k)
  (lambda (s)
    (nothing-to-random-color-f s k)))

(defmethod nothing-to-random-color ((signed-object signed-object) k)
  (vhomomorphism
   signed-object
   (nothing-to-random-color-h k)))

(defmethod nothing-to-random-color ((a abstract-automaton) k)
  (vhomomorphism
   (add-empty-symbol a)
   (nothing-to-random-color-h k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-random-partition-f ((s (eql *empty-symbol*)) m)
  (list (make-zero-vbits-constant-symbol (name s) m)))

(defmethod nothing-to-random-partition-f ((s constant-symbol) m)
  (list (make-random-partition-vbits-constant-symbol (name s) m)))

(defmethod nothing-to-random-partition-f ((s abstract-parity-symbol) m)
  (list s))

(defmethod nothing-to-random-partition-h (m)
  (lambda (s)
    (nothing-to-random-partition-f s m)))

(defmethod nothing-to-random-partition ((signed-object signed-object) m)
  (vhomomorphism
   signed-object
   (nothing-to-random-partition-h m)))

(defmethod nothing-to-random-partition ((a abstract-automaton) m)
  (vhomomorphism
   (add-empty-symbol a)
   (nothing-to-random-partition-h m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-color-f ((s (eql *empty-symbol*)) (csign signature) k color)
  (mapcan
   (lambda (c)
     (mapcar (lambda (cs)
	       (make-color-constant-symbol (name cs) c))
	     (signature-symbols csign)))
   (remove color (iota k 1))))

(defmethod nothing-to-color-f ((s constant-symbol) (csign signature) k color)
  (list (make-color-constant-symbol (name s) color)))

(defmethod nothing-to-color-f ((s abstract-parity-symbol) (csign signature) k color)
  (list s))

(defmethod nothing-to-color-h ((csign signature) k color)
  (lambda (s)
    (nothing-to-color-f s csign k color)))

(defmethod nothing-to-color ((signed-object signed-object) k color)
  (vhomomorphism
   signed-object
   (nothing-to-color-h (constant-signature signed-object) k color)))

(defmethod nothing-to-color ((a abstract-automaton) k color)
  (vhomomorphism
   (add-empty-symbol a)
   (nothing-to-color-h (constant-signature a) k color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-xj-f ((s (eql *empty-symbol*)) m j (csign signature))
  (let ((lvbits (remove-if
		 (lambda (vbits)
		   (onep (aref vbits (1- j))))
		 (mapcar #'make-vbits (lvbits m))))
	(csyms (signature-symbols csign)))
    (mapcan
     (lambda (cs)
       (mapcar
	(lambda (vbits)
	  (make-vbits-constant-symbol (name cs) vbits))
	lvbits))
     csyms)))

(defmethod nothing-to-xj-f ((s constant-symbol) m j (csign signature))
  (let ((lvbits (remove-if
		 (lambda (vbits)
		   (zerop (aref vbits (1- j))))
		 (mapcar #'make-vbits (lvbits m)))))
    (mapcar
     (lambda (vbits)
       (make-vbits-constant-symbol (name s) vbits))
     lvbits)))

(defmethod nothing-to-xj-f ((s abstract-parity-symbol) m j (csign signature))
  (list s))

(defmethod nothing-to-xj-h (m j (csign signature))
  (lambda (s)
    (nothing-to-xj-f s m j csign)))

(defmethod nothing-to-xj (m j (signed-object signed-object))
  (vhomomorphism
   signed-object
   (nothing-to-xj-h m j (constant-signature signed-object))))

(defmethod nothing-to-xj ((a abstract-automaton) m j)
  (vhomomorphism
   (add-empty-symbol a)
   (nothing-to-xj-h m j (constant-signature a))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-xj1-bool-xj2-f ((s (eql *empty-symbol*)) m j1 j2 (csign signature) bool)
  (let ((lvbits
	 (remove-if
	  (lambda (vbits)
	    (let ((b1 (aref vbits (1- j1)))
		  (b2 (aref vbits (1- j2))))
	      (case bool
		(union (union-bin-p b1 b2))
		(intersection (intersection-bin-p b1 b2)))))
	  (mapcar #'make-vbits (lvbits m))))
	(csyms (signature-symbols csign)))
    (mapcan
     (lambda (cs)
       (mapcar (lambda (vbits) 
		 (make-vbits-constant-symbol (name cs) vbits))
	       lvbits))
     csyms)))

(defmethod nothing-to-xj1-bool-xj2-f
    ((s constant-symbol) m j1 j2 (csign signature) bool)
  (let ((lvbits
	 (remove-if-not
	  (lambda (vbits)
	    (let ((b1 (aref vbits (1- j1)))
		  (b2 (aref vbits (1- j2))))
	      (case bool
		(union (union-bin-p b1 b2))
		(intersection (intersection-bin-p b1 b2)))))
	  (mapcar #'make-vbits (lvbits m)))))
    (mapcar
     (lambda (vbits)
       (make-vbits-constant-symbol (name s) vbits))
     lvbits)))

(defmethod nothing-to-xj1-bool-xj2-f ((s abstract-parity-symbol) m j1 j2 (csign signature) bool)
  (list s))

(defmethod nothing-to-xj1-bool-xj2-h (m j1 j2 (csign signature) bool)
  (lambda (s)
    (nothing-to-xj1-bool-xj2-f s m j1 j2 csign bool)))

(defmethod nothing-to-xj1-bool-xj2 (m j1 j2 (signed-object signed-object) bool)
  (vhomomorphism
   signed-object
   (nothing-to-xj1-bool-xj2-h
    m j1 j2
    (constant-signature signed-object) bool)))

(defmethod nothing-to-xj1-bool-xj2 ((a abstract-automaton) m j1 j2 bool)
  (vhomomorphism
   (add-empty-symbol a)
   (nothing-to-xj1-bool-xj2-h m j1 j2 (constant-signature a) bool)))

(defmethod nothing-to-xj1-union-xj2 ((signed-object signed-object) m j1 j2)
  (nothing-to-xj1-bool-xj2 signed-object m j1 j2 'union))

(defmethod nothing-to-xj1-intersection-xj2 ((signed-object signed-object) m j1 j2)
  (nothing-to-xj1-bool-xj2 signed-object m j1 j2 'intersection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun x1-to-xj1-bool-xj2-f (m j1 j2 bool)
  (lambda (vbits)
    (let ((lvbits (mapcar #'make-vbits (lvbits m))))
      (mapcar
       #'make-vbits
       (remove-if-not
	(lambda (nvbits)
	  (let ((b (aref vbits 0))
		(b1 (aref nvbits (1- j1)))
		(b2 (aref nvbits (1- j2))))
	    (if (eq 'union bool)
		(bunion-bin-p b b1 b2)
		(bintersection-bin-p b b1 b2))))
	lvbits)))))

(defmethod x1-to-xj1-bool-xj2
    ((a abstract-automaton) m j1 j2 bool)
  (hvmodification a (x1-to-xj1-bool-xj2-f m j1 j2 bool)))

(defmethod x1-to-xj1-union-xj2 ((a abstract-automaton) m j1 j2)
  (x1-to-xj1-bool-xj2 a m j1 j2 'union))

(defmethod x1-to-xj1-intersection-xj2 ((a abstract-automaton) m j1 j2)
  (x1-to-xj1-bool-xj2 a m j1 j2 'intersection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun y1-y2-to-xj1-xj2-f (m j1 j2)
  (assert (<= 1 j1 m))
  (assert (<= 1 j2 m))
  (lambda (vbits)
    (let ((lvbits (mapcar #'make-vbits (lvbits m))))
      (remove-if-not
       (lambda (nvbits)
	 (let ((b1 (aref nvbits (1- j1)))
	       (b2 (aref nvbits (1- j2))))
	   (and
	    (=  (aref vbits 0) b1)
	    (=  (aref vbits 1) b2))))
       lvbits))))

(defmethod y1-y2-to-xj1-xj2
    ((a abstract-automaton) m j1 j2)
  (hvmodification a (y1-y2-to-xj1-xj2-f m j1 j2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun y1-y2-to-xj-xj1-bool-xj2-f (m j j1 j2 bool)
  (assert (<= 1 j m))
  (assert (<= 1 j1 m))
  (assert (<= 1 j2 m))
  (lambda (vbits)
    (let ((lvbits (mapcar #'make-vbits (lvbits m))))
      (remove-if-not
       (lambda (nvbits)
	 (let ((b (aref vbits 1))
	       (b1 (aref nvbits (1- j1)))
	       (b2 (aref nvbits (1- j2))))
	 (and
	  (=  (aref vbits 0) (aref nvbits (1- j)))
	  (case bool
	    (union  (bunion-bin-p b b1 b2))
	    (intersection (bintersection-bin-p b b1 b2))))))
       lvbits))))

(defmethod y1-y2-to-xj-xj1-bool-xj2
    ((a abstract-automaton) m j j1 j2 bool)
  (hvmodification a (y1-y2-to-xj-xj1-bool-xj2-f m j j1 j2 bool)))

(defmethod y1-y2-to-xj-xj1-union-xj2 ((a abstract-automaton) m j j1 j2)
  (y1-y2-to-xj-xj1-bool-xj2 a m j j1 j2 'union))

(defmethod y1-y2-to-xj-xj1-intersection-xj2 ((a abstract-automaton) m j j1 j2)
  (y1-y2-to-xj-xj1-bool-xj2 a m j j1 j2 'intersection))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A FINIR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod nothing-to-c1-union-c2-f ((s (eql *empty-symbol*)) (csign signature) k c1 c2)
  (let ((colors (remove c1 (remove c2 (iota k 1))))
	(csyms (signature-symbols csign)))
    (mapcan
     (lambda (cs)
       (mapcar (lambda (color)
		 (make-color-constant-symbol (name cs) color))
	       colors))
     csyms)))

(defmethod nothing-to-c1-union-c2-f ((s constant-symbol) (csign signature) k c1 c2)
  (let ((colors (list c1 c2))
	(name (name s)))
    (mapcar
     (lambda (color)
       (make-color-constant-symbol name color))
     colors)))

(defmethod nothing-to-c1-union-c2-f ((s abstract-parity-symbol) (csign signature) k c1 c2)
  (list s))

(defmethod nothing-to-c1-union-c2-h ((csign signature) k c1 c2)
  (lambda (s)
    (nothing-to-c1-union-c2-f s csign k c1 c2)))

(defmethod nothing-to-c1-union-c2 ((signed-object signed-object) k c1 c2)
  (vhomomorphism
   signed-object
   (nothing-to-c1-union-c2-h (constant-signature signed-object) k c1 c2)))

(defmethod nothing-to-c1-union-c2 ((a abstract-automaton) k c1 c2)
  (vhomomorphism
   (add-empty-symbol a)
   (nothing-to-c1-union-c2-h (constant-signature a) k c1 c2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
