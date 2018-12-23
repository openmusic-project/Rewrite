(in-package :nautowrite)

(defmethod vcylindrification ((s abstract-parity-symbol) i)
  (declare (ignore i))
  (list s))

(defmethod vcylindrification ((s vbits-constant-symbol) i)
  (let* ((vbits (coerce (vbits s) 'list))
	 (len (length vbits))
	 (start (butlast vbits (- len i )))
	 (end (nthcdr i vbits)))
    (list 
     (make-vbits-constant-symbol (name s) (append start (list 0) end))
     (make-vbits-constant-symbol (name s) (append start (list 1) end)))))

(defmethod vcylindrification ((s constant-symbol) i)
  (assert (zerop i))
  (list 
   (make-vbits-constant-symbol (name s) (list 0))
   (make-vbits-constant-symbol (name s) (list 1))))
  
(defmethod vcylindrification ((signed-object signed-object) (i integer))
  (vhomomorphism
   signed-object
   (lambda (s) (vcylindrification s i))))

(defmethod vcylindrification ((signed-object signed-object) (positions list))
  (loop for i in positions
	do (setf signed-object (vcylindrification signed-object i)))
  signed-object)
