(in-package :nautowrite)

(defun vbits-projection (vbits positions)
  (when positions
    (loop
       with nbits = ()
       for i from 0 below (length vbits)
       when (member i positions :test #'=)
       do (push (aref vbits i) nbits)
       finally
	 (return (make-vbits (nreverse nbits))))))

(defmethod vprojection ((signed-object signed-object) &optional (positions '()))
  (hvmodification
   signed-object
   (lambda (vbits)
     (list
      (vbits-projection vbits positions)))))

(defmethod vprojection-automaton ((a abstract-automaton) &optional (positions '()))
  (vprojection a positions))

