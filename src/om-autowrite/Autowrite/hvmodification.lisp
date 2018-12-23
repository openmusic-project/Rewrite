(in-package :nautowrite)

(defgeneric hvmodification (s h)
  (:documentation "si s^w constante et h(w) = {w1 ...wk} -> (s^w1 ... s^vk) 
  sinon (s)"))

(defmethod hvmodification ((s abstract-parity-symbol) h)
  (list s))

(defmethod hvmodification ((s vbits-constant-symbol) h)
  (let ((lvbits (funcall h (vbits s))))
    (mapcar
     (lambda (vbits)
       (make-vbits-constant-symbol (name s) vbits))
     lvbits)))

(defmethod hvmodification
    ((signed-object signed-object) h)
  (vhomomorphism
   signed-object 
   (lambda (s) (hvmodification s h))))
