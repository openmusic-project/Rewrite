(in-package :nautowrite)

;; replaces symbols

(defvar *modification-function*
  "s -> (s1 ... sk)")

(defmethod vmodification-symbols ((symbols list))
  (mapcar
   (lambda (s)
     (cons s (funcall *modification-function* s)))
   symbols))

(defmethod vmodification ((signed-object signed-object))
  (let ((*signature-mapping*
	 (vmodification-symbols
	  (signature-symbols
	   (signature signed-object)))))
    (apply-signature-mapping signed-object)))
