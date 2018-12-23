(in-package :object)

(defgeneric show (o &optional stream)
  (:documentation "print an Autowrite object O defined in Autowrite to the stream STREAM when defined to *output-stream* otherwise"))

(defgeneric compare-object (o1 o2)
  (:documentation "compare the content of two Autowrite objects"))

(defgeneric display-sequence (l stream &key sep)
  (:documentation "print the objects of the sequence L separated by the separator SEP on the STREAM"))

(defgeneric find-object (o os &key test key)
  (:documentation "find an Autowrite object O in a list of objects OS according to the compare function TEST (by default COMPARE-OBJECT"))

(defgeneric size (object)
  (:documentation "returns the size of OBJECT"))

(defgeneric show (o &optional stream))

(defmethod show ((o t) &optional stream)
  (print-object o stream))

(defmethod compare-object ((o1 t) (o2 t))
;;  (format *trace-output* "WARNING (compare-object ~A ~A) !" o1 o2)
  (equalp o1 o2))

(defmethod compare-object ((s1 string) (s2 string))
  (equal s1 s2))

(defmethod compare-object ((l1 sequence) (l2 sequence))
  (or (eq l1 l2)
      (and (compare-object (car l1) (car l2))
	   (compare-object (cdr l1) (cdr l2)))))

(defmethod compare-object ((bv1 bit-vector) (bv2 bit-vector))
  (equal bv1 bv2))

(defmethod show ((l sequence) &optional (stream t))
  (mapc (lambda (x)
	  (show x stream)
	  (format stream " ")) l))

(defmethod display-sequence ((l sequence) stream &key (sep " "))
  (when l
    (mapc (lambda (e)
	    (format stream "~A~A" e sep))
	  (butlast l))
    (format stream "~A" (car (last l)))))

(defmethod display-sequence ((l vector) stream &key (sep " "))
  (display-sequence (coerce l 'list) stream :sep sep))

(defun my-assoc (x l) (assoc x l :test #'compare-object))
(defun my-member (x l) (member x l :test #'compare-object))
(defun my-remove (e set) (remove e set :test #'compare-object))
(defun my-remove-duplicates (l) (remove-duplicates l :test #'compare-object))
; destructive 
(defun my-delete (e set) (delete e set :test #'compare-object))
; destructive
(defun my-delete-duplicates (l) (delete-duplicates l :test #'compare-object))
(defun my-adjoin (e set) (adjoin e set :test #'compare-object))
(defun my-subsetp (s1 s2) (subsetp s1 s2 :test #'compare-object))
(defun compare-set (s1 s2) (and (my-subsetp s1 s2) (my-subsetp s2 s1)))
(defun my-union (l1 l2) (union l1 l2 :test #'compare-object))
(defun my-nunion (l1 l2) (nunion l1 l2 :test #'compare-object))
(defun my-intersection (l1 l2) (intersection l1 l2 :test #'compare-object))
(defun my-setdifference (l1 l2) (set-difference l1 l2 :test #'compare-object))
(defun my-nsetdifference (l1 l2) (nset-difference l1 l2 :test #'compare-object))
(defun mapunion (fn &rest lsts)
  (my-delete-duplicates (mappend fn (car lsts))))

(defmethod find-object ((o t) (os list) &key (test #'compare-object) (key #'identity))
  (car (member o os :test test :key key)))

(defgeneric no-duplicates (l)
  (:documentation
   "return TRUE if L contains no duplicates according COMPARE-OBJECT"))

(defmethod no-duplicates ((l list))
  (= (length l) (length (remove-duplicates l :test #'compare-object))))
