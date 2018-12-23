(in-package :nautowrite)
(defun normalizable (term sys &key (extra nil))
    (let* ((signature-term (signature term))
	   (bullet (in-signature-bullet signature-term))
	   (extra-p (in-signature-extra signature-term)))
      (let ((recognized
		 (recognized-p
		  term
		  (seqsys-aut-c sys :extra (or extra extra-p) :bullet bullet))))
	(format *output-stream* "~A " term)
	(unless recognized
	  (format *output-stream* "not "))
	(format
	 *output-stream*
	 "~A-normalizable" (get-approx-name (seqsys-approx sys)))
	(when extra (format *output-stream* "@")))))
  
(defmacro term-check-property (property)
  `(unless (null (term *spec*))
    (format
      *output-stream*
      "term ~A~A.~%"
      (if (,property (term *spec*)) "" "not ")
      (string-downcase (symbol-name ',property)))))

(defun term-needed-redexes (term sys &key (extra nil))
    (when term
    (if (merge-signature (signature sys) (signature term))
	(let ((rp (needed-redex-positions term sys :extra extra)))
	  (if (null rp)
	      (format
	       *output-stream*
	       "no needed-redex")
	      (format
	       *output-stream*
	       "needed redex positions: ~A" (positions-to-string rp))))
	(format t "incompatible signatures"))))

(defun set-current-term (term)
  (setf (previous-term *spec*) nil)
  (setf (term *spec*) term)
  (add-current-term))
