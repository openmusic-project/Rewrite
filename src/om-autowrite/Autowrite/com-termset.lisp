(in-package :nautowrite)

(defun list-termsets ()
  (list-keys (table (termsets *spec*))))

(defun accessibility ()
  (when (and (trs *spec*) (termset *spec*))
    (let ((signature-term (signature-from (term *spec*))))
      (if (and (termset-merge-signature (termset *spec*) signature-term)
	       (seqsys-change-aut-t (trs *spec*) (aut-termset (termset *spec*))))
	  (progn
	    (format *standard-output* "termset ~A " (name (termset *spec*)))
	    (if (recognized-p (term *spec*) (seqsys-aut-c-t (trs *spec*)) )
		(prog1 t
		  (format *output-stream* "accessible from term ~A" (term *spec*)))
		(format *output-stream*
			"not accessible from term ~A" (term *spec*))))
	  (format *output-stream* "incompatible signatures~%")))))

(defun set-current-termset (termset)
  (setf (termset *spec*) termset)
  (add-current-termset))

(defun get-termset (name)
  (gethash name (table (termsets *spec*))))

(defun build-termset (name termset)
  (make-termset name termset))
