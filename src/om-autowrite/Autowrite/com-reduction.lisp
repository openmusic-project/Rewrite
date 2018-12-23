(in-package :nautowrite)

(defun reduction-step (rp &key (tr t))
  (when tr
    (format *output-stream*
	    "contracting redex at position ~A~%"
	    (position-to-string rp)))
  (let ((new-term (reduction
		   (term *spec*)
		   rp (get-rules
		       (trs *spec*)))))
    (setf (previous-term *spec*) (term *spec*))
    (setf (term *spec*) new-term)))

(defun parallel-reduction-step (positions &key (tr t))
  (when tr
    (format *output-stream*
	    "contracting redex(es) at position(s) ~A~%" (positions-to-string positions)))
  (let ((initial-term (term *spec*)))
    (dolist (r positions (term *spec*))
      (reduction-step r :tr nil))
    (setf (previous-term *spec*) initial-term)))



