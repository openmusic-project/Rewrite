(in-package :gautowrite)

(defun display-current-approximation (&optional stream)
  (let ((output-stream
	 (or stream
	     (climi::find-pane-named *application-frame* 'approx-pane))))
    (unless stream
      (window-clear output-stream))
      (when (trs *spec*)
	(format output-stream "~A of ~A~%~A"
		(symbol-name (seqsys-approx (trs *spec*)))
		(name (trs *spec*))
		(get-approx-rules (trs *spec*)))
	(unless stream
	  (scroll-extent output-stream 0 0)))))

(defun display-trs (trs &optional (stream nil))
  (let ((output-stream
	 (or stream *trs-pane*)))
      (unless stream
	(window-clear output-stream))
      (when trs
	(format output-stream "CURRENT TRS ~A~%~A" (name trs) (get-rules trs))
	(let ((additional-signature
	       (signature-difference
		(signature trs)
		(signature-from (get-rules trs)))))
	  (unless (signature-empty-p additional-signature)
	    (format output-stream
		    "additional symbol~:[~;s~]: ~A~%"
		    (> (nb-symbols additional-signature) 1) additional-signature))
	  (unless stream
	    (scroll-extent output-stream 0 0))))))

(defun display-termset (termset &optional (stream nil))
  (let ((output-stream
	 (or stream *termset-pane*)))
    (unless stream
      (window-clear output-stream))
    (when termset
      (format output-stream "CURRENT TERMSET ")
      (format output-stream "~A~%" termset))
    (unless stream
      (scroll-extent output-stream 0 0))))

