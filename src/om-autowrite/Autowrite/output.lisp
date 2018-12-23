(in-package :nautowrite)

(defun write-object-to-file  (object file writer)
  (when (probe-file (absolute-filename file))
    (format *error-output* "~A~%" (absolute-filename file))
    (rename-file (absolute-filename file) (concatenate 'string file ".bak")))
  (with-open-file (foo (absolute-filename file) :direction :output)
    (if foo
	(funcall writer object foo)
	(prog1
	    nil
	  (format *error-output* "unable to open ~A~%" file)))))

