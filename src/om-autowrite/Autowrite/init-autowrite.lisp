(in-package :nautowrite)

(defun init-autowrite ()
  (setf *data-directory* (initial-data-directory))
  (setf *specs*  (make-specs))
  (setf *spec*
	(set-current-spec (make-spec "empty" (make-signature nil) nil)))
  (setf *state-internal-number* 0)
  (init-names)
  (init-symbols)
  (reset-aux-variables)
  (init-variables)
  (init-parser))
