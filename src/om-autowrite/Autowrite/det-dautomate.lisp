(in-package :nautowrite)

(defun det-create-transitions (key bc srh prh)
;; creates one rule in this deterministic case
  (add-dtransition-to-transitions key (make-dstate srh bc prh) *global-transitions*))


