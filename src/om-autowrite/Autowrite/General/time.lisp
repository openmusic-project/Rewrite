(in-package :general)

(defun time-in-sec (tstart tend)
  (/ (- tend tstart) internal-time-units-per-second))

(defmacro get-time (call)
 `(let ((tstart (get-internal-run-time)))
   ,call
   (time-in-sec tstart (get-internal-run-time))))

(defmacro display-time (call &optional (stream t))
  `(print-time (get-time ,call) ,stream))

(defun print-time (time &optional (stream t))
  (multiple-value-bind (min sec) (floor time 60)
    (if (zerop min)
	(format stream " in ~Asec~%" (coerce sec 'float))
	(format stream " in ~Amin ~Asec~%" min (coerce sec 'float)))))

(defmacro avec-temps (call &optional (stream t))
  `(display-time ,call ,stream))

(defmacro evaluate-time (call &optional (times 1))
  (let ((times times))
    `(coerce
      (/
       (loop
	 repeat ,times
	 sum (get-time ,call))
       ,times)
      'float)))
