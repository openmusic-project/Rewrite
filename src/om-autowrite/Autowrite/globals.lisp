(in-package :nautowrite)

(defvar *debug* nil)

(defvar *output-stream* *standard-output*)

(defmacro avec-temps-to-output-stream (call)
  `(avec-temps ,call *output-stream*))
