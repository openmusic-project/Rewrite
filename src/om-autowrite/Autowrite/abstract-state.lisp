(in-package :nautowrite)

(defclass abstract-state () ())

(defmethod root ((state abstract-state)) state)

(defmethod arg ((state abstract-state)) nil)

(defmethod constant ((s abstract-state)) t)

(defmethod name ((s abstract-state))
  (strong-name (format nil "~A" s)))

(defun abstract-state-p (s)
  (typep s 'abstract-state))

(defmethod symbols-from ((term abstract-state))
  '())

(defmethod vars-of ((state abstract-state))
  nil)
