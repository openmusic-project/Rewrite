(in-package :nautowrite)

(defclass kwd (named-object) ())

(defun make-keyword (name)
  (make-instance 'kwd :name name))
  
(defmethod print-object ((object kwd) stream)
  (format stream "kwd: ~a" (name object)))

(defclass sym (named-object) ())

(defclass vbits () ((vbits :initarg :vbits :reader vbits)))

(defmethod print-object ((object vbits) stream)
  (format stream "vbits: ~A~%" (vbits object)))

(defclass separator (named-object) ())

(defparameter +sep-open-par+ (make-instance 'separator :name "("))
(defparameter +sep-closed-par+ (make-instance 'separator :name ")"))
(defparameter +sep-colon+ (make-instance 'separator :name ":"))
(defparameter +sep-arrow+ (make-instance 'separator :name "->"))
(defparameter +sep-comma+ (make-instance 'separator :name ","))
(defparameter +sep-open-bracket+ (make-instance 'separator :name "["))
(defparameter +sep-closed-bracket+ (make-instance 'separator :name "]"))
(defparameter +sep-open-sbracket+ (make-instance 'separator :name "<"))
(defparameter +sep-closed-sbracket+ (make-instance 'separator :name ">"))
(defparameter +sep-hat+ (make-instance 'separator :name "^"))

(defvar *keywords* nil)
(setf *keywords*
  '("Eof" "Ops" "Vars" "TRS" "Termset" "Term"
    "Automaton" "States" "Final"
    "Description" "Transitions"
    "Prior" "Tautomaton" "Automata"))

(defun lex (stream)
  (let ((buffer (make-array 1 :adjustable t :element-type 'character :fill-pointer 0)))
    ;; skip until next non blank or return nil if EOF
    (loop for ch = (read-char stream nil nil) then (read-char stream nil nil)
;;	  do (print ch)
       when (null ch) do (return-from lex (make-keyword "Eof"))
       until (not (member ch '(#\Space #\Tab #\Newline)))
       finally
       (case ch
	 (#\( (return-from lex +sep-open-par+))
	 (#\) (return-from lex +sep-closed-par+))
	 (#\< (return-from lex +sep-open-sbracket+))
	 (#\> (return-from lex +sep-closed-sbracket+)) 
	 (#\[ (return-from lex +sep-open-bracket+))
	 (#\] (return-from lex +sep-closed-bracket+))
	 (#\: (return-from lex +sep-colon+))
	 (#\, (return-from lex +sep-comma+))
	 (#\^ (loop
		 with vbits = ()
		 for next = (read-char stream nil nil) then (read-char stream nil nil)
		 until (not (member next '(#\0 #\1)))
		 do (push next vbits)
		 finally (progn (unread-char next stream)
				(return-from lex (make-instance 'vbits :vbits (nreverse vbits))))))
	 (#\; (loop for ch = (read-char stream nil nil)
		 then (read-char stream nil nil)
		 when (null ch) do (return-from lex (make-keyword "Eof"))
		 until (eql ch #\Newline)
		 finally (return-from lex (lex stream))))
	 (#\- (let ((next (read-char stream nil nil)))
		(if (eql next #\>)
		    (return-from lex +sep-arrow+)
		    (progn (vector-push-extend ch buffer)
			   (vector-push-extend next buffer)
			   (return-from nil)))))
		    
		;; (if (eql next #\>)
		;;     (return-from lex +sep-arrow+)
		    ;; (progn (unread-char next stream)
		    ;; 	   (return-from lex (make-instance 'sym :name "-"))))))
	 (#\" (loop for ch = (read-char stream nil nil)
		 then (read-char stream nil nil)
		 when (null ch) do (return-from lex  (make-keyword "Eof"))
		 until (eql ch #\")
		 do (vector-push-extend ch buffer)
		 finally (return-from lex buffer))
	      ;;			       finally (return-from lex (make-instance 'sym :name buffer))
	      )
	 (otherwise (vector-push-extend ch buffer))))
    ;; read chars until a separator is reached
    (loop for ch = (read-char stream nil nil) then (read-char stream nil nil)
       until (member ch '(nil #\Space #\Tab #\Newline #\( #\) #\: #\, #\^ #\; #\[ #\] #\< #\>))
					;	  when (null ch) do (return-from lex nil)
       do (vector-push-extend ch buffer)
       finally (progn
		 (unless (null ch) (unread-char ch stream))
		 (return-from lex (cond ((member buffer *keywords*
						 :test #'string=)
					 (make-keyword buffer))
					((every #'digit-char-p buffer)
					 (make-instance 'sym :name (parse-integer buffer)))
					((and (eql (aref buffer 0) #\-)
					      (every #'digit-char-p (subseq buffer 1)))
					 (make-instance 'sym :name (parse-integer buffer)))
					(t
					 (make-instance 'sym :name buffer))))))))
					

(defmethod print-object ((object sym) stream)
  (format stream "sym: ~a" (name object)))

(defmethod print-object ((object separator) stream)
  (format stream "separator: ~a" (name object)))

(defun lex-file (filename)
  (with-open-file (stream filename :direction :input)
    (loop while (print (lex stream)))))

(defun lex-stream (stream)
  (loop while (print (lex stream))))
