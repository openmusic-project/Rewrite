; Author : Pierre Donat-Bouillud

(in-package :rw)

;;Class : strategy
;  "A strategy to decide where to put a note on the grid.
;Can use information about past and future"
(defclass strategy () ())

(defgeneric note-begin? (strategy subdi-duration onset duration)
  (:documentation "Decides whether there is a note or a silence in the subdivision which interlaps with
the beginning of the note")
)

(defgeneric note-end? (strategy subdi-duration onset duration)
(:documentation "Decides whether there is a note or a silence in the subdivision which interlaps with the
end of the note")
)

(defgeneric put-note (strategy grid subdi-duration onset duration)
(:documentation "Put a note on the grid according to the strategy")
)

;;Class : amount of note strategy
;"Given an amount ( in [0,1] ), says that it is a note if there is more than amount * duration of note in the
;subdivision"
(defclass amount-strategy (strategy)
  ((amount :initarg :amount :initform 0.5 :accessor amount :documentation "Amount to decide of the existence of a note"))
  )

(defmethod note-begin? ((strategy amount-strategy) subdi-duration onset duration)
  (multiple-value-bind (start-subdi note-subdi) (floor onset subdi-duration)

   (values  (<= note-subdi (* (amount strategy)  subdi-duration))  start-subdi ))); returns also the case in the grid which has been analysed

(defmethod note-end?  ((strategy amount-strategy) subdi-duration onset duration)
  (let* ( (end-note (+ onset duration)))
    (multiple-value-bind (end-subdi note-subdi) (floor end-note subdi-duration)

      (values (>= note-subdi (* (amount strategy) subdi-duration)) end-subdi ))));  returns also the case in the grid which has been analysed

(defmethod put-note ( (strategy strategy) grid subdi-duration onset duration)
  (multiple-value-bind (begin start-subdi) (note-begin? strategy subdi-duration onset duration)
    (multiple-value-bind (end end-subdi) (note-end? strategy subdi-duration onset duration)
       ;Is there a note to put ?
       (unless (and (not begin) (not end) (> start-subdi end-subdi))
         (let ( (begin-subdi (if begin start-subdi (1+ start-subdi)))
                (termin-subdi (if end end-subdi (1- end-subdi))))
           ;just put a note :n followed by an adequate number of slurs
           (setf (elt grid begin-subdi) :n)
           (loop for i from (1+ begin-subdi) to termin-subdi do
                 (setf (elt grid i) :s)
                 )
           )))))


;;Class : silence strategy
;"A strategy that removes parts of note that do not overlap an entire subdivision"
(defclass silence-strategy (amount-strategy)
((amount :initform 1))
)

;;Class : note strategy
;"A strategy that puts a note on the whole subdivision if the note intersects the subdivision"
(defclass note-strategy (amount-strategy)
((amount :initform 0.01))
)

;;Class : closest strategy
;"A strategy that puts a note on the closest point of the grid"
(defclass closest-strategy (amount-strategy)
((amount :initform 0.5))
)



(defun basic-subdivision (onsets durations &key (subdivisions '(4 2 2)) 
                                         (tempo 60) (strategy (make-instance 'closest-strategy)))
"Transforms a list of onsets and durations into a tree by brutally
subdividing.
Depth : starts at 0. With subdivision = 4, it gives 16 subdivisions per measures. That's enough (maybe 16 * 4 ?).
3 strategys to decide wheter to put a note or a silence if there is both sound or silence 
for a same subdivision :
- put the note
- put the silence
- put what takes the more time in the measure between the two"

; builds the grid so that a case of the grid is a leaf of the tree of <depth> and initializez it with :r
(let (( grid-quant (build-grid onsets durations :subdivisions subdivisions :tempo tempo :strategy strategy)))
  
  (labels ( (build-measures (grid subdi)
              "Builds the complete subtree from the grid"
              (if (null subdi)
                  grid
                (let ((step (reduce #'* subdi))
                      (start 0)
                      (end (1- (length grid))))
                  (loop for i from start to end by step
                        collect  (build-measures (subseq grid i (+ i step)) (rest subdi)  )))
                )
              ))              
  (build-measures (coerce grid-quant 'list) subdivisions ) 
)
)
)

(defun build-grid  (onsets durations &key (subdivisions '(4 2 2)) 
                                         (tempo 60) (strategy (make-instance 'closest-strategy)))
"Builds the grid which will be used to build an inetrnal tree"

;create a grid to put the notes on
(let* ((total-duration (+ (first (last onsets)) (first (last durations))))
       (beat-duration (/ 60000 tempo)) ; 1 min = 60000 milliseconds
       (nb-measures (/ total-duration (* beat-duration (first subdivisions))))
       (nb-subdi (reduce #'* subdivisions)); nb of subdivisions at max depth per measure
       (subdi-duration (/  beat-duration (/ nb-subdi (first subdivisions)) ));duration of a subdivision at max depth
       (grid-length  (* (ceiling nb-measures)  nb-subdi)); we complete the last measure
       (grid (make-array grid-length :initial-element :r))
       )
 ; (print total-duration)
 ; (print nb-measures)
 ; (print subdi-duration)
 ; (print grid-length)
  ;Process onsets and durations
  (mapc #'(lambda (onset duration) (put-note strategy grid subdi-duration onset duration)) onsets durations)
 (values grid subdi-duration)
  ))



;functions to debug quantification and subdivision

(defun explore-tree (explorator tree &optional (find-action #'(lambda(element) element)) )
  "Explore a tree. Do what you want where true"
  (reduce #'(lambda (a b ) (or a b))  (mapcar #'(lambda ( element) (typecase element
                             (list (explore-tree explorator element find-action))
                             (t (when (funcall explorator element)
                                  (funcall find-action element)))
                             )
                      )
                  tree
                  ) :initial-value t)
  )


(defun time-grid (grid subdi-duration)
  "Returns the lists of onsets and durations from the grid of :r :n and :s "
  (let (onsets durations (begin-note -1) 
               (length-grid (length grid)))
      (flet ((add-note (num)
             (push (* begin-note subdi-duration)  onsets)
              (push (* (- num begin-note) subdi-duration) durations)))
        (dotimes (i (length grid))
          (case (elt grid i)
            (:r (when (not (equalp begin-note -1)) 
                  (add-note (1- i))
                  (setf begin-note -1)))
            (:n (when (not (equalp begin-note -1))
                    (add-note (1- i)))
             (setf begin-note i))))
        ;Taking care of the case (should be always) a note is a the end of the grid
        (when (or (equalp (elt grid (1- length-grid)) :s) (equalp (elt grid (1- length-grid)) :n))
            (add-note (1- length-grid)))
      (setf onsets (nreverse onsets))
      (setf durations (nreverse durations))
      (values onsets durations))))
                    


