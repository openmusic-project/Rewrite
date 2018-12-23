; Author : Pierre Donat-Bouillud

(use-package :rw :om)

(in-package :rw)


; Functions definition

(om::defmethod! quantify ((onsets list) (durations list) (subdivisions list) (tempo number) strategy)
                :initvals '((0) (1024) (4 2 2) 60 'closest-strategy)
                :indoc '("onsets" "durations" "subdivisions" "tempo" "strategy") 
                :icon 111  ; the icon
                :numouts 2
                :doc "Quantify <onsets> and <durations>"
                :menuins '((5 (("Silence strategy" 'silence-strategy)
                               ("Note strategy" 'note-strategy)
                               ("Closest strategy" 'closest-strategy)
                               )))
                ;basic quantification
 (let* ((tree (basic-subdivision onsets  (normalize-durations durations) 
                                :subdivisions subdivisions :tempo tempo 
                                :strategy (cond 
                                           ((equal strategy 'amount) (make-instance 'amount-strategy :amount (amount-strategy-choose)))
                                           ((symbolp strategy) (make-instance strategy))
                                           (t nil))
                                ))
       (sigs (make-list (length tree) :initial-element (list (first subdivisions) 4)) );list of all time signatures
       ) 
   ;Basic simplification : juste merge rests, or slurs when they are only them in the same subdivision
   ;'(move-up gen-= move-up merge-ns move-up  merge-rs move-up)
  ; (let ((merged-tree  (apply-transforms tree '(merge-rs merge-ns move-up cut-sides correct-slurs merge-rs resubdivize gen-=) :measure t))) ;'(identity)



; to bypass the rewrite rules :
; comment these two lines
   (let ((merged-tree  (apply-transforms tree '(merge-rs merge-ns ) :measure t)))
     (values (to-omtree merged-tree sigs) merged-tree))
;and uncomment this line
; (values (to-omtree tree sigs ) tree )

))

(defun normalize-durations (durations &optional (buffer nil))
  "if durations is the output of a chord-seq, it is a list of lists of length 1, otherwise, the input is a normal list
This function transforms the durations output of a chord-seq into a normal list
The chord-seq has to be monophonic"

  (if (null durations)
      buffer
    (let ((current (car durations)))
      (if (listp current)
          (normalize-durations (rest durations) (append buffer current))
        (normalize-durations (rest durations) (append buffer (list current)))))))


;TRANSFORMATIONS NOT WORKING : gen-=

(om::defmethod! to-omtree (tree &optional (sigs '(4 4) sigs-supplied-p))
;(om::defmethod! to-omtree (tree &optional (measures '((4 4)) measures-supplied-p))
                :doc "Converts the internal representation into an Om rhythm tree"
                :indoc '("a internal tree")
                :icon 111
  (let ((last-symbol) (result))
  ;Transformer chaque mesure
  ;Supprimer la durée en début de liste
  ;Choisir un chifrage de la mesure cohérent
  ;On rajoute un ? au début
    (if sigs-supplied-p 

        (list :? (mapcar #'(lambda (measure time-signature)
                             (multiple-value-bind (mes last) (tree-to-om measure last-symbol)
                               (setf last-symbol last) 
                               (list time-signature (cadr mes))))
                         tree
                         sigs))
      (list :? (mapcar #'(lambda (measure) 
                           (multiple-value-bind (mes last) (tree-to-om measure last-symbol) 
                             (setf last-symbol last) 
                             ;(list (time-sig measure) (cadr mes))))
                             (list sigs (cadr mes))))
                       tree)))   
      
      ))


;le comptage du nombre de temps dans la mesure n'est pas très efficace...
;TODO : heuristique pour détecter le nombre de temps dans la mesure comme décrit ci-dessous
(defun time-sig (measure &key (denom 4) (max-beats 7) (guess nil) )
  "Find a time signature for the measure. 
- denom : denominator of the time signature Default : unit of the measure is 4.
   Should always be a power of 2.
- max-beats : if the number of subdivisions of the measure strictly exceeds max-beats, the time signature will be divided
If guess is true, tries to detect the denominator of the time signature (4 or 8) by counting if there are mostly ternary or binary subdivisions
at beat level. Of course, it is just caricatural (for instance 3/8 won't be detected). It is rather (but not really) 
 a detectioon of compound or simple time signature."
  (if guess 
      (let ((binary-subdi 0) (ternary-subdi 0)); nb of binary and ternary divisions
        (loop for subdi in measure do
              (when (listp subdi)
                (cond ( (zerop (mod (length subdi) 2 )) (incf binary-subdi))
                      ( (zerop (mod (length subdi) 3 )) (incf ternary-subdi)))))
         (if (>= binary-subdi ternary-subdi) (list (* 3  (length measure)) 4)  (list length measure 8)))
    (list (length measure) denom)
    )
  )


(defun tree-to-om 
       (tree &optional  (last-symbol nil last-symbol-supplied-p) (nb-pulses 0) )
  "Converts the internal measure representation into an OM rhythm tree.
  n : note
  r : rest
  s : slur
  = : merge with following pulse"
  ;Conversion de la structure arborescente
  (let ((new-children nil) 
        (eq-count 0)) ;to count the number of :=
    (dolist (child tree)
    (cond ( (equalp child :n) ; note
            ( push (1+ eq-count) new-children) 
            (setf eq-count 0)
            (setf last-symbol 1))
          ( (equalp child :r) ;rest
            ( push (- (1+ eq-count)) new-children)
            (setf eq-count 0)
            (setf last-symbol -1))
          ( (listp child) ;subdivision
            (multiple-value-bind (mes last) (tree-to-om child last-symbol eq-count )
                (push  mes new-children) 
            (setf eq-count 0)
            (setf last-symbol last)));to remember the last symbol of the subdivision
          ( (equalp child :=) (incf eq-count)) ;a merge symbol
          ( (equalp child :s) ; a slur
            (if  (numberp last-symbol)
                ( push (* (float last-symbol) (1+ eq-count)) new-children)
              (progn 
                (print ":s at very beginning so I decided it was a note.")
                ( push  (1+ eq-count) new-children)))
            (setf eq-count 0))
          ( t (print "Error while converting")))
    )
(setf new-children (nreverse new-children))
(values (list (1+ nb-pulses) new-children) last-symbol);return the subdivision and the last symbol
))




(om::defmethod! from-omtree (tree)
                :doc "Converts the internal representation into an Om rhythm tree"
                :indoc '("a internal tree")
                :icon 111
                :numouts 2
  ;Get the measures
  ;Get the duration
               (values 
                ; the measures
                (mapcar #'(lambda (measure) (pulse-from-om (canonize (second measure) ))) (second tree))
                ; the time signature
                (mapcar #'first (second tree)))
  )


(defun pulse-from-om (om-measure)
  "Converts an om measure (more generally, a subdivision) into the measure internal representation"
  (unless (null om-measure)
    (let* ( (subdi (first om-measure))
            (symbols (typecase subdi
                       (number (expand-symb subdi))
                       (list  (expand-symb (first subdi) (pulse-from-om (canonize (cadr subdi)))))
                       ))
            )
      (append symbols (pulse-from-om (rest om-measure)))
      ))
)

(defun symbols-from-om (symb) 
  "Converts unary numbers into their internal representation"
  (typecase symb
    (float :s)
    (integer (if (plusp symb) :n :r))
    )
  )

(defun expand-symb (symb &optional (subdivision nil))
  "When n > 1 hit, expand it. Works on an isolated n or if it is the duration of a subdivision
as in (2 (1 1)).
Example : 
4 -> := := := n"
  (append (make-list (1- (abs (floor symb))) :initial-element :=) 
          (if (null subdivision) (list (symbols-from-om symb)) (list subdivision)))
)

(defun canonize (om-measure)
"Processes the sub-rhythm tree : divides all ratios at the same level by the gcd.
om-measure should be only the subdivision, not the duration"
;gcd ( (gcd a b c) = (gcd (gcd a b) c) )
; (gcd 0 n) = n 
(flet ((gcd-reduce (a b) 
         (let ((nb1 (floor a)))
           (typecase b
             (list (gcd nb1 (floor (first b)))) ; gc operates only on integers, of course
             (number (gcd nb1 (floor b)))
             ))))
  (let ((gcd-measure (reduce #'gcd-reduce om-measure :initial-value 0)))
      (mapcar #'(lambda (n) 
           ( typecase n
             (list (cons (/ (first n) gcd-measure) (canonize (rest n))))
             (number (/ n gcd-measure))
            ))
              om-measure)
    )
  )
)

;EXAMPLE NOT WORKING !
;(from-omtree '(? (((1 4) ((1 ((1 ((1 (-1 -1)) 1)) 1)))))))



;TODO : does not work
;(om::defmethod! gcdize (om-tree)
;            :doc "Simplifies an om rhythm tree by deviding all subdivisions of the same level by their gcd"
;            :icon 111
;            :indoc '("om rhythm tree")
;            (labels ((gcdize-sublevels (measure) 
;                       (mapcar #'(lambda (subdi) (canonize (gcdize-sublevels subdi))) measure)))
;
;              (mapcar #'(lambda (measure) (gcdize-sublevels (second measure ))) (second om-tree))
;              )
;)


(om::defmethod! gcdize (om-tree)
                :doc "Simplifies an om rhythm tree by deviding all subdivisions of the same level by their gcd"
                :icon 111
                :indoc '("om rhythm tree")
                
                (let* (
                       (durations-canonized (mapcar #'(lambda (tree) (canonize (second tree))) (second om-tree)))
                       (signatures (mapcar #'(lambda(tree) (first tree)) (second om-tree)))
                       )
                  (list :? (mapcar #'(lambda (mes sig) (list sig mes)) durations-canonized signatures))
                  )
                )





;;Functions to discover the content of symbolic rhythm trees

(defun note-p (symb)
  "True if symb is a note. Currently, true if it is a slur also, even if a slur could be a silence"
  (or (equalp symb :n) (equalp symb :s)))

(defun silence-p (symb)
  "True if symb is a silence"
  (equalp symb :r))

(defun has-note (subdi)
  "True if there is a note (or a slur) at any depth in the subdivision"
  (cond 
   ( (null subdi) nil)
   ( (listp subdi) (or (has-note (first subdi)) (has-note (rest subdi))))
   ( (note-p subdi) t)))

(defun closest-right-subdi (subdi)
  "Returns the closest right symbol : (n n (r (n r))) -> r"
  (cond 
   ( (null subdi) nil)
   ( (listp subdi) (closest-right-subdi (first (last subdi))))
   ( t subdi)))
                             
(defun closest-left-subdi (subdi)
  "Returns the closest left subdivision ((((((n r) r) r ) s )r )) -> n"
  (cond 
   ( (null subdi) nil)
   ( (listp subdi) (closest-left-subdi (first subdi)))
   (t subdi)))

(defun change-closest-left-subdi (subdi new)
  "Modifies the bootom left subdi et put new in it"
  (cond 
   ((null subdi) nil)
   ( ( listp subdi)  (cons (change-closest-left-subdi (first subdi) new) (rest subdi)))
   ( t new)))
  
;(change-closest-left-subdi '(((((((:n :r ) :r ) :r ) :r :r :r ) :r ) :r :r ) :r) :s)

;(change-closest-left-subdi '(:r :n :n :n) :n)

