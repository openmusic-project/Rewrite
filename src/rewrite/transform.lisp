; Author : Pierre Donat-Bouillud

;;;;Set of transformation (rewriting rules) on the trees (see general.lisp)

(in-package :rw)



;;;;Transforms


;TODO : information passing between adjacent subdivisions do not work
(defun apply-transform ( tree transform &key (measure nil))
  "Applies a given transformation to a rhythm tree. You should provide a symbol, et not a function.
If specified measure, the upper subdivision are regarded as measures, and treated accordingly"
(let ( (min-depth (getf (symbol-plist transform) 'min-depth))
       (max-depth (getf (symbol-plist transform) 'max-depth))
       ; new-silence : to track the cration of new silence in late subdivisions, to ensure that n s s s -> r s s s -> r n s s 
       (new-silence t))
  
  (labels ( (last-subdi(subdi)
              (if (listp subdi)
                  (first (last subdi))
                subdi))
           (execute (tree depth)
                    (typecase tree
                      (list (funcall 
                             (if  (or (< depth min-depth) (and (/= max-depth -1) (> depth max-depth)))   ;si depth n'est pas entre min et max, la fonction à appliquer est #'(lambda(x y) x) (renvoie uniquement la liste des sudivisions transformées, sinon c'est transform
                                 #'(lambda(x y) x)
                               transform)
                             (mapcar #'(lambda(subdi)                                                    ;on applique cette fonction sur la liste des subdivisions transformées
                                         (let* ((new-subdi (execute subdi (1+ depth)))                   ;on applique d'abord les transformations sur les sous-arbres les plus bas de l'arbre :
                                                (new-last-symb (last-subdi new-subdi)))                  ;appels de execute jusqu'à ce qu'on arrive à max-depth ou que l'on n'ait plus une liste, a ce moment la on commence a renvoyer les listes et leur appliquer transform
                                           (setf new-silence (and (equalp new-last-symb :r)
                                                                   (not (equalp (last-subdi subdi) new-last-symb))))
                                           new-subdi)) tree ) new-silence))                              ;interet de new-silence ??
                      (t #|(let ((new-symb (funcall transform tree new-silence)))
                           (setf new-silence (equalp new-symb :r) )
                           new-symb)|# 
                          tree)
                      )))
    (if measure
        (mapcar #'(lambda(x) (if (listp x) x (list x))) (execute tree -1))
    (execute tree 0))
)
))


(defun apply-transforms (tree transforms &key (measure nil))
  (if (null transforms)
      tree
    (progn
      (print (first transforms))
      (apply-transforms (apply-transform tree (first transforms) :measure measure) (rest transforms) :measure measure))))




;;Maybe use that to create the transformations : data and function as the same time. But problems for recursion
;; because the funcallable object has to be called with (funcall fun) and not directly with (fun).
;; And over all : information not stored by a class of transformation, but by an instance of it
(defclass transform (funcallable-standard-object)
  ((min-depth :initarg :min-depth 
        :initform 0
        :accessor min-depth
        :documentation "The transform begins to be applied at this depth")
   (max-depth :initarg :max-depth
              :initform -1 ;-1 stands for no max depth
              :accessor max-depth
              :documentation "The transform is not applied when depth is > max-depth")
   (transform-action :initarg :transform-action
                     :initform #'identity
                     :accessor transform-action
                     :documentation "Function for the transformation of the tree : should have at leat one argument which is either a node or a leaf")
   (documentation :initarg :doc
                  :initform ""
                  :documentation "Documentation for the transformation")
              )
  (:metaclass funcallable-standard-class))

;To intialize the methode to be called
#|(defmethod initialize-instance :after ((c transform) &key)
  
    (set-funcallable-instance-function
      c
      #'(lambda ()
          (let ((new (make-array (1+ (length fields)))))
            (setf (aref new 0) name)
            new))))|#


;We should be able to define functions and variables shared by all the funcall of transforms : put them in a clojure, of course ! Or not...
(defmacro deftransform (name lambda-list node-form leaf-form &key (min-depth 0) (max-depth -1) (documentation ""))
  "Create a transformation : 
-lambda-list : at leat two args, the first one will be the subdivision, and the second one a boolean which indicates whether the last symbol of
the late subdivision has been transformed into a silence
- node-form : executed when arg is a node
- node-leaf : executed when arg is a leaf
- min-depth : transform is executed when depth in the rhythm tree is >= min-depth. Depth = 0 <=> measure
- max-depth : transform is not executed avy more when depth > max-depth ( max-depth = -1 means no max-depth) "
  (setf (symbol-plist name) nil);erase the plist if there is one
  ;;set attributes of the function
  (setf (get name 'min-depth) min-depth)
  (setf (get name 'max-depth) max-depth)
  (setf (documentation name 'function) documentation)
  ;is there an argument for new-silence ?
  `(defun ,name ,lambda-list
     (typecase ,(first lambda-list)
         (list ,@node-form)
         (t ,@leaf-form)))
    
  )

;Previous code for deftransform to choose the right number of args 
#|
,(cond
                  ((= (length lambda-list) 1) (list (first lambda-list) '&key 'azglopbli892976))
                  ( (or (equalp (second lambda-list) '&key) (equalp (second lambda-list) '&optional)
                        (equalp (second lambda-list) '&rest))
                    (list* (first lambda-list) 'azglopbli892976 (rest lambda-list))))
|#



;;Basic transformations
;; = transformations which preserve rythms


;; " ( ( :r :r :r ) :n ) -> ( :r :n) ( :n (:s :s) :n ) -> ( :n :s :n)"
(deftransform merge-rs (subdi new-silence)
              ( (let ((first-subdi (first subdi)))
             ;all the elements of the suddivision are the same
             (if (and (reduce #'(lambda(a b) (if (equalp a b) a nil)) subdi) (or (equalp first-subdi :s) (equalp first-subdi :r))) first-subdi subdi)))
              (subdi)
              :min-depth 0
              :documentation  " ( ( :r :r :r ) :n ) -> ( :r :n) ( :n (:s :s) :n ) -> ( :n :s :n)")
  

;; "( :n  (:n :s :s :s :s ) :r) -> (:n :s :r)"
(deftransform merge-ns (subdi new-silence)
              ((if (and (not (null (rest subdi))) (equalp (merge-rs (rest subdi) new-silence) :s))
              (first subdi)
              subdi))
              (subdi)
              :min-depth 0
              :documentation "( :n  (:n :s :s :s :s ) :r) -> (:n :n :r)")

;;Moves up a lonely node
(deftransform move-up (subdi new-silence)
              ( (if (= (length subdi) 1)
                    (first subdi)
                  subdi
                  ))
                (subdi)
                :min-depth 1
                :documentation "(Moves up a lonely node : ((n)) -> (n)")

;Quite similar to to-omtree
; Do not handle = for now, but it is planned
(defun tokenize-count (subdi)
  "Returns three lists : the first one are the elements of the subdi, but only one by element n s s becomes n for instance,
                                       r r r -> r and a list which gives their durations."
  (let ((types nil)
        (durations nil)
        (last-symbol nil)
        (current-duration 0))
    (flet ((push-symb ()
             (when (> current-duration 0)
               (push last-symbol types)
               (push current-duration durations)))
           (reset-symb (symb) 
             (setf current-duration 1)
             (setf last-symbol symb))
           (inc-symb (symb)
             (incf current-duration)
             (setf last-symbol symb)))
      (dolist (child subdi)
        (cond
         ((equalp child :n)
          (push-symb)
          (reset-symb :n))
         ( (equalp child :r)
           (if (equalp last-symbol :r)
               (incf current-duration)
             (progn 
               (push-symb)
               (reset-symb :r))))
         ( (equalp child :s) ; :s is always a note, so, if there is r s, we correct that
           (if (not (or (equalp last-symbol :n) (equalp last-symbol :s)))
               (progn 
                 (push-symb)
                 (reset-symb :n))
             (inc-symb :n)))
         ( (listp child)
           (push-symb)
           (reset-symb child))
         (t (print "Error while parsing")))) 
      (push-symb))
    (setf types (nreverse types))
    (setf durations (nreverse durations))
    (list types durations)))


(tokenize-count '(:r :r :r :n :s :s (:r :r :n)))

(deftransform resubdivize (subdi new-silence)
              ( (destructuring-bind (types durations) (tokenize-count subdi)
                  (let ((gcd-subdi (apply #'gcd durations)))
                    (if (> gcd-subdi 1); works without this cond, but for performance reasons...
                        (let ((new-durations (mapcar #'(lambda (n) (/ n gcd-subdi)) durations)))
                          ;(print new-durations)
                          (loop for duration in new-durations 
                                for symb in types
                                append (make-list duration :initial-element symb)))
                      subdi))))
              (subdi)
              :documentation "Resubdivize. For instance, n s n s -> n n"
              :min-depth 1)
                    
                   


;; "( n n s n) -> (n = n n)"
(deftransform gen-= (subdi new-silence)
              ((cond ((not (null (rest subdi))); we look at 2 ajdacents elements at the same time
                    (let ((current-symbol (first subdi))
                          (next-symbol (second subdi)))
                      ;To handle cases like (:n := := :s), need to have pred symbols from previous subdivisions
                      ;(if (and (equalp next-symbol :s) (or (equalp current-symbol :n ) (equalp current-symbol :r)));first version : do not merge s s
                      ;(if (and (equalp next-symbol :s)  (not (listp current-symbol))); second version : do not merge r r
                      (if (or (and (equalp next-symbol :s)  (not (listp current-symbol))) (and (equalp next-symbol :r) (equalp current-symbol :r)))
                          (cons := (gen-= (cons current-symbol (cddr subdi)) new-silence))
                        (cons current-symbol (gen-= (rest subdi) new-silence)))))
                     ( (not (null subdi)) subdi)
                   ))
              (subdi)
              :min-depth 0
              :documentation "( n n s n) -> (n = n n)")


;; Transformations which do not preserve rythms

;; Dilatation (in fact, majority)
;For now, nb-left and nb-right do not change for the whole sequence, but you could do that
(defun local-dilate (seq  current nb-left nb-right)
  "Dilatation in <seq> around <current> looking at <nb-left> left elements and <nb-right> right elements"
  (let ((central-elem (elt seq current))
        (nb-notes 0)
        (nb-rests 0))
    (flet ((put-note ()
             (cond 
              ((zerop current) :n)
              ((or (equalp central-elem :n) (equalp central-elem :s)) central-elem)
              (t :s))))
      (if (listp central-elem) 
          central-elem
        (progn
          (dotimes (i (+ 1 nb-left nb-right))
          ;handles the case of the beginning or the end of the seq
            (let ((current-note (handler-case (elt seq (+ current (- nb-left) i)) (error () :u)))) ; :u not a note nor a rest
              (cond 
               ( (or (equalp current-note :n) (equalp current-note :s)) (incf nb-notes))
               ( (equalp current-note :r) (incf nb-rests)))))
          (cond 
           ((> nb-notes nb-rests) (put-note));default : :s but if it was :n before, :n
           ( (< nb-notes nb-rests) :r);also dilatation on rests
           ( (= nb-notes nb-rests) central-elem)))))))

  
;;The two next transformations are not very efficient because they only act on a notes and rests of a subdivision, and not its own subdivisions

;We do not use information about adjacent subdivisions : this a good thing, since we rather like notes on pulses...
(deftransform dilate (subdi new-silence &key (nb-neighbours-left 2) (nb-neighbours-right 2) )
              ( (let* ( (length-subdi (length subdi))
                        ; conversion from a list o an array
                       (sequ (make-array  length-subdi :initial-contents subdi))
                       (new-sequ (make-array length-subdi :initial-element :r)))
                  (dotimes (i (length sequ))
                    (setf (elt new-sequ i) (local-dilate sequ i nb-neighbours-left nb-neighbours-right)))
                  ;conversion from an array to a list
                  (loop for i below length-subdi
                        collect (elt new-sequ i))))
              (subdi)
              :min-depth 1
              :documentation "Dilatation by majority. Parameters : nb-neighbours-left and nb-neighbours-right")
                    


(deftransform metric-reduce (subdi new-silence &key (nb-neighbours-left 2) (nb-neighbours-right 2) )
              ( (let* ( (length-subdi (length subdi))
                        ; conversion from a list o an array
                        (sequ (make-array  length-subdi :initial-contents subdi))
                        (new-sequ (make-array length-subdi :initial-element :r)))
                  (dotimes (i (length sequ))
                    (setf (elt new-sequ i) (local-dilate sequ i nb-neighbours-left nb-neighbours-right)))
                  ;we delete the element where rests have been added
                  (loop for i below length-subdi
                        append (let ((new-elem (elt new-sequ i))) (if (and (equalp new-elem :r) (not (equalp (elt sequ i) :r))) nil (list new-elem))))))
              (subdi)
              :min-depth 1
              :documentation "Dilates and then erases new silences")
            
; Problem with this transformation : if a note is cut which was followed by a slur is cut, the slur will be interpreted as a silence.
(deftransform cut-sides (subdi new-silence &key (nb-left 1) (nb-right 1)) 
              ((let ((nb-notes (count-if #'has-note subdi))
                     (length-subdi (length subdi))
                     ;Where is the first rest ?
                     (first-left-silence (position-if #'silence-p subdi ))
                     (first-right-silence (position-if #'silence-p subdi  :from-end t)))
                 (if (< (- (+ first-left-silence first-right-silence) 2) nb-notes) ; nb of notes on sides < total nb of notes
                       (loop for i from 1
                             for element in subdi
                             when (and (<= i nb-left) (<= (1- first-left-silence) nb-left)) collect :r
                             else when (and (>= i (- length-subdi nb-right)) (<= (1- first-right-silence) nb-right))  collect :r
                             ; we handle also the case when the transform created a silence at the late subdivision
                             else collect (if (and (= i 1) new-silence) :n element))
                   ; otherwise, nb of notes on sides = total nb of notes
                   ;we should check the number of notes on each sides
                    :r)))
              ( subdi)
              :min-depth 2
              :documentation "Removes notes on sides of a subdivision.")  



(defun correct-slurs-level (subdi)
  "Auxiliary function for correct-slurs : process a subdivision"
  (unless  (null (rest subdi))
    (let ((first-subdi (first subdi))
          (snd-subdi (second subdi)))          
        (let ((last-first-subdi (closest-right-subdi first-subdi))
              (first-snd-subdi (closest-left-subdi snd-subdi)))
          
          #|(change-closest-left-subdi (cons first-snd-subdi (correct-slurs-level (rest subdi) ) )
                                     (if (and (equalp first-snd-subdi :s) (equalp last-first-subdi :r))
                                         :n
                                       snd-subdi  ))|#

          (cons  (if (and (equalp first-snd-subdi :s) (equalp last-first-subdi :r))
                     (change-closest-left-subdi snd-subdi :n)
                   snd-subdi  )
                 (correct-slurs-level (rest subdi) ))))
   ))

(deftransform correct-slurs (subdi new-silence)
              ( (cond 
                 ((null (rest subdi)) subdi)
                 (t (cons (first subdi) (correct-slurs-level subdi)))))
              (subdi)
              :min-depth 0
              :documentation "A hack for correcting outputs of cut-sides")

#|(deftransform tuplet-gen (subdi new-silence &key (nb-subdi)
              ( (destructuring-bind (tokens durations) (tokenize-count subdi)
                  (let ((length-subdi (length subdi))
                        (length-tokens (length tokens)))|#



;;; User (-friendly) defining new transformations

(defun match (tree pattern)
  "True if tree matches the pattern.
    Second return value : binding of the variables and their values"
  (let ((variables nil))
    (labels ((process-tree (tr pat)
               (if (or (null tr) (null pat))
                   (when (and (null tr) (null pat))
                     (values t t))
                 (let ((f-tr (first tr))
                       (f-pat (first pat))
                       (r-tr (rest tr))
                       (r-pat (rest pat)))
                   (typecase f-pat
                     (list (and (listp f-pat) (process-tree r-tr r-pat) (process-tree f-tr f-pat)))
                     (t (if (or (equalp f-pat :n)
                                (equalp f-pat :s)
                                (equalp f-pat :=)
                                (equalp f-pat :r))
                            (and (equalp f-tr f-pat) (process-tree r-tr r-pat))
                          ;otherwise it is a variable
                          (let ((binding (assoc f-pat variables)))
                            (if binding
                                ;is the new binding of the vairable equal to the previous binding ?
                                (and (equalp (cdr binding) f-tr) (process-tree r-tr r-pat))
                              (progn
                                (push (cons f-pat f-tr) variables) ; add the new variable
                                (process-tree r-tr r-pat)))))))))))             
        (values (process-tree tree pattern) variables))))
      

;(match '((:n :r :s) :n :n (:n :r :s)) '(x :n :n x))

(defun inject-variables (pattern variables)
  "Return the pattern with the variables substituted by their values.
    variables is an association list"
  (unless (null pattern)
      (typecase pattern
        (list (cons (inject-variables (first pattern) variables) (inject-variables (rest pattern) variables)))
        (t (let ((binding (assoc pattern variables)))
             (if binding
                 (rest binding)
               pattern))))))
             
;(inject-variables '(x :n :n x) '((x :n :r :n)))


;;WHat if the user creates a pattern with only a symbol ?
(defmacro user-transform (name pattern-match pattern-write &key (min-depth 0) (max-depth -1) (documentation ""))
  "Creates a transform which rewrite pattern-match when matched into pattern-write.
Name is a symbol for the transform. See deftransform for the other args"
  `(deftransform ,name (subdi new-silence)
                ((multiple-value-bind (matched variables) (match subdi ,pattern-match)
                   (if matched
                       (inject-variables ,pattern-write variables)
                     subdi)))
                (subdi)
                :min-depth ,min-depth
                :max-depth ,max-depth
                :documentation ,documentation))
  
;(user-transform test '(x :n :n x) '(x :n x))
;(apply-transform '((:n :n) :n :n (:n :n)) 'test)