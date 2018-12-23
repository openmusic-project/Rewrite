; Author : Pierre Donat-Bouillud

;;Interface which displays a tree, to act o, and display the quantification
(in-package :rw)

(use-package :oa :rw)


; (amount-strategy-choose)

(defun amount-strategy-choose ()
  "A function to choose the amount of the strategy"
  (let ((amount 0.5))
    (let* ((window (om-make-window 'om-dialog :size (om-make-point 300 120)))
          (text (om-make-dialog-item 'om-static-text (om-make-point 230 20) (om-make-point 30 15)  (write-to-string amount)))
          (slider (om-make-dialog-item 'om-slider (om-make-point 20 20) (om-make-point 200 20) ""
                                             :value (* amount 100)
                                             :range '(0 100)
                                             :di-action #'(lambda(slide)  
                                                            (om-set-dialog-item-text text (write-to-string (float (/ (om-slider-value slide) 100))))))))
      
      (om-add-subviews window 
                       
                       slider
                       text
                       (om-make-dialog-item 'om-button (om-make-point 100 60) (om-make-point 60 10)
                                            "OK"
                                            :di-action #'(lambda (button) (om-return-from-modal-dialog window 
                                                                                                       (* (om-slider-value slider) 0.01)))))
      
      (om-modal-dialog window nil)
      )))


      
#|  
(capi::contain (make-instance 'capi::slider :start 0.0 
                                :end 1.0 
                                :slug-start 0.5
                                :callback #'(lambda (data &rest args) (setf amount data))))
    (make-instance 'amount-strategy :amount amount)))
|#



;TODO : mieux détecter la taille du tableau (en principe, ils devraient s'agrandir)
;With hashtables. Returns the function node-children
(defun unique-labels (tree)
  (let ((unique-id 0) 
        (table (make-array 1000 :fill-pointer 999 :adjustable t));array to store list of sons
        (tags (make-array 1000 :fill-pointer 999 :adjustable t)));array to store the tags for the item
        (labels ((save-node (id sons tag)
                   (if (< id (length table))
                       (progn 
                         (setf (elt table id) sons)
                         (setf (elt tags id) tag))
                     (progn
                       (vector-push  sons table)
                       (vector-push tag tags))))
                     
                     
                     
                 (labelize (node)
                   (let ((id unique-id))
                     (incf unique-id)
                     (typecase node
                       (list (save-node id (mapcar #'labelize node) :u)
                       #| (if (< id (length table))
                             (setf (elt table id) (mapcar #'labelize node))
                          (vector-push (mapcar #'labelize node) table)))|#
                             )
                       (t (save-node id (- id) node)
                        #|(if (< id (length table))
                            (setf (elt table id) (- id))
                          (vector-push (- id) table))|#
                        ))
                     id
                     )))
          (labelize tree)     
          (values
           (lambda (node table)
             (when (>= node 0)
               (let ((new-node (elt table node)))
                 (typecase new-node
                   (list new-node)
                   (t nil))
                 
                 )))
           (lambda (graph node tags)
             (let ((symb (elt tags node)))
               (make-instance 'capi::item-pinboard-object :text (if (equalp symb :u) "|" (symbol-name symb)))))
           table
           tags)
          )))




;;REMOVE this method ?
;pas besoin de rédéfinir node-pane-function, mais plutôt renvoyer des items ?
(om::defmethod! display-tree ((tree list))
            :icon 111
            :intivals '((:r :n (:n :r) (:n (:n :n))))
            :indoc '("Internal rhythm tree")
            :doc "Displays as a tree <tree>"

            (multiple-value-bind (node-children node-pane-function table tags) (unique-labels tree)
              
              (capi:contain  (make-instance 'capi:graph-pane
                                            :roots '(0)
                                            :children-function #'(lambda (node) (funcall node-children node table))
                                            :layout-function 
                                            :top-down        
                                            :node-pane-function #'(lambda (graph node) (funcall node-pane-function graph node tags))
                                            :internal-border 10
                                              ;:visible-border :outline
                                            )
                             )
               ;(print table)
               ;(print tags)
              
              )
            
            )

;; Class to display a tree

(om::defclass! rtree () ((tree :accessor tree :initarg :tree :initform nil)) (:icon 130))

(defmethod om::class-has-editor-p ((self rtree)) t)
(defmethod om::get-editor-class ((self rtree)) 'rtree-editor)

(defclass rtree-editor (om::editorview) 
  ((treeview :accessor treeview :initarg :treeview :initform nil)))

(defclass om-graph-pane (capi:graph-pane oa::om-graphic-object) ())

(defmethod initialize-instance :after ((self rtree-editor) &rest args) 
  (multiple-value-bind (node-children node-pane-function table tags) (unique-labels (tree (om::object self)))
              
    (om-add-subviews self
                     (setf (treeview self)
                           (make-instance 'om-graph-pane
                                          :default-x 10 :default-y 10
                                          :width 300
                                          :height 300
                                    :roots '(0)
                                    :children-function #'(lambda (node) (funcall node-children node table))
                                    :layout-function :top-down        
                                    :node-pane-function #'(lambda (graph node) (funcall node-pane-function graph node tags))
                                    :internal-border 10
                                              ;:visible-border :outline
                                    )
                           ))  
  ))

(defmethod om::update-subviews ((self rtree-editor))
  (om-set-view-size (treeview self)
                    (om-subtract-points (om-view-size self) (om-make-point 20 20))))


(om::defclass! transformation () 
               ((transform-function :accessor transform-function :initarg :transform-function :initform nil)
                (tree-init :accessor tree-init :initarg :tree-init :initform '(n))
                (tree-res :accessor tree-res :initarg :tree-res :initform '(n))
                (transform-name :accessor transform-name :initarg :transform-name :initform "My-transform" ) 
                (min-depth :accessor min-depth :initarg :min-depth :initform 0)
                (max-depth :accessor max-depth :initarg :max-depth :initform -1))
               (:documentation "Define your own transformations")
               (:icon 130))

(defmethod om::class-has-editor-p ((self transformation)) t)
(defmethod om::get-editor-class ((self transformation)) 'transform-editor)


(defclass transform-editor (om::editorview)
  ((text-pat-init :accessor text-pat-init :initarg :text-pat-init :initform nil)
   (text-pat-res :accessor text-pat-res :initarg :text-pat-res :initform nil)
  (tree-view-init  :initarg :tree-view-init :initform nil)
  (tree-view-res  :initform nil)
  (check-box :initform nil)
  (min-depth :initform nil)
  (max-depth :initform nil)
  (name-view :initform "")))

; Entails a stack overflow... strange...
#|(defmethod om::update-subviews ((self transform-editor))
  (om-set-view-size self (om-make-point 500 500)))|#

(defun replace-subview (view prev-subview new-subview)
  (let ((size (om-view-size prev-subview))
        (pos (om-view-position prev-subview))) ; to use the prev coordinates, but graph-pane is not an openmusic object, so... useless
      (om-remove-subviews view prev-subview)
    ;(om-set-view-size new-subview size) ; does not work
    ;(om-set-view-position new-subview pos) ; neither
      (om-add-subviews view new-subview)
      (om-invalidate-view view)
      new-subview
    ))


(defun list-to-string (l)
  "Converts a list into a string"
  (format nil "~a" l))

(defun get-rhythm-list (s)
  "Converts a string into a list with symbols in :keywords"
  (let ((*package* (find-package :keyword)))
                                  (read-from-string s)))


;Ugly hack for the macro...
(defparameter *name-transform* "")

(defmacro dynamic-user-transform ( pattern-match pattern-write &key (min-depth 0) (max-depth -1) (documentation ""))
  `(user-transform ,(intern *name-transform*) ,pattern-match ,pattern-write :min-depth ,min-depth :max-depth ,max-depth :documentation ,documentation))

(defmethod  initialize-instance :after ((self transform-editor) &rest args)
  ;(om::om-set-view-size (om-view-container self) (om::om-make-point 600 600))
  (om-set-help t)
  (labels ((generate-tree (edit-text)
           (unique-labels (get-rhythm-list edit-text)))
         (create-tree-view (edit-text posX )
           (multiple-value-bind (node-children node-pane-function table tags) (generate-tree edit-text)
              (make-instance 'om-graph-pane :default-x posX :default-y 40
                                                             :width 190 :height 200 :roots '(0)
                                                             :children-function  #'(lambda (node) (funcall node-children node table))
                                                             :layout-function :top-down  
                                                             :node-pane-function #'(lambda (graph node) (funcall node-pane-function graph node tags)))
                                                             ) )
         (gen-function ()
           (let ((*name-transform* (transform-name (om::object self))))
             (dynamic-user-transform   (tree-init (om::object self)) (tree-res (om::object self))
                                       :min-depth (parse-integer (om-dialog-item-text (min-depth self)))
                                       :max-depth (parse-integer (om-dialog-item-text (max-depth self)))
                                       :documentation (concat 'string "Generated transformation : " (transform-name (om::object self)))
                                       )
           ))
         (save-state ()
           (setf (tree-init (om::object self)) (get-rhythm-list (om-dialog-item-text (text-pat-init self))))
           (setf (tree-res (om::object self)) (get-rhythm-list (om-dialog-item-text (text-pat-res self))))
           (setf (transform-name (om::object self)) (om-dialog-item-text (slot-value self 'name-view)))
           (gen-function)
           (setf (transform-function (om::object self))  #'(lambda (sudbi new-silence) (funcall (intern *name-transform*) subdi new-silence)))
           (setf (min-depth (om::object self)) (parse-integer (om-dialog-item-text (slot-value self 'min-depth))))
           (when (om-checked-p (slot-value self 'check-box))
             (setf (max-depth (om::object self)) (parse-integer (om-dialog-item-text (slot-value self 'max-depth)))))
           ))
  (om-add-subviews self
                  (setf (text-pat-init self)
                        (om-make-dialog-item 'om-editable-text 
                                             (om-make-point 10 10) (om-make-point 170 15)  (list-to-string (tree-init (om::object self)))
                                             :di-action #'(lambda (edit-text)  (replace-subview self (slot-value self 'tree-view-init )
                                                                                                (setf (slot-value self 'tree-view-init) 
                                                                                                      (create-tree-view (om-dialog-item-text edit-text) 10  )))
                                                            (save-state))
                                             :change-callback nil))
                  (om-make-dialog-item 'om-static-text
                                       (om-make-point 230 15) (om-make-point 20 15) "=>")
                  (setf (text-pat-res self) 
                        (om-make-dialog-item 'om-editable-text
                                             (om-make-point 280 10) (om-make-point 170 15) (list-to-string (tree-res (om::object self)))
                                             :di-action #'(lambda (edit-text) (replace-subview self (slot-value self 'tree-view-res)
                                                                                               (setf (slot-value self 'tree-view-res) 
                                                                                                     (create-tree-view (om-dialog-item-text edit-text) 280 )))
                                                            (save-state))
                                             :change-callback nil))
                  (setf (slot-value self 'tree-view-init) (create-tree-view   (list-to-string (tree-init (om::object self))) 10))
                  (setf (slot-value self 'tree-view-res) (create-tree-view  (list-to-string (tree-res (om::object self))) 280))
                  (setf (slot-value self 'check-box) (om-make-dialog-item 'om-check-box
                                       (om-make-point 10 250) (om-make-point 20 15)
                                       "Max depth"
                                      ))
                  (setf (slot-value self 'max-depth) (om-make-dialog-item 'om-editable-text
                                                                          (om-make-point 30 250) (om-make-point 50 12)
                                                                          (write-to-string (max-depth (om::object self)))
                                                                          :di-action #'(lambda (edit-text) (save-state))
                                                                          :change-callback nil))
                  (setf (slot-value self 'min-depth) (om-make-dialog-item 'om-editable-text
                                                                         (om-make-point 100 250) (om-make-point 50 12)
                                                                         (write-to-string (min-depth (om::object self)))
                                                                         :di-action #'(lambda (edit-text) (save-state))
                                                                         :change-callback nil))
                  (setf (slot-value self 'name-view) (om-make-dialog-item 'om-editable-text
                                                                     (om-make-point 180 250) (om-make-point 100 12)
                                                                     (transform-name (om::object self))
                                                                     :di-action #'(lambda (edit-text) (save-state))
                                                                     ))
                  (om-make-dialog-item 'om-static-text
                                       (om-make-point 30 280) (om-make-point 50 40)
                                       "Max depth")
                  (om-make-dialog-item 'om-static-text
                                       (om-make-point 100 280) (om-make-point 50 40)
                                       "Min depth")
                  (om-make-dialog-item 'om-static-text
                                       (om-make-point 180 280) (om-make-point 100 40)
                                       "Nom de la transformation")
)
  (om-set-check-box (slot-value self 'check-box) (/= (max-depth (om::object self)) -1))
  (om-view-set-help (text-pat-init self) "Pattern to match")
  (om-view-set-help (text-pat-res self) "New subtree")
  ))


(om::defmethod! tree-transform ( (tree list) (transforms list) measure)
                :initvals '((n) ('merge-rs) t)
                :indoc '("onsets" "durations" "subdivision" "depth" "tempo" "strategy") 
                :icon 111  ; the icon
                :doc "Transform <tree> using <transforms>"
  (apply-transforms tree transforms :measure measure))


