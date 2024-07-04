;; https://en.wikipedia.org/wiki/BK-tree

(defpackage pure-spatial-trees/bk
  (:use #:cl)
  (:shadow #:delete)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:lens #:picolens))
  (:export #:node
           #:tree
           #:metric
           #:insert
           #:delete
           #:nearest-neighbors
           #:nearest-neighbor
           #:items-in-ball
           #:belongs-to-set-p
           #:flatten

           #:sets-equal-p
           #:trees-equal-p))
(in-package :pure-spatial-trees/bk)

(sera:defconstructor node
  (value    t)
  (visible  boolean)
  (children list))

(lens:gen-lenses node
  (node-value    value)
  (node-visible  visible)
  (node-children children))

(deftype tree () '(or node null))
(deftype metric () '(sera:-> (t t) (values alex:non-negative-fixnum &optional)))

(sera:-> leaf-node (t)
         (values node &optional))
(declaim (inline leaf-node))
(defun leaf-node (obj)
  "Construct a fresh leaf node containing @c(obj) as its value."
  (node obj t nil))

;; A lens for sets which does not respect the laws
(defun item (e)
  (declare (optimize (speed 3)))
  (lambda (fmap f x)
    (declare (type function f fmap))
    (funcall
     fmap (lambda (s)
            (cons s (remove e x)))
     (funcall f e))))

(sera:-> insert (tree t metric)
         (values tree boolean &optional))
(defun insert (tree obj distance)
  "Insert a new object in the tree. An empty tree is designated by
@c(nil). This operation does not modify its arguments and returns a
new tree instead. The second value is @c(t) if insertion was
successfull and @c(nil) otherwise (when the object is already in the
tree). An integer-valued metric @c(distance) on objects in the tree
must be supplied."
  (declare (optimize (speed 3)))
  (labels ((%go (lens)
             (let* ((node (lens:view lens tree))
                    (d (funcall distance obj (node-value node))))
               (if (zerop d)
                   ;; There is a node with this object
                   (if (node-visible node)
                       ;; It's not deleted, do nothing
                       (values tree nil)
                       ;; It's deleted, mark as visible
                       (values (lens:set (lens:compose lens #'visible) t tree) t))
                   (let ((child (find-if
                                 (lambda (child)
                                   (= (funcall distance
                                               (node-value child)
                                               (node-value node))
                                      d))
                                 (node-children node))))
                     (if child
                         ;; Insert at least one level deeper
                         (%go (lens:compose lens #'children (item child)))
                         ;; Add the new node as a child of NODE
                         (values
                          (lens:over (lens:compose lens #'children)
                                     (lambda (children)
                                       (cons (leaf-node obj) children))
                                     tree)
                          t)))))))
    (if (null tree)
        ;; Create a new tree
        (values (leaf-node obj) t)
        ;; Traverse the tree to insert a new node
        (%go #'lens:empty))))

(sera:-> find-node (tree t metric)
         (values function boolean &optional))
(defun find-node (tree obj distance)
  "Return a lens to a node which contains the object. The second
returned value is @c(t) is the node was found and @c(nil)
otherwise. This function ignores invisible nodes."
  (labels ((%go (lens)
             (let* ((node (lens:view lens tree))
                    (value (node-value node))
                    (dist (funcall distance obj value)))
               (if (zerop dist)
                   (if (node-visible node)
                       (values lens t)
                       (values #'lens:empty nil))
                   (let ((child (find-if
                                 (lambda (child)
                                   (= dist (funcall distance value (node-value child))))
                                 (node-children node))))
                     (if child (%go (lens:compose lens #'children (item child)))
                         (values #'lens:empty nil)))))))
    (%go #'lens:empty)))

(sera:-> delete (tree t metric)
         (values tree boolean &optional))
(defun delete (tree obj distance)
    "Delete an object from the tree. An empty tree is designated by
@c(nil). This operation does not modify its arguments and returns a
new tree instead. The second value is @c(t) if deletion was
successfull and @c(nil) otherwise (when the object is absent from the
tree). An integer-valued metric @c(distance) on objects in the tree
must be supplied. It must be the same metric as the one used during
creation of the tree.

NB: BK-trees @b(do not support deletion) of the root node. To overcome
this restriction and for the sake of code uniformity, a node
corresponding to @c(obj) is not really deleted, but is
\"shadowed\". If you really need to delete it permanently, you need to
rebuild the tree using @c(faltten) and @c(insert). Patches for removal
of non-root nodes are welcome."
  (multiple-value-bind (lens foundp)
      (find-node tree obj distance)
    (if foundp
        (values (lens:set (lens:compose lens #'visible) nil tree) t)
        (values tree nil))))

(sera:-> nearest-neighbors (tree t metric)
         (values list alex:non-negative-fixnum &optional))
(defun nearest-neighbors (tree obj distance)
  "Find all elements in the tree which have the least distance to
@c(obj) according to an integer-valued metric. The second returned
value is that distance. The metric must be the same as the one used
during creation of the tree."
  (declare (optimize (speed 3)))
  (labels ((%go (accum node)
             (let* ((closest-distance (cdr accum))
                    (value (node-value node))
                    (dist  (funcall distance value obj))
                    (accum (if (node-visible node)
                               (cond
                                ((< dist closest-distance)
                                 (cons (cons value nil)         dist))
                                ((= dist closest-distance)
                                 (cons (cons value (car accum)) dist))
                                (t accum))
                               accum)))
               (declare (type alex:non-negative-fixnum closest-distance))
               (flet ((%%go (accum child)
                        (if (<= (abs (- dist (funcall distance value (node-value child))))
                                closest-distance)
                            (%go accum child) accum)))
                 (reduce #'%%go (node-children node) :initial-value accum)))))
    (if tree 
        (let ((results (%go (cons nil most-positive-fixnum) tree)))
          (values (car results) (cdr results)))
        (values nil most-positive-fixnum))))

(sera:-> nearest-neighbor (tree t metric)
         (values t alex:non-negative-fixnum &optional))
(defun nearest-neighbor (tree obj distance)
  "Like @c(nearest-neighbors), but returns only one (arbitrary)
element and is faster."
  (declare (optimize (speed 3)))
  (labels ((%go (accum node)
             (let* ((closest-distance (cdr accum))
                    (value (node-value node))
                    (dist  (funcall distance value obj))
                    (accum (if (and (node-visible node)
                                    (< dist closest-distance))
                               (cons value dist) accum)))
               (declare (type alex:non-negative-fixnum closest-distance))
               (flet ((%%go (accum child)
                        (if (< (abs (- dist (funcall distance value (node-value child))))
                               closest-distance)
                            (%go accum child) accum)))
                 (reduce #'%%go (node-children node) :initial-value accum)))))
    (if tree 
        (let ((results (%go (cons nil most-positive-fixnum) tree)))
          (values (car results) (cdr results)))
        (values nil most-positive-fixnum))))


(sera:-> items-in-ball (tree t alex:non-negative-fixnum metric)
         (values list &optional))
(defun items-in-ball (tree obj dist distance)
  "Return a list of elements from the tree whose distance to @c(obj)
is less or equal to @c(dist) according to an integer-valued metric
@c(distance). It must be the same metric as the one used during
creation of the tree."
  (declare (optimize (speed 3)))
  (labels ((%go (accum node)
             (let* ((value (node-value node))
                    (d (funcall distance obj value))
                    (accum (if (and (node-visible node)
                                    (<= d dist))
                               (cons value accum) accum)))
               (flet ((%%go (accum child)
                        (if (<= (abs (- d (funcall distance value (node-value child)))) dist)
                            (%go accum child) accum)))
                 (declare (dynamic-extent #'%%go))
                 (reduce #'%%go (node-children node) :initial-value accum)))))
    (declare (dynamic-extent #'%go))
    (if tree (%go nil tree))))

(sera:-> belongs-to-set-p (tree t metric)
         (values boolean &optional))
(defun belongs-to-set-p (tree elem distance)
  "Test if @c(elem) belongs to the tree. @c(distance) must be the same
metric as the one used during creation of the tree."
  (declare (optimize (speed 3)))
  (not (null (items-in-ball tree elem 0 distance))))

(sera:-> flatten (tree)
         (values list &optional))
(defun flatten (tree)
  "Return all elements of the tree as a list."
  (declare (optimize (speed 3)))
  (labels ((%go (accum node)
             (reduce #'%go (node-children node)
                     :initial-value
                     (if (node-visible node)
                         (cons (node-value node) accum)
                         accum))))
    (if tree (%go nil tree))))

;; Utility stuff & testing

(defun sets-equal-p (s1 s2 &key (test #'equalp))
  (and (subsetp s1 s2 :test test)
       (subsetp s2 s1 :test test)))

(sera:-> trees-equal-p (tree tree
                             &key (:test (sera:-> (t t) (values boolean &optional))))
         (values boolean &optional))
(defun trees-equal-p (t1 t2 &key (test #'equalp))
  "Test if two trees are equal."
  (declare (optimize (speed 3)))
  (or (and (null t1) (null t2))
      (and (funcall test (node-value t1) (node-value t2))
           (eql (node-visible t1) (node-visible t2))
           (sets-equal-p (node-children t1) (node-children t2)
                         :test #'trees-equal-p))))
