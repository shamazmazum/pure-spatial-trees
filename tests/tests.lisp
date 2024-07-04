(in-package :pure-spatial-trees/tests)

(defun run-tests ()
  (explain! (run 'bk)))

(sera:-> hamming-distance (simple-bit-vector simple-bit-vector)
         (values alex:non-negative-fixnum &optional))
(defun hamming-distance (v1 v2)
  (declare (optimize (speed 3)))
  (let ((v (map '(vector bit) #'logxor v1 v2)))
    (declare (dynamic-extent v))
    (reduce #'+ v)))

(defun random-vector (n state)
  (make-array n
              :element-type 'bit
              :initial-contents (loop repeat n collect (random 2 state))))

(defun random-list (n m state)
  (remove-duplicates
   (loop repeat n collect (random-vector m state))
   :test #'equalp))

(def-suite bk :description "BK-trees test suite")

(sera:-> nearest-neighbors-naïve (list t bk:metric)
         (values list &optional))
(defun nearest-neighbors-naïve (list obj distance)
  (declare (optimize (speed 3)))
  (let ((closest-dist most-positive-fixnum)
        closest-objs)
    (dolist (value list)
      (let ((dist (funcall distance obj value)))
        (cond
          ((< dist closest-dist)
           (setq closest-dist dist closest-objs (list value)))
          ((= dist closest-dist)
           (push value closest-objs)))))
    closest-objs))

(sera:-> items-in-ball-naïve (list t alex:non-negative-fixnum bk:metric)
         (values list &optional))
(defun items-in-ball-naïve (list obj dist distance)
  (loop for elem in list
        for d = (funcall distance obj elem)
        when (<= d dist) collect elem))

(in-suite bk)

(test insert
  (loop with state = (make-random-state t)
        repeat 100
        for stuff = (random-list 2000 20 state)
        for tree  = (reduce (lambda (tree obj) (bk:insert tree obj #'hamming-distance))
                            stuff :initial-value nil)
        do (is (bk:sets-equal-p stuff (bk:flatten tree)))))

(test nearest-neighbors
  (loop with state = (make-random-state t)
        repeat 500
        for obj   = (random-vector 20 state)
        for stuff = (random-list 10000 20 state)
        for tree  = (reduce (lambda (tree obj) (bk:insert tree obj #'hamming-distance))
                            stuff :initial-value nil)
        for s1 = (nearest-neighbors-naïve stuff obj #'hamming-distance)
        for s2 = (bk:nearest-neighbors tree obj #'hamming-distance)
        do
        (is (bk:sets-equal-p s1 s2))
        (is (member (bk:nearest-neighbor tree obj #'hamming-distance) s1))))

(test close-items
  (loop with state = (make-random-state t)
        repeat 500
        for obj   = (random-vector 20 state)
        for stuff = (random-list 10000 20 state)
        for dist  = (1+ (random 7))
        for tree  = (reduce (lambda (tree obj) (bk:insert tree obj #'hamming-distance))
                            stuff :initial-value nil)
        for s1 = (items-in-ball-naïve stuff obj dist #'hamming-distance)
        for s2 = (bk:items-in-ball tree obj dist #'hamming-distance)
        do (is (bk:sets-equal-p s1 s2))))

(test delete-insert
  (loop with state = (make-random-state t)
        repeat 500
        for stuff = (random-list 10000 20 state)
        for obj   = (nth (random (length stuff) state) stuff)
        for tree  = (reduce (lambda (tree obj) (bk:insert tree obj #'hamming-distance))
                            stuff :initial-value nil)
        do (is (bk:trees-equal-p
                (bk:insert
                 (bk:delete tree obj #'hamming-distance) obj #'hamming-distance)
                tree))))

(test insert-delete-search
  (loop with state = (make-random-state t)
        repeat 250
        for obj    = (random-vector 16 state)
        for istuff = (random-list 10000 16 state)
        for dstuff = (random-list 10000 16 state)
        for stuff  = (set-difference istuff dstuff :test #'equalp)
        for dist   = (1+ (random 7 state))
        for tree1  = (reduce (lambda (tree obj) (bk:insert tree obj #'hamming-distance))
                             istuff :initial-value nil)
        for tree2  = (reduce (lambda (tree obj) (bk:delete tree obj #'hamming-distance))
                             dstuff :initial-value tree1)
        for tree3  = (reduce (lambda (tree obj) (bk:insert tree obj #'hamming-distance))
                             stuff :initial-value nil)
        for s1 = (bk:items-in-ball tree2 obj dist #'hamming-distance)
        for s2 = (bk:items-in-ball tree3 obj dist #'hamming-distance)
        do (is (bk:sets-equal-p s1 s2))))
