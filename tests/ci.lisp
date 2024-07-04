(defun do-all()
  (ql:quickload :pure-spatial-trees/tests)
  (uiop:quit
   (if (uiop:call-function "pure-spatial-trees/tests:run-tests")
       0 1)))

(do-all)
