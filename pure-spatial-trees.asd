(defsystem :pure-spatial-trees
    :name :pure-spatial-trees
    :version "0.2.2"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Pure functional spatial trees"
    :license "2-clause BSD"
    :serial t
    :pathname "src"
    :class :package-inferred-system
    :depends-on (:alexandria
                 :serapeum
                 :picolens
                 :pure-spatial-trees/bk)
    :in-order-to ((test-op (load-op "pure-spatial-trees/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (uiop:symbol-call :pure-spatial-trees/tests '#:run-tests)))

(defsystem :pure-spatial-trees/tests
    :name :pure-spatial-trees/tests
    :version "0.2.2"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :pathname "tests"
    :components ((:file "package")
                 (:file "tests"))
    :depends-on (:fiveam :pure-spatial-trees))
