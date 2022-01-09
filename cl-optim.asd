(defsystem :cl-optim
  :name :cl-optim
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Optimization of differentiable functions in CL"
  :licence "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "optimize"))
  :depends-on (:cl-forward-diff :serapeum :alexandria)
  :in-order-to ((test-op (load-op "cl-optim/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-optim-tests  '#:run-tests)))

(defsystem :cl-optim/tests
  :name :cl-optim/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "functions")
               (:file "tests"))
  :depends-on (:fiveam :alexandria :cl-optim))
