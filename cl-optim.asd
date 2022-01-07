(defsystem :cl-optim
  :name :cl-optim
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Optimization of differentiable functions in CL"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "optimize"))
  :depends-on (:cl-forward-diff :serapeum :alexandria))
