(defpackage cl-optim-tests-fn
  (:use #:cl)
  (:import-from #:cl-forward-diff #:dual)
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:rosenbrock
           #:paraboloid
           #:booth
           #:hills))

(defpackage cl-optim-tests
  (:use #:cl
        #:fiveam
        #:alexandria
        #:cl-optim
        #:cl-optim-tests-fn)
  (:export #:run-tests))
