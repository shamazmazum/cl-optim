(defpackage cl-optim-tests-fn-diff
  (:use #:cl)
  #.(cl-forward-diff:shadowing-import-math)
  (:local-nicknames (#:si   #:stateless-iterators)
                    (#:diff #:cl-forward-diff))
  (:export #:rosenbrock #:rosenbrock-grad
           #:paraboloid #:paraboloid-grad
           #:booth      #:booth-grad
           #:hills      #:hills-grad))

(defpackage cl-optim-tests-fn
  (:use #:cl)
  (:export #:noise-sinc))

(defpackage cl-optim-tests
  (:use #:cl
        #:fiveam
        #:alexandria
        #:cl-optim
        #:cl-optim-tests-fn
        #:cl-optim-tests-fn-diff)
  (:import-from #:cl-forward-diff
                #:to-doubles)
  (:export #:run-tests))
