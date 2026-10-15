(defpackage cl-optim/tests/fn-diff
  (:use #:cl)
  #.(cl-forward-diff:shadowing-import-math)
  (:local-nicknames (#:si   #:stateless-iterators)
                    (#:diff #:cl-forward-diff))
  (:export #:rosenbrock #:rosenbrock-grad
           #:paraboloid #:paraboloid-grad
           #:booth      #:booth-grad
           #:beale      #:beale-grad))

(defpackage cl-optim/tests/fn
  (:use #:cl)
  (:export #:noise-sinc))

(defpackage cl-optim/tests
  (:use #:cl
        #:fiveam
        #:alexandria
        #:approx
        #:cl-optim
        #:cl-optim/tests/fn
        #:cl-optim/tests/fn-diff)
  (:local-nicknames (#:em #:entzauberte-matrices))
  (:export #:run-tests))
