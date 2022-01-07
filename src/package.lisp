(defpackage cl-optim
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria))
  (:import-from
   #:cl-forward-diff
   #:dual #:differentiable-multivariate #:ad-multivariate)
  (:export #:gradient-descent
           #:gradient-descent-momentum
           #:nag))
