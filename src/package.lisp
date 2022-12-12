(defpackage cl-optim
  (:use #:cl)
  (:local-nicknames (:sera :serapeum)
                    (:alex :alexandria))
  (:import-from
   #:cl-forward-diff
   #:dual #:differentiable-multivariate #:ad-multivariate)
  (:export
   ;; Gradient based
   #:gradient-descent
   #:gradient-descent-momentum
   #:nag
   #:adam

   ;; Heuristics
   #:simulated-annealing
   #:exponential-cooldown
   #:normal-neighborhood

   ;; Parameters
   #:*η*
   #:*ε*
   #:*β1*
   #:*β2*
   #:*max-iterations*
   #:*final-temperature*

   ;; Run summary (simulated annealing)
   #:simulated-annealing-summary
   #:summary-temperature
   #:summary-iterations
   #:summary-minimizing-steps
   #:summary-maximizing-steps
   #:summary-rejected-steps))
