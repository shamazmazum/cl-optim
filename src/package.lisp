(defpackage cl-optim
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:diff #:cl-forward-diff)
                    (#:alex #:alexandria))
  (:export
   ;; Gradient based
   #:gradient-descent
   #:gradient-descent-momentum
   #:nag
   #:adam

   ;; Gradient based with Hessian approximation
   #:bfgs #:bfgs/magicl #:backtracking-options

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
