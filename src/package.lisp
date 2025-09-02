(defpackage cl-optim
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria))
  (:export
   ;; Gradient based
   #:gradient-descent
   #:gradient-descent-momentum
   #:nag
   #:adam

   ;; Gradient based with Hessian approximation
   #:bfgs #:bfgs/magicl #:backtracking-options

   ;; Least squares
   #:linear-least-squares
   #:linear-irls
   #:*default-linear-irls-options*

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
   #:*default-backtracking-options*

   ;; Run summary (simulated annealing)
   #:simulated-annealing-summary
   #:summary-temperature
   #:summary-iterations
   #:summary-minimizing-steps
   #:summary-maximizing-steps
   #:summary-rejected-steps))
