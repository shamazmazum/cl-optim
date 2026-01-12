(defpackage cl-optim
  (:use #:cl)
  (:shadow #:cl #:vector)
  (:local-nicknames (#:sera #:serapeum)
                    (#:alex #:alexandria)
                    (#:em   #:entzauberte-matrices))
  (:export
   ;; Gradient based
   #:gradient-descent
   #:gradient-descent-momentum
   #:nag
   #:adam

   ;; Gradient based with Hessian approximation
   #:bfgs #:backtracking-options

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
