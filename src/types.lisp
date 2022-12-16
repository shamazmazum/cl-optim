(in-package :cl-optim)

(deftype single-unit-range () '(double-float 0d0 1d0))

;; For non-differentiable functions (differentiable have their own
;; type in cl-forward-diff).

(deftype optimizable-function ()
  '(sera:-> (list) (values double-float &optional)))

;; Types for simulated annealing

(deftype cooldown-function ()
  '(sera:->
    (alex:non-negative-double-float double-float)
    (values alex:non-negative-double-float &optional)))

(deftype neighborhood-function ()
  '(sera:-> (list) (values list &optional)))
