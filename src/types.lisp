(in-package :cl-optim)

(deftype single-unit-range () '(single-float 0f0 1f0))

;; For non-differentiable functions (differentiable have their own
;; type in cl-forward-diff).

(deftype optimizable-function ()
  '(sera:-> (list) (values single-float &optional)))

;; Types for simulated annealing

(deftype cooldown-function ()
  '(sera:->
    (alex:non-negative-single-float single-float)
    (values alex:non-negative-single-float &optional)))

(deftype neighborhood-function ()
  '(sera:-> (list) (values list &optional)))
