(in-package :cl-optim)

(deftype %vector (type) `(simple-array ,type (*)))
(deftype single-unit-range () '(double-float 0d0 1d0))
(deftype doubles->double ()
  '(sera:-> ((%vector double-float)) (values double-float &optional)))
(deftype doubles->doubles ()
  '(sera:-> ((%vector double-float)) (values (%vector double-float) &optional)))

;; Types for simulated annealing

(deftype cooldown-function ()
  '(sera:->
    (alex:non-negative-double-float double-float)
    (values alex:non-negative-double-float &optional)))
