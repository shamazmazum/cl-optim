(declaim (optimize (speed 3)))

;; Differentiable
(in-package :cl-optim-tests-fn-diff)

(defun rosenbrock (list)
  (destructuring-bind (x y)
      list
    (declare (type dual x y))
    (+ (expt (- 2 x) 2)
       (* 100 (expt (- y (expt x 2)) 2)))))

(defun paraboloid (list)
  (destructuring-bind (x y)
      list
    (declare (type dual x y))
    (+ (expt x 2)
       (expt y 2))))

(defun booth (list)
  (destructuring-bind (x y)
      list
    (declare (type dual x y))
    (+ (expt (+ x (* 2 y) -7) 2)
       (expt (+ (* 2 x) y -5) 2))))

(defun hills (x)
  (declare (type dual x))
  (+ (expt (- x 4) 2)
     (* 10 (expt (sin x) 2))))

;; Not differentiable
(in-package :cl-optim-tests-fn)

(defun noise-sinc (x)
  (declare (type single-float x))
  (let ((x-shift (- x 4)))
    (- (random 0.2)
       (if (zerop x-shift) 1.0
           (/ (sin (* 5 x-shift)) x-shift)))))
