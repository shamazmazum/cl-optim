(in-package :cl-optim)

(declaim (type alex:positive-double-float *η*))
(defparameter *η* 5d-4
  "Default descent rate for gradient descent algorithms.")

(declaim (type single-unit-range *β2*))
(defparameter *β2* 0.999d0
  "Default parameter for calculating the second momentum of the
gradient in Adam.")

(declaim (type alex:positive-double-float *ε*))
(defparameter *ε* 1d-3
  "Exit criterion for gradient descent algorithms.")

(declaim (type single-unit-range *β1*))
(defparameter *β1* 0.9d0
  "Default parameter for calculating the first momentum of the
gradient in algorithms with momentum.")

(declaim (type alex:positive-fixnum *max-iterations*))
(defparameter *max-iterations* 10000
  "Maximal allowed number of iterations.")

(sera:-> every-magniude-<
         (list double-float)
         (values boolean &optional))
(defun every-magnitude-< (xs x)
  (declare (optimize (speed 3))
           (type double-float x))
  (every
   (lambda (y)
     (declare (type double-float y))
     (< (abs y) x))
   xs))

(sera:-> df--
         (double-float double-float)
         (values double-float &optional))
(declaim (inline df--))
(defun df-- (x y)
  (- x y))

;; Gradient descent
(sera:-> gradient-descent
         (differentiable-multivariate
          list
          &key (:η              alex:positive-double-float)
               (:ε              alex:positive-double-float)
               (:max-iterations alex:positive-fixnum))
         (values list fixnum &optional))
(defun gradient-descent (function start-point
                         &key
                           (η              *η*)
                           (ε              *ε*)
                           (max-iterations *max-iterations*))
  "Find minimum of a function using vanilla gradient descent.

@c(function) takes a list of values of type @c(cl-forward-diff:dual)
and returns one value of type @c(cl-forward-diff:dual) (see
documentation for cl-forward-diff). @c(start-point) must be a list of
double floats and serves as a starting point for the algorithm. @c(η)
controls how fast the algorithm follows the gradient of the function.

The algorithms exists when either a number of iterations exceeds
@c(max-iterations) or absolute value of every gradient component is
less than @c(ε)."
  (declare (optimize (speed 3)))
  (labels ((descend (iteration x)
             (declare (type alex:non-negative-fixnum iteration))
             (let ((gradient (ad-multivariate function x)))
               (if (or (= iteration max-iterations)
                       (every-magnitude-< gradient ε))
                   (values x iteration)
                   (descend (1+ iteration)
                            (mapcar
                             (lambda (x dx)
                               (declare (type double-float x dx))
                               (- x (* η dx)))
                             x gradient))))))
    (descend 0 start-point)))

;; Gradient descent with momentum
(sera:-> gradient-descent-momentum
         (differentiable-multivariate
          list
          &key (:η              alex:positive-double-float)
               (:ε              alex:positive-double-float)
               (:β1             single-unit-range)
               (:max-iterations alex:positive-fixnum))
         (values list fixnum &optional))
(defun gradient-descent-momentum (function start-point
                                  &key
                                    (η              *η*)
                                    (ε              *ε*)
                                    (β1             *β1*)
                                    (max-iterations *max-iterations*))
  "Find minimum of a function using gradient descent with momentum.

@c(β1) controls how fast the momentum decreases on a flat surface. For
a description of other parameters, see @c(gradient-descent).

The algorithms exists when either a number of iterations exceeds
@c(max-iterations) or absolute value of every gradient component and
momentum component is less than @c(ε)."
  (declare (optimize (speed 3)))
  (labels ((descend (iteration x momentum)
             (declare (type alex:non-negative-fixnum iteration))
             (let ((gradient (ad-multivariate function x)))
               (if (or (= iteration max-iterations)
                       (and
                        (every-magnitude-< gradient ε)
                        (every-magnitude-< momentum ε)))
                   (values x iteration)
                   (descend (1+ iteration)
                            (mapcar #'df-- x momentum)
                            (mapcar
                             (lambda (m dx)
                               (declare (type double-float m dx))
                               (+ (* β1 m) (* η dx)))
                             momentum gradient))))))
    (descend
     0 start-point
     (loop repeat (length start-point) collect 0d0))))

;; NAG
(sera:-> nag
         (differentiable-multivariate
          list
          &key (:η              alex:positive-double-float)
               (:ε              alex:positive-double-float)
               (:β1             single-unit-range)
               (:max-iterations alex:positive-fixnum))
         (values list fixnum &optional))
(defun nag (function start-point
            &key
              (η              *η*)
              (ε              *ε*)
              (β1             *β1*)
              (max-iterations *max-iterations*))
  "Find minimum of a function using Nesterov's advanced gradient
descent algorithm. For a description of parameters see
@c(gradient-descent) and @c(gradient-descent-momentum).

Exit criterion is the same as in @c(gradient-descent-momentum)."
  (declare (optimize (speed 3)))
  (labels ((descend (iteration x momentum)
             (declare (type alex:non-negative-fixnum iteration))
             (let ((gradient (ad-multivariate function x)))
               (if (or (= iteration max-iterations)
                       (and
                        (every-magnitude-< gradient ε)
                        (every-magnitude-< momentum ε)))
                   (values x iteration)
                   (let ((gradient-look-ahead
                          (ad-multivariate
                           function (mapcar
                                     (lambda (x m)
                                       (declare (type double-float x m))
                                       (- x (* β1 m)))
                                     x momentum))))
                     (descend (1+ iteration)
                              (mapcar #'df-- x momentum)
                              (mapcar
                               (lambda (m dx)
                                 (declare (type double-float m dx))
                                 (+ (* β1 m) (* η dx)))
                               momentum gradient-look-ahead)))))))
    (descend
     0 start-point
     (loop repeat (length start-point) collect 0d0))))

;; Adam
(sera:-> adam
         (differentiable-multivariate
          list
          &key (:η              alex:positive-double-float)
               (:ε              alex:positive-double-float)
               (:β1             single-unit-range)
               (:β2             single-unit-range)
               (:max-iterations alex:positive-fixnum))
         (values list fixnum &optional))
(defun adam (function start-point
             &key
               (η              *η*)
               (ε              *ε*)
               (β1             *β1*)
               (β2             *β2*)
               (max-iterations *max-iterations*))
  "Find minimum of a function using Adam. @c(β2) is a parameter used
in calculation of the second momentum of the gradient. For a
description of other parameters see @c(gradient-descent) and
@c(gradient-descent-momentum).

Exit criterion is the same as in @c(gradient-descent-momentum)."
  (declare (optimize (speed 3)))
  (labels ((corrent-value (value coeff iteration)
             (declare (type alex:non-negative-fixnum iteration)
                      (type double-float coeff))
             (mapcar (lambda (v)
                       (declare (type double-float v))
                       (/ v (- 1 (expt coeff (1+ iteration)))))
                     value))
           (descend (iteration x momentum momentum2)
             (declare (type alex:non-negative-fixnum iteration))
             (let ((gradient (ad-multivariate function x)))
               (if (or (= iteration max-iterations)
                       (and
                        (every-magnitude-< gradient ε)
                        (every-magnitude-< momentum ε)))
                   (values x iteration)
                   (let ((momentum2 (mapcar
                                     (lambda (m2 g)
                                       (declare (type double-float m2 g))
                                       (+ (* β2 m2)
                                          (* (- 1 β2) (expt g 2))))
                                     momentum2 gradient))
                         (momentum (mapcar
                                    (lambda (m g)
                                      (declare (type double-float m g))
                                      (+ (* β1 m) (* (- 1 β1) g)))
                                    momentum gradient)))
                     (descend (1+ iteration)
                              (mapcar (lambda (x m m2)
                                        (declare (type double-float x m)
                                                 (type alex:non-negative-double-float m2))
                                        (- x (/ (* η m)
                                                (+ (sqrt m2) 1f-8))))
                                      x
                                      (corrent-value momentum  β1 iteration)
                                      (corrent-value momentum2 β2 iteration))
                              momentum momentum2))))))
    (let ((zeros (loop repeat (length start-point) collect 0d0)))
      (descend 0 start-point zeros zeros))))