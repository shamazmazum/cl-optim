(in-package :cl-optim)

(declaim (type (single-float 0f0) *descent-rate*))
(defparameter *descent-rate* 5f-4
  "Default descent rate for gradient descent algorithms.")

(declaim (type (single-float 0f0) *epsilon*))
(defparameter *epsilon* 1f-3
  "Exit criterion for gradient descent algorithms.")

(declaim (type (single-float 0f0 1f0) *friction*))
(defparameter *friction* 0.9
  "Friction parameter for gradient descent algorithms with momentum.")

(declaim (type alex:positive-fixnum *max-iterations*))
(defparameter *max-iterations* 10000
  "Maximal allowed number of iterations.")

(sera:-> every-magniude-<
         (list single-float)
         (values boolean &optional))
(defun every-magnitude-< (xs x)
  (declare (optimize (speed 3))
           (type list xs)
           (type single-float x))
  (every
   (lambda (y)
     (declare (type single-float y))
     (< (abs y) x))
   xs))

(sera:-> sf--
         (single-float single-float)
         (values single-float &optional))
(declaim (inline sf--))
(defun sf-- (x y)
  (declare (type single-float x y))
  (- x y))

(sera:-> gradient-descent
         (differentiable-multivariate
          list
          &key (:descent-rate   (single-float 0f0))
               (:max-iterations alex:positive-fixnum)
               (:epsilon        (single-float 0f0)))
         (values list fixnum &optional))
(defun gradient-descent (function start-point
                         &key
                           (descent-rate   *descent-rate*)
                           (max-iterations *max-iterations*)
                           (epsilon        *epsilon*))
  "Find minimum of a function using vanilla gradient descent.

@c(function) takes a list of values of type @c(cl-forward-diff:dual)
and returns one value of type @c(cl-forward-diff:dual) (see
documentation for cl-forward-diff). @c(start-point) must be a list of
single floats and serves as a starting point for the
algorithm. @c(descent-rate) controls how fast the algorithm follows
the gradient of the function.

The algorithms exists when either a number of iterations exceeds
@c(max-iterations) or absolute value of every gradient component is
less than @c(epsilon)."
  (declare (optimize (speed 3))
           (type differentiable-multivariate function)
           (type list start-point)
           (type (single-float 0f0) descent-rate epsilon)
           (type alex:positive-fixnum max-iterations))
  (labels ((descend (iteration x)
             (declare (type alex:non-negative-fixnum iteration))
             (let ((gradient (ad-multivariate function x)))
               (if (or (= iteration max-iterations)
                       (every-magnitude-< gradient epsilon))
                   (values x iteration)
                   (descend (1+ iteration)
                            (mapcar
                             (lambda (x dx)
                               (declare (type single-float x dx))
                               (- x (* descent-rate dx)))
                             x gradient))))))
    (descend 0 start-point)))


(sera:-> gradient-descent-momentum
         (differentiable-multivariate
          list
          &key (:descent-rate   (single-float 0f0))
               (:max-iterations alex:positive-fixnum)
               (:epsilon        (single-float 0f0))
               (:friction       (single-float 0f0 1f0)))
         (values list fixnum &optional))
(defun gradient-descent-momentum (function start-point
                                  &key
                                    (descent-rate   *descent-rate*)
                                    (max-iterations *max-iterations*)
                                    (epsilon        *epsilon*)
                                    (friction       *friction*))
  "Find minimum of a function using gradient descent with momentum.

@c(friction) controls how fast the momentum decreases on a flat
surface. For a description of other parameters, see
@c(gradient-descent).

The algorithms exists when either a number of iterations exceeds
@c(max-iterations) or absolute value of every gradient component and
momentum component is less than @c(epsilon)."
  (declare (optimize (speed 3))
           (type differentiable-multivariate function)
           (type list start-point)
           (type (single-float 0f0) descent-rate epsilon)
           (type (single-float 0f0 1f0) friction)
           (type alex:positive-fixnum max-iterations))
  (labels ((descend (iteration x momentum)
             (declare (type alex:non-negative-fixnum iteration))
             (let ((gradient (ad-multivariate function x)))
               (if (or (= iteration max-iterations)
                       (and
                        (every-magnitude-< gradient epsilon)
                        (every-magnitude-< momentum epsilon)))
                   (values x iteration)
                   (descend (1+ iteration)
                            (mapcar #'sf-- x momentum)
                            (mapcar
                             (lambda (m dx)
                               (declare (type single-float m dx))
                               (+ (* friction m) (* descent-rate dx)))
                             momentum gradient))))))
    (descend
     0 start-point
     (loop repeat (length start-point) collect 0f0))))


(sera:-> nag
         (differentiable-multivariate
          list
          &key (:descent-rate   (single-float 0f0))
               (:max-iterations alex:positive-fixnum)
               (:epsilon        (single-float 0f0))
               (:friction       (single-float 0f0 1f0)))
         (values list fixnum &optional))
(defun nag (function start-point
            &key
              (descent-rate   *descent-rate*)
              (max-iterations *max-iterations*)
              (epsilon        *epsilon*)
              (friction       *friction*))
  "Find minimum of a function using Nesterov's advanced gradient
descent algorithm. For a description of parameters see
@c(gradient-descent) and @c(gradient-descent-momentum)."
  (declare (optimize (speed 3))
           (type differentiable-multivariate function)
           (type list start-point)
           (type (single-float 0f0) descent-rate epsilon)
           (type (single-float 0f0 1f0) friction)
           (type alex:positive-fixnum max-iterations))
  (labels ((descend (iteration x momentum)
             (declare (type alex:non-negative-fixnum iteration))
             (let ((gradient (ad-multivariate function x)))
               (if (or (= iteration max-iterations)
                       (and
                        (every-magnitude-< gradient epsilon)
                        (every-magnitude-< momentum epsilon)))
                   (values x iteration)
                   (let ((gradient-look-ahead
                          (ad-multivariate
                           function (mapcar
                                     (lambda (x m)
                                       (declare (type single-float x m))
                                       (- x (* friction m)))
                                     x momentum))))
                   (descend (1+ iteration)
                            (mapcar #'sf-- x momentum)
                            (mapcar
                             (lambda (m dx)
                               (declare (type single-float m dx))
                               (+ (* friction m) (* descent-rate dx)))
                             momentum gradient-look-ahead)))))))
    (descend
     0 start-point
     (loop repeat (length start-point) collect 0f0))))
