(in-package :cl-optim)

;; Default values
(declaim (type alex:non-negative-single-float *final-temperature*))
(defparameter *final-temperature* 1f-4
  "Final temperature for the simulated annealing method")

;; Simulated annealing summary

(defstruct (simulated-annealing-summary (:conc-name summary-))
  "Structure containing summary of optimization performed by
@c(simulated-annealing)."
  (temperature      0f0 :type alex:non-negative-single-float)
  (iterations       0   :type alex:non-negative-fixnum)
  (minimizing-steps 0   :type alex:non-negative-fixnum)
  (rejected-steps   0   :type alex:non-negative-fixnum))

(defun summary-maximizing-steps (summary)
  "Get number of accepted steps"
  (- (summary-iterations summary)
     (summary-minimizing-steps summary)
     (summary-rejected-steps summary)))

;; Neighborhood generation

(sera:-> standard-normal () (values single-float &optional))
(defun standard-normal ()
  (declare (optimize (speed 3)))
  (let ((x1 (random 1.0))
        (x2 (random 1.0)))
    (if (zerop x1)
        (standard-normal)
        (* (sqrt (* -2.0 (log x1)))
           (cos (* 2 (float pi 0f0) x2))))))

(sera:-> normal
         (single-float alex:non-negative-single-float)
         (values single-float &optional))
(defun normal (μ σ)
  (declare (optimize (speed 3))
           (type single-float μ)
           (type alex:non-negative-single-float σ))
  (+ (* σ (standard-normal)) μ))

(sera:-> normal-neighborhood
         (alex:non-negative-single-float)
         (values neighborhood-function &optional))
(defun normal-neighborhood (σ)
  "Create a neighborhood function which calculates a new candidate by
adding a vector of independent random values with distribution
@c(N(0, σ)) to the current candidate."
  (declare (optimize (speed 3)))
  (lambda (xs)
    (mapcar
     (lambda (x)
       (declare (type single-float x))
       (+ x (normal 0.0 σ)))
     xs)))

;; Cooldown schedules

(sera:-> exponential-cooldown
         (single-unit-range)
         (values cooldown-function &optional))
(defun exponential-cooldown (λ)
  "Create exponential cooldown schedule which multiplies the
temperature by a parameter @c(0 < λ < 1)"
  (declare (optimize (speed 3))
           (type single-unit-range λ))
  (lambda (temp value)
    (declare (type alex:non-negative-single-float temp)
             (ignore value))
    (* temp λ)))

;; Simulated annealing

(defun simulated-annealing (function start-point
                            &key
                              (max-iterations      most-positive-fixnum)
                              (initial-temperature 1.0)
                              (final-temperature   *final-temperature*)
                              (cooldown            (exponential-cooldown 0.999))
                              (next                (normal-neighborhood  1.0)))
  "Find a minimum of @c(function) using simulated annealing
algorithm. The function must take a list of @c(single-float) numbers
and return a @c(single-float) number, in other words this method does
not require the function to be differentiable. @c(start-point) is a
list of @c(single-float) numbers which serves as a starting point for
a search. The algorithm exists when either the number of iterations
exceeds @c(max-iterations) or the temperature drops below
@c(final-temperature).

@c(cooldown) may be used to explicitly specify a cooldown
schedule. The cooldown schedule must be a function which takes the
current temperature and value of @c(function) at the current point and
returns new temperature.

@c(next) is a function which takes a point in optimization space (as a
list of single float numbers) and returns a new point as a new
candidate for a minimum.

This function returns the found minimum and an object of type
@c(simulated-annealing-summary) which contains some statistics like a
total number of iterations, the final temperature and so on."
  (declare (optimize (speed 3))
           (type optimizable-function function)
           (type cooldown-function cooldown)
           (type neighborhood-function next)
           (type alex:non-negative-single-float final-temperature)
           (type alex:positive-fixnum max-iterations))
  (let ((scaling (abs (funcall function start-point))))
    (labels ((annealing-step (summary point)
               (let ((temperature  (summary-temperature  summary))
                     (iterations   (summary-iterations   summary)))
                 (if (or (< temperature final-temperature)
                         (> iterations  max-iterations))
                     (values point summary)
                     (let* ((new-point     (funcall next     point))
                            (current-value (funcall function point))
                            (new-value     (funcall function new-point))
                            (minimizing (< new-value current-value))
                            (accept (or minimizing
                                        (< (random 1.0) 
                                           (exp (- (/ (- new-value current-value)
                                                      scaling temperature)))))))
                       (annealing-step (make-simulated-annealing-summary
                                        :temperature      (if accept
                                                              (funcall cooldown temperature new-value)
                                                              temperature)
                                        :iterations       (1+ iterations)
                                        :minimizing-steps (+ (summary-minimizing-steps summary)
                                                             (if minimizing 1 0))
                                        :rejected-steps   (+ (summary-rejected-steps summary)
                                                             (if accept 0 1)))
                                       (if accept new-point point)))))))

      (annealing-step
       (make-simulated-annealing-summary :temperature initial-temperature)
       start-point))))
