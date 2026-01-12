(in-package :cl-optim)

(sera:-> eval-function (doubles->double (vector double-float))
         (values double-float &optional))
(defun eval-function (f x)
  (declare (optimize (speed 3)))
  (funcall f x))

(sera:-> eval-gradient (doubles->doubles (vector double-float))
         (values (vector double-float) &optional))
(defun eval-gradient (f x)
  (declare (optimize (speed 3)))
  (funcall f x))

;; Backtracking line search
(sera:defconstructor backtracking-options
  "Create backtracking line search options. @c(Η) is an initial
step in the search direction. @c(Τ) is a multiplier of the seach step
at each iteraction. @c(C) is a constant in Armijo rule. @c(MAX-STEPS)
is a maximal number of steps in the backtracking search."
  (η double-float)
  (τ double-float)
  (c double-float)
  (max-steps alex:positive-fixnum))

(defparameter *default-backtracking-options*
  (backtracking-options 1d0 9d-1 5d-1 120)
  "Default options for the backtracking line search algorithm.")

(sera:-> backtracking-search
         (doubles->double
          (vector double-float)
          (vector double-float)
          double-float
          &key
          (:options backtracking-options))
         (values (vector double-float) &optional))
(defun backtracking-search (f x direction dot
                            &key (options *default-backtracking-options*))
  "Perform a line search using the backtracking algorithm. @c(f) is a
function to be minimized, @c(x) is a starting point for the search,
@c(direction) is a search direction. A dot product of the gradient and
the search direction is supplied in @c(dot)."
  (declare (optimize (speed 3)))
  (let ((%t (* (backtracking-options-c options) dot))
        (function-at-x (eval-function f x)))
    (labels ((%search (a step)
               (declare (type double-float a)
                        (type alex:non-negative-fixnum step))
               (let ((new-x (em:sub x (em:scale direction a))))
                 (if (or (= step (backtracking-options-max-steps options))
                         (>= (- function-at-x (eval-function f new-x))
                             (* a %t)))
                     new-x (%search (* a (backtracking-options-τ options))
                                    (1+ step))))))
      (%search (backtracking-options-η options) 0))))

;; BFGS algorithm
(sera:-> bfgs (doubles->double doubles->doubles
               (vector double-float) &key
               (:ε double-float)
               (:max-steps alex:positive-fixnum)
               (:backtracking-options backtracking-options))
         (values (vector double-float)
                 (matrix double-float)
                 alex:non-negative-fixnum &optional))
(defun bfgs (f g initial-approximation
             &key
               (backtracking-options *default-backtracking-options*)
               (ε 1d-6)
               (max-steps 10000))
  "Minimize a function using BFGS with the backtracking line search
algorithm. @c(F) is a function to be minimized, @c(G) is its gradient
function and @c(initial-approximation) is a vector which contains a
starting point for the search. The search stops when either the number
of steps exceeds @c(max-steps) or L²-norm of the gradient is below
@c(ε). This function returns a vector which minimizes @c(F), an
approximation of Hessian at this point and a total number of steps."
  (declare (optimize (speed 3)))
  (let* ((nvars (length initial-approximation))
         (id (eye nvars)))
    (labels ((%iteration (x h step)
               (declare (type alex:non-negative-fixnum step))
               (let ((grad (eval-gradient g x)))
                 (if (or (< (em:norm grad) ε)
                         (= step max-steps))
                     (values x (em:invert h) step)
                     (let* ((anti-direction (mult-mv h grad))
                            (dot (em:dot anti-direction grad))
                            (direction-ok-p (> dot 0))
                            (anti-direction (if direction-ok-p anti-direction grad))
                            (h (if direction-ok-p h id))
                            (dot (if direction-ok-p
                                     dot (em:dot anti-direction grad)))
                            (new-x (backtracking-search f x anti-direction dot
                                                        :options backtracking-options))
                            (diff-x (em:sub new-x x))
                            (diff-grad (em:sub (eval-gradient g new-x) grad))

                            (tmp1 (em:dot diff-x diff-grad))
                            (tmp2 (em:dot diff-grad (mult-mv h diff-grad)))

                            (diff-h (em:sub
                                     (em:scale
                                      (column-*-row diff-x diff-x)
                                      (/ (+ tmp1 tmp2) (expt tmp1 2)))
                                     (em:scale
                                      (em:add
                                       (em:mult h (column-*-row diff-grad diff-x))
                                       (em:mult (column-*-row diff-x diff-grad) h))
                                      (/ tmp1)))))
                       (%iteration new-x (em:add h diff-h) (1+ step)))))))
      (%iteration initial-approximation id 0))))
