(in-package :cl-optim)

;; Helper functions
(sera:-> %dot (magicl:vector/double-float
               magicl:vector/double-float)
         (values double-float &optional))
(declaim (inline %dot))
(defun %dot (v1 v2)
  (magicl:dot v1 v2))

(sera:-> %norm (magicl:vector/double-float)
         (values double-float &optional))
(declaim (inline %norm))
(defun %norm (v)
  (magicl:norm v))

(sera:-> %storage (magicl:vector/double-float)
         (values (%vector double-float) &optional))
(declaim (inline %storage))
(defun %storage (xs)
  (magicl::storage xs))

(sera:-> column-*-row (magicl:vector/double-float
                       magicl:vector/double-float)
         (values magicl:matrix/double-float &optional))
(declaim (inline column-*-row))
(defun column-*-row (c r)
  (magicl:@
   (magicl:vector->column-matrix c)
   (magicl:vector->row-matrix    r)))

(sera:-> eval-function (diff:differentiable-multivariate
                        magicl:vector/double-float)
         (values double-float &optional))
(defun eval-function (function xs)
  (declare (optimize (speed 3)))
  (diff:dual-realpart
   (funcall function
            (map '(vector diff:dual)
                 #'diff:make-dual
                 (%storage xs)))))

(sera:-> eval-gradient (diff:differentiable-multivariate
                        magicl:vector/double-float)
         (values magicl:vector/double-float &optional))
(defun eval-gradient (function xs)
  (declare (optimize (speed 3)))
  (magicl:make-tensor
   'magicl:vector/double-float (magicl:shape xs)
   :storage (diff:ad-multivariate function (%storage xs))))

;; Backtracking line search
(sera:defconstructor backtracking-options
  (η double-float)
  (τ double-float)
  (c double-float)
  (max-steps alex:positive-fixnum))

(setf (documentation 'backtracking-options 'function)
      "Create backtracking line search options. @c(Η) is an initial
step in the search direction. @c(Τ) is a multiplier of the seach step
at each iteraction. @c(C) is a constant in Armijo rule. @c(MAX-STEPS)
is a maximal number of steps in the backtracking search.")

(defparameter *default-backtracking-options*
  (backtracking-options 1d0 9d-1 5d-1 120)
  "Default options for the backtracking line search algorithm.")

(sera:-> backtracking-search
         (diff:differentiable-multivariate
          magicl:vector/double-float
          magicl:vector/double-float
          &key
          (:dot     double-float)
          (:options backtracking-options)
          (:grad    magicl:vector/double-float))
         (values magicl:vector/double-float &optional))
(defun backtracking-search (function x direction
                            &key (options *default-backtracking-options*)
                              (grad (eval-gradient function x))
                              (dot (%dot grad direction)))
  "Perform a line search using the backtracking
algorithm. @c(Function) is a function to be minimized, @c(x) is a
starting point for the search, @c(direction) is a search
direction. Optionally, a precomputed gradient of the function at the
point @c(x) can be supplied in @c(grad) and a dot product of the
gradient and the search direction can be supplied in @c(dot)."
  (declare (optimize (speed 3)))
  (assert (> dot 0))
  (let ((%t (* (backtracking-options-c options) dot))
        (function-at-x (eval-function function x)))
    (labels ((%search (a step)
               (declare (type double-float a)
                        (type alex:non-negative-fixnum step))
               (let ((new-x (magicl:.- x (magicl:scale direction a))))
                 (if (or (= step (backtracking-options-max-steps options))
                         (>= (- function-at-x (eval-function function new-x))
                             (* a %t)))
                     new-x (%search (* a (backtracking-options-τ options))
                                    (1+ step))))))
      (%search (backtracking-options-η options) 0))))


;; BFGS algorithm
(sera:-> bfgs
         (diff:differentiable-multivariate
          magicl:vector/double-float
          &key
          (:ε double-float)
          (:max-steps alex:positive-fixnum)
          (:backtracking-options backtracking-options))
         (values magicl:vector/double-float
                 magicl:matrix/double-float
                 alex:non-negative-fixnum &optional))
(defun bfgs (function initial-approximation
             &key
               (backtracking-options *default-backtracking-options*)
               (ε 1d-6)
               (max-steps 10000))
  "Minimize a function using BFGS with backtracking line search
algorithm. @c(Function) is a differentiable function to be minimized
and @c(initial-approximation) is a MAGICL vector which contains a
starting point for the search. The search stops when either the number
of steps exceeds @c(max-steps) or L²-norm of the gradient is below
@c(ε). This function returns a vector which minimizes @c(function), an
approximation of Hessian at this point and a total number of steps."
  (declare (optimize (speed 3)))
  (let* ((nvars (car (magicl:shape initial-approximation)))
         (id (magicl:eye (list nvars nvars) :type 'double-float)))
    (labels ((%iteration (x h step)
               (declare (type alex:non-negative-fixnum step))
               (let ((grad (eval-gradient function x)))
                 (if (or (< (%norm grad) ε)
                         (= step max-steps))
                     (values x (magicl:inv h) step)
                     (let* ((anti-direction (magicl:@ h grad))
                            (dot (%dot anti-direction grad))
                            (direction-ok-p (> dot 0))
                            (anti-direction (if direction-ok-p anti-direction grad))
                            (h (if direction-ok-p h id))
                            (dot (if direction-ok-p
                                     dot (%dot anti-direction grad)))
                            (new-x (backtracking-search function x anti-direction
                                                        :options backtracking-options
                                                        :grad grad
                                                        :dot dot))
                            (diff-x (magicl:.- new-x x))
                            (diff-grad (magicl:.- (eval-gradient function new-x) grad))

                            (tmp1 (%dot diff-x diff-grad))
                            (tmp2 (%dot diff-grad (magicl:@ h diff-grad)))

                            (diff-h (magicl:.-
                                     (magicl:scale
                                      (column-*-row diff-x diff-x)
                                      (/ (+ tmp1 tmp2) (expt tmp1 2)))
                                     (magicl:scale
                                      (magicl:.+
                                       (magicl:@ h (column-*-row diff-grad diff-x))
                                       (magicl:@ (column-*-row diff-x diff-grad) h))
                                      (/ tmp1)))))

                       (%iteration new-x (magicl:.+ h diff-h) (1+ step)))))))
      (%iteration initial-approximation id 0))))
