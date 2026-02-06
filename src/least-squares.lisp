(in-package :cl-optim)

(sera:-> linear-least-squares-cost
    ((vector double-float)
     (vector double-float)
     (matrix double-float))
    (values double-float &optional))
(defun linear-least-squares-cost (βs ys xs)
  (let ((diff (em:sub (mult-mv xs βs) ys)))
    (em:dot diff diff)))

(declaim (inline lsqr-xs))
(defun lsqr-xs (xs length)
  (em:hstack
   (reduce #'cons xs
           :key (lambda (x)
                  (declare (type (vector double-float) x))
                  (em:vector->column-unsafe x))
           :initial-value
           (list (make-array (list length 1)
                             :element-type 'double-float
                             :initial-element 1d0))
           :from-end t)
   'double-float))

(sera:-> linear-least-squares
         ((array double-float (*)) &rest (array double-float (*)))
         (values (simple-array double-float (*)) double-float &optional))
(defun linear-least-squares (ys &rest xs)
  "Find \\(\\beta\\) which minimizes the following expression:

\\(|(Y - f(X_0, X_1, \\dots))|^2\\)

where \\(f\\) is

\\(f(X_0, X_1, \\dots) = X_0 \\beta_0 + X_1 \\beta_1 + \\dots + \\beta_N \\)
(\\(N\\) being the last element in the returned array).

Also the value of the target expression is returned as the second value.

All passed arrays must be of the same size. This function may signal
SIMPLE-ERROR if they are too short."
  (declare (optimize (speed 3)))
  (let* ((xs (lsqr-xs xs (length ys)))
         (%ys (em:vector->column-unsafe ys))
         (β (em:column
             (em:solve
              (em:mult xs  xs :ta t)
              (em:mult xs %ys :ta t))
             0)))
    (values β (linear-least-squares-cost β ys xs))))

;; IRLS
(sera:defconstructor linear-irls-options
  (ε double-float)
  (n alex:positive-fixnum))

(defparameter *default-linear-irls-options*
  (linear-irls-options 1d-10 200)
  "Default parameters for @c(linear-irls).")

(sera:-> linear-irls-cost
         ((vector double-float)
          (vector double-float)
          (matrix double-float)
          double-float)
         (values double-float &optional))
(defun linear-irls-cost (βs ys xs p)
  (declare (optimize (speed 3)))
  (let ((diff (em:sub (mult-mv xs βs) ys)))
    (reduce #'+ diff
            :key (lambda (x)
                   (declare (type double-float x))
                   (expt (abs x) p)))))

(sera:-> xt-*-diag ((simple-array double-float (* *))
                    (simple-array double-float (*)))
         (values (simple-array double-float (* *)) &optional))
(defun xt-*-diag (x d)
  (declare (optimize (speed 3)))
  (assert (= (array-dimension x 0) (length d)))
  (let ((result (make-array (list (array-dimension x 1)
                                  (array-dimension x 0))
                            :element-type 'double-float)))
    (loop for i below (array-dimension result 0) do
      (loop for j below (array-dimension result 1) do
        (setf (aref result i j)
              (* (aref x j i) (aref d j)))))
    result))

;; https://en.wikipedia.org/wiki/Iteratively_reweighted_least_squares
(sera:-> linear-irls
         (double-float
          linear-irls-options
          (array double-float (*))
          &rest (array double-float (*)))
         (values (simple-array double-float (*)) double-float &optional))
(defun linear-irls (p opt ys &rest xs)
    "Find \\(\\beta\\) which minimizes the following expression:

\\(|(Y - f(X_0, X_1, \\dots))|^p\\)

where \\(f\\) is

\\(f(X_0, X_1, \\dots) = X_0 \\beta_0 + X_1 \\beta_1 + \\dots + \\beta_N \\)
(\\(N\\) being the last element in the returned array).

Also the value of the target expression is returned as the second value.

All passed arrays must be of the same size. This function may signal
SIMPLE-ERROR if they are too short.

For \\(p = 2\\) use @c(linear-least-squares). This function is
iterative. Options @c(opt) contain the maximal number of iterations
\\(n\\) and the tolerance \\(\\varepsilon\\). Iterations stop when
either the number of iterations exceeds \\(n\\) or \\(\\sqrt{\\sum_i
(\\beta_i^k - \\beta_i^{k-1})^2} < \\varepsilon\\) on \\(k\\)-th
iteration."
  (declare (optimize (speed 3)))
  (let ((xs (lsqr-xs xs (length ys))))
    (labels ((%go (w β n)
               (declare (type alex:non-negative-fixnum n))
               (let* ((xtw (xt-*-diag xs w))
                      (%β (em:solve (em:@ xtw xs)
                                    (em:@ xtw (em:vector->column-unsafe ys))))
                      (βv (em:column %β 0))
                      (w (map
                          '(vector double-float)
                          (lambda (x y)
                            (expt (max 1d-4 (abs (- x y))) (- p 2)))
                          ys (em:column (em:mult xs %β) 0))))
                 (if (or (= n (linear-irls-options-n opt))
                         (and (not (zerop n))
                              (< (em:norm (map '(vector double-float) #'- β βv))
                                 (linear-irls-options-ε opt))))
                     βv (%go w βv (1+ n))))))
      (let ((β (%go (make-array (length ys)
                                :element-type 'double-float
                                :initial-element 1d0)
                    nil 0)))
        (values
         β (linear-irls-cost β ys xs p))))))
