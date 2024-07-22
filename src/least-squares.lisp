(in-package :cl-optim)

(sera:-> linear-least-squares-cost
         (magicl:vector/double-float
          magicl:vector/double-float
          magicl:matrix/double-float)
         (values double-float &optional))
(defun linear-least-squares-cost (βs ys xs)
  (let ((diff (magicl:.- (magicl:@ xs βs) ys)))
    (magicl:dot diff diff)))

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
  (let* ((y (to-magicl-vector ys))
         (x (magicl:hstack
             (append (mapcar
                      (lambda (v) (magicl:vector->column-matrix (to-magicl-vector v)))
                      xs)
                     (list (magicl:ones (list (length ys) 1) :type 'double-float)))))
         (β (magicl:mult
             (magicl:mult
              (magicl:inv (magicl:mult x x :transa :t))
              x :transb :t)
             y)))
    (values
     (%storage β)
     (linear-least-squares-cost β y x))))
