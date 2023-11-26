;; Differentiable functions
(in-package :cl-optim-tests-fn-diff)

(declaim (ftype differentiable-multivariate rosenbrock))
(defun rosenbrock (xs)
  (declare (optimize (speed 3)))
  (si:foldl #'+ #d(0d0 0d0)
            (si:imap
             (lambda (i)
               (+ (* 100 (expt (- (aref xs (cl:1+ i))
                                  (expt (aref xs i) 2))
                               2))
                  (expt (- 1 (aref xs i)) 2)))
             (si:range 0 (cl:1- (length xs))))))

(declaim (ftype differentiable-multivariate paraboloid))
(defun paraboloid (xs)
  (declare (optimize (speed 3)))
  (+ (expt (aref xs 0) 2)
     (expt (aref xs 1) 2)))

(declaim (ftype differentiable-multivariate booth))
(defun booth (xs)
  (declare (optimize (speed 3)))
  (let ((x (aref xs 0))
        (y (aref xs 1)))
    (+ (expt (+ x (* 2 y) -7) 2)
       (expt (+ (* 2 x) y -5) 2))))

(declaim (ftype differentiable-univariate hills))
(defun hills (x)
  (declare (optimize (speed 3)))
  (+ (expt (- x 4) 2)
     (* 10 (expt (sin x) 2))))

;; Not differentiable
(in-package :cl-optim-tests-fn)

(defun noise-sinc (x)
  (declare (type double-float x)
           (optimize (speed 3)))
  (let ((x-shift (- x 4)))
    (- (random 0.2)
       (if (zerop x-shift) 1d0
           (/ (sin (* 5 x-shift)) x-shift)))))
