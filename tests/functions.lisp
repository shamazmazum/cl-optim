;; Differentiable functions
(in-package :cl-optim-tests-fn-diff)

(defun rosenbrock (xs)
  (si:foldl #'+ 0d0
            (si:imap
             (lambda (i)
               (+ (* 100 (expt (- (aref xs (cl:1+ i))
                                  (expt (aref xs i) 2))
                               2))
                  (expt (- 1 (aref xs i)) 2)))
             (si:range 0 (cl:1- (length xs))))))

(defun rosenbrock-grad (xs)
  (diff:ad-multivariate #'rosenbrock xs))

(defun paraboloid (xs)
  (+ (expt (aref xs 0) 2)
     (expt (aref xs 1) 2)))

(defun paraboloid-grad (xs)
  (diff:ad-multivariate #'paraboloid xs))

(defun booth (xs)
  (let ((x (aref xs 0))
        (y (aref xs 1)))
    (+ (expt (+ x (* 2 y) -7) 2)
       (expt (+ (* 2 x) y -5) 2))))

(defun booth-grad (xs)
  (diff:ad-multivariate #'booth xs))

(defun hills (xs)
  (let ((x (aref xs 0)))
    (+ (expt (- x 4) 2)
       (* 10 (expt (sin x) 2)))))

(defun hills-grad (xs)
  (diff:ad-multivariate #'hills xs))

;; Not differentiable
(in-package :cl-optim-tests-fn)

(defun noise-sinc (x)
  (let ((x-shift (- x 4)))
    (- (random 0.2)
       (if (zerop x-shift) 1d0
           (/ (sin (* 5 x-shift)) x-shift)))))
