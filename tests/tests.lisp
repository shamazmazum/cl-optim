(in-package :cl-optim-tests)

(def-suite optim :description "Test optimization")

(defun run-tests ()
  (let ((status (run 'optim)))
    (explain! status)
    (results-status status)))

(defun closep (result expected &optional (tolerance 5d-3))
  (every
   (lambda (x y) (< (abs (- x y)) tolerance))
   result expected))

(in-suite optim)

(test opt-paraboloid
  (mapcar
   (lambda (optimizer)
     (loop repeat 10 do
          (multiple-value-bind (x iter)
              (funcall optimizer #'paraboloid
                       (list (random 10d0)
                             (random 10d0)))
            (is (< iter *max-iterations*))
            (is (closep x '(0 0))))))
   (list #'gradient-descent
         #'gradient-descent-momentum
         #'nag
         (lambda (fn x)
           (adam fn x :η 1d-2)))))

(test opt-booth
  (mapcar
   (lambda (optimizer)
     (loop repeat 10 do
          (multiple-value-bind (x iter)
              (funcall optimizer #'booth
                       (list (- (random 10d0) 5d0)
                             (- (random 10d0) 5d0)))
            (is (< iter *max-iterations*))
            (is (closep x '(1 3))))))
   (list #'gradient-descent-momentum
         #'nag
         (lambda (fn x)
           (adam fn x :η 1d-2)))))

(test opt-rosenbrock
  (mapcar
   (lambda (optimizer)
     (loop repeat 10 do
          (multiple-value-bind (x iter)
              (funcall optimizer
                       #'rosenbrock
                       (list (- (random 4d0) 2d0)
                             (- (random 4d0) 2d0)))
            (is (< iter 100000))
            (is (closep x '(2 4))))))
   (list
    (lambda (fn x)
      (nag fn x
           :η  1d-4
           :β1 0.99d0
           :ε  1d-4
           :max-iterations 20000))
    (lambda (fn x)
      (adam fn x
            :η 1d-2
            :ε 1d-4
            :max-iterations 20000)))))

(test opt-hills
  (loop repeat 10 do
       (multiple-value-bind (x iter)
           (nag (alexandria:compose #'hills #'car)
                (list (+ (random 4d0) 11d0))
                :η  1d-3
                :β1 0.95d0)
         (is (< iter *max-iterations*))
         (is (closep x '(3.219))))))

(test opt-noise-sinc
  (is
   (<= (count
        nil
        (loop repeat 10 collect
             (let ((opt (simulated-annealing
                         (alexandria:compose #'noise-sinc #'first)
                         '(10d0))))
               (closep opt '(4d0) 0.15d0))))
       2)))
