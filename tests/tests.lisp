(in-package :cl-optim-tests)

(def-suite optim :description "Test optimization")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (explain! (run suite)))
                 '(optim))))

(defun closep (result expected)
  (every
   (lambda (x y) (< (abs (- x y)) 5e-3))
   result expected))

(in-suite optim)

(test opt-paraboloid
  (mapcar
   (lambda (optimizer)
     (loop repeat 10 do
          (multiple-value-bind (x iter)
              (funcall optimizer #'paraboloid
                       (list (random 10.0)
                             (random 10.0)))
            (is (< iter *max-iterations*))
            (is (closep x '(0 0))))))
   (list #'gradient-descent
         #'gradient-descent-momentum
         #'nag)))

(test opt-booth
  (mapcar
   (lambda (optimizer)
     (loop repeat 10 do
          (multiple-value-bind (x iter)
              (funcall optimizer #'booth
                       (list (- (random 10.0) 5.0)
                             (- (random 10.0) 5.0)))
            (is (< iter *max-iterations*))
            (is (closep x '(1 3))))))
   (list #'gradient-descent-momentum
         #'nag)))

(test opt-rosenbrock
  (loop repeat 10 do
       (multiple-value-bind (x iter)
           (nag #'rosenbrock
                (list (- (random 4.0) 2.0)
                      (- (random 4.0) 2.0))
                :descent-rate 1e-4
                :epsilon 1e-4
                :max-iterations 100000)
         (is (< iter 100000))
         (is (closep x '(2 4))))))

(test opt-hills
  (loop repeat 10 do
       (multiple-value-bind (x iter)
           (nag (alexandria:compose #'hills #'car)
                (list (+ (random 4.0) 11.0))
                :descent-rate 1e-3
                :friction 0.95)
         (is (< iter *max-iterations*))
         (is (closep x '(3.219))))))
