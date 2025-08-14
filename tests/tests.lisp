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
              (funcall optimizer #'paraboloid-grad
                       (to-doubles
                        (list (random 10d0)
                              (random 10d0))))
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
              (funcall optimizer #'booth-grad
                       (to-doubles
                        (list (- (random 10d0) 5d0)
                              (- (random 10d0) 5d0))))
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
                       #'rosenbrock-grad
                       (to-doubles
                        (list (- (random 4d0) 2d0)
                              (- (random 4d0) 2d0))))
            (is (< iter 100000))
            (is (closep x '(1 1))))))
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
           (nag #'hills-grad
                (to-doubles (list (+ (random 4d0) 11d0)))
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
                         (alexandria:compose #'noise-sinc (alexandria:rcurry #'aref 0))
                         (to-doubles '(10d0)))))
               (closep opt '(4d0) 0.15d0))))
       2)))

(test bfgs
  (loop with expected = (magicl:ones '(3))
        repeat 1000
        for vector = (magicl:.- (magicl:scale (magicl:rand '(3)) 40) 20) do
        (is (< (magicl:norm
                (magicl:.- (bfgs/magicl #'rosenbrock #'rosenbrock-grad vector) expected))
               1d-4))))

(defun llsq-diff (a b xs ys p)
  (reduce
   #'+ (map '(vector double-float)
            (lambda (x y)
              (expt (abs (- y (* a x) b)) p))
            xs ys)))

(test linear-least-squares-exact
  (loop repeat 500 do
        (is (< (nth-value
                1 (linear-least-squares
                   (to-doubles (loop repeat 2 collect (random 1d0)))
                   (to-doubles (loop repeat 2 collect (random 1d0)))))
               1d-10)))
  (loop repeat 500 do
        (is (< (nth-value
                1 (linear-least-squares
                   (to-doubles (loop repeat 3 collect (random 1d0)))
                   (to-doubles (loop repeat 3 collect (random 1d0)))
                   (to-doubles (loop repeat 3 collect (random 1d0)))))
               1d-10)))
  (loop repeat 500 do
        (is (< (nth-value
                1 (linear-least-squares
                   (to-doubles (loop repeat 4 collect (random 1d0)))
                   (to-doubles (loop repeat 4 collect (random 1d0)))
                   (to-doubles (loop repeat 4 collect (random 1d0)))
                   (to-doubles (loop repeat 4 collect (random 1d0)))))
               1d-10))))

(test linear-least-square-univariate
  (loop repeat 1000
        for a  = (random 100d0)
        for b  = (random 100d0)
        for n  = (+ (random 100) 2)
        for xs = (to-doubles (loop repeat n collect (random 100d0)))
        for ys = (map '(vector double-float)
                      (lambda (x noise) (+ (* x a) b noise))
                      xs (loop repeat n collect (random 1d0)))
        for βs = (linear-least-squares ys xs)
        for β0 = (aref βs 0)
        for β1 = (aref βs 1)

        for diff-min = (llsq-diff β0 β1 xs ys 2)
        for diff     = (llsq-diff (+ β0 (random 1d0))
                                  (+ β1 (random 1d0))
                                  xs ys 2)
        do (is (< diff-min diff))))

(test linear-irls-exact
  (loop repeat 500 do
        (is (< (nth-value
                1 (linear-irls
                   (1+ (random 2d0))
                   *default-linear-irls-options*
                   (to-doubles (loop repeat 2 collect (random 1d0)))
                   (to-doubles (loop repeat 2 collect (random 1d0)))))
               1d-6)))
  (loop repeat 500 do
        (is (< (nth-value
                1 (linear-irls
                   (1+ (random 2d0))
                   *default-linear-irls-options*
                   (to-doubles (loop repeat 3 collect (random 1d0)))
                   (to-doubles (loop repeat 3 collect (random 1d0)))
                   (to-doubles (loop repeat 3 collect (random 1d0)))))
               1d-6)))
  (loop repeat 500 do
        (is (< (nth-value
                1 (linear-irls
                   (1+ (random 2d0))
                   *default-linear-irls-options*
                   (to-doubles (loop repeat 4 collect (random 1d0)))
                   (to-doubles (loop repeat 4 collect (random 1d0)))
                   (to-doubles (loop repeat 4 collect (random 1d0)))
                   (to-doubles (loop repeat 4 collect (random 1d0)))))
               1d-6))))

(test linear-irls-univariate
  (loop repeat 10000
        for a  = (+ (random 10d0) 3d-1)
        for b  = (random 10d0)
        for p  = (+ (random 2d0) 1)
        for n  = (+ (random 200) 2)
        for xs = (to-doubles (loop repeat n collect (random 100d0)))
        for ys = (map '(vector double-float)
                      (lambda (x noise) (+ (* x a) b noise))
                      xs (loop repeat n collect (random 1d0)))

        for βs = (linear-irls p *default-linear-irls-options* ys xs)
        for β0 = (aref βs 0)
        for β1 = (aref βs 1)

        for diff-min = (llsq-diff β0 β1 xs ys p)
        for diff     = (llsq-diff (+ β0 (random 1d0) -5d-1)
                                  (+ β1 (random 1d0) -5d-1)
                                  xs ys p)
        do (is (> diff diff-min))))
