(in-package :cl-optim/tests)

(def-suite optim :description "Test optimization")

(defun run-tests ()
  (let ((status (run 'optim)))
    (explain! status)
    (results-status status)))

(defun random-vector (n b)
  (gen-buffer :length (lambda () n)
              :element-type 'double-float
              :elements (gen-float :bound b
                                   :type 'double-float)))

(in-suite optim)

(test opt-paraboloid
  (for-all ((initial (random-vector 2 10d0)))
    (mapcar
     (lambda (optimizer)
       (multiple-value-bind (x iter)
           (funcall optimizer #'paraboloid-grad initial)
         (is (< iter *max-iterations*))
         (is (array-approx-p x #a(2 double-float . (0d0 0d0)) :atol 5d-3 :rtol 0d0))))
     (list #'gradient-descent
           #'gradient-descent-momentum
           #'nag
           (lambda (fn x)
             (adam fn x :η 1d-2))))))

(test opt-booth
  (for-all ((initial (random-vector 2 5d0)))
    (mapcar
     (lambda (optimizer)
       (multiple-value-bind (x iter)
           (funcall optimizer #'booth-grad initial)
         (is (< iter *max-iterations*))
         (is (array-approx-p x #a(2 double-float . (1d0 3d0)) :atol 1d-3))))
     (list #'gradient-descent-momentum
           #'nag
           (lambda (fn x)
             (adam fn x :η 1d-2))))))

(test opt-rosenbrock
  (for-all ((initial (random-vector 2 2d0)))
    (mapcar
     (lambda (optimizer)
       (multiple-value-bind (x iter)
           (funcall optimizer #'rosenbrock-grad initial)
         (is (< iter 100000))
         (is (array-approx-p x #a(2 double-float . (1d0 1d0)) :atol 1d-3))))
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
              :max-iterations 20000))))))

(test opt-beale
  (for-all ((initial (random-vector 2 5d-1)))
    (multiple-value-bind (x iter)
        (nag #'beale-grad initial :η 1d-2 :ε 1d-6 :β1 0.85d0)
      (is (array-approx-p x #a(2 double-float . (3d0 5d-1)) :atol 1d-5))
      (is (< iter *max-iterations*)))
    (let ((x (bfgs #'beale #'beale-grad initial)))
      (is (array-approx-p x #a(2 double-float . (3d0 5d-1)) :atol 1d-5)))))

(test opt-noise-sinc
  (is
   (<= (count
        nil
        (loop repeat 10 collect
             (let ((opt (simulated-annealing
                         (alexandria:compose #'noise-sinc (alexandria:rcurry #'aref 0))
                         #a(1 double-float . (10d0)))))
               (array-approx-p opt #a(1 double-float . (4d0)) :atol 0.15d0))))
       2)))

(test bfgs
  (let ((expected #a(3 double-float . (1d0 1d0 1d0))))
    (for-all ((initial (random-vector 3 10d0)))
      (let ((solution (bfgs #'rosenbrock #'rosenbrock-grad initial)))
        (is-true (array-approx-p solution expected :atol 1d-5))))))

(test nelder-mead
  (let ((expected #a(3 double-float . (1d0 1d0 1d0))))
    (for-all ((initial (random-vector 3 10d0)))
      (let* ((initial-simplex (make-initial-simplex initial 5d-1))
             (solution (nelder-mead #'rosenbrock initial-simplex
                                    :ftol 0d0 :xstol 0d0 :max-iterations 1000)))
        (is-true (array-approx-p solution expected :atol 1d-8))))))

(defun llsq-diff (a b xs ys p)
  (reduce
   #'+ (map '(vector double-float)
            (lambda (x y)
              (expt (abs (- y (* a x) b)) p))
            xs ys)))

(test linear-least-squares-exact
  (for-all ((xs (random-vector 2 1d0))
            (ys (random-vector 2 1d0)))
    (is (approxp (nth-value
                  1 (linear-least-squares ys xs))
                 0d0 :atol 1d-10)))
  (for-all ((xs1 (random-vector 3 1d0))
            (xs2 (random-vector 3 1d0))
            (ys  (random-vector 3 1d0)))
    (is (approxp (nth-value
                  1 (linear-least-squares ys xs1 xs2))
                 0d0 :atol 1d-10)))
  (for-all ((xs1 (random-vector 4 1d0))
            (xs2 (random-vector 4 1d0))
            (xs3 (random-vector 4 1d0))
            (ys  (random-vector 4 1d0)))
    (is (approxp (nth-value
                  1 (linear-least-squares ys xs1 xs2 xs3))
                 0d0 :atol 1d-10))))

(test linear-least-square-univariate
  (for-all* ((a (gen-float :bound 100d0 :type 'double-float))
             (b (gen-float :bound 100d0 :type 'double-float))
             (n (gen-integer :min 2 :max 100))
             (xs (random-vector n 100d0))
             (noise (random-vector n 1d0))
             (Δβ0 (gen-float :bound 1d0 :type 'double-float))
             (Δβ1 (gen-float :bound 1d0 :type 'double-float)))
    (let* ((ys (map '(vector double-float)
                    (lambda (x noise) (+ (* x a) b noise))
                    xs noise))
           (βs (linear-least-squares ys xs))
           (β0 (aref βs 0))
           (β1 (aref βs 1))
           (diff-min (llsq-diff β0 β1 xs ys 2))
           (diff     (llsq-diff (+ β0 Δβ0)
                                (+ β1 Δβ1)
                                xs ys 2)))
      (is (< diff-min diff)))))

(test linear-irls-exact
  (for-all ((xs (random-vector 2 1d0))
            (ys (random-vector 2 1d0))
            (p  (gen-float :bound 1d0 :type 'double-float)))
    (is (approxp (nth-value
                  1 (linear-irls
                     (+ p 2)
                     *default-linear-irls-options*
                     ys xs))
                 0d0 :atol 1d-10)))
  (for-all ((xs1 (random-vector 3 1d0))
            (xs2 (random-vector 3 1d0))
            (ys  (random-vector 3 1d0))
            (p  (gen-float :bound 1d0 :type 'double-float)))
    (is (approxp (nth-value
                  1 (linear-irls
                     (+ p 2)
                     *default-linear-irls-options*
                     ys xs1 xs2))
                 0d0 :atol 1d-10)))
  (for-all ((xs1 (random-vector 4 1d0))
            (xs2 (random-vector 4 1d0))
            (xs3 (random-vector 4 1d0))
            (ys  (random-vector 4 1d0))
            (p  (gen-float :bound 1d0 :type 'double-float)))
    (is (approxp (nth-value
                    1 (linear-irls
                       (+ p 2)
                       *default-linear-irls-options*
                       ys xs1 xs2 xs3))
                   0d0 :atol 1d-10))))

(test linear-irls-univariate
  (for-all* ((a (gen-float :bound 100d0 :type 'double-float))
             (b (gen-float :bound 100d0 :type 'double-float))
             (p (gen-float :bound 1d0 :type 'double-float))
             (n (gen-integer :min 2 :max 100))
             (xs (random-vector n 100d0))
             (noise (random-vector n 1d0))
             (Δβ0 (gen-float :bound 1d0 :type 'double-float))
             (Δβ1 (gen-float :bound 1d0 :type 'double-float)))
    (let* ((ys (map '(vector double-float)
                    (lambda (x noise) (+ (* x a) b noise))
                    xs noise))
           (p (+ p 2))
           (βs (linear-irls p *default-linear-irls-options* ys xs))
           (β0 (aref βs 0))
           (β1 (aref βs 1))
           (diff-min (llsq-diff β0 β1 xs ys p))
           (diff     (llsq-diff (+ β0 Δβ0)
                                (+ β1 Δβ1)
                                xs ys p)))
      (is (< diff-min diff)))))
