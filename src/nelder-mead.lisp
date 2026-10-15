(in-package :cl-optim)

(defconstant +α+ -1d0)
(defconstant +γ+ +2d0)
(defconstant +ρ+ +5d-1)
(defconstant +σ+ +5d-1)

(sera:defconstructor datapoint
  (xs  (vector double-float))
  (val double-float))

(sera:-> make-initial-simplex ((vector double-float) double-float)
         (values list &optional))
(defun make-initial-simplex (xs delta)
  (let ((dim (length xs)))
    (loop for i below (1+ dim)
          collect (let ((point (make-array dim
                                           :element-type 'double-float)))
                    (loop for j below dim do
                      (setf (aref point j)
                            (+ (aref xs j)
                               (if (= i j) delta 0))))
                    point))))

(sera:-> sort-datapoints (list)
         (values list &optional))
(defun sort-datapoints (points)
  ;; Biggest value first
  (sort (copy-list points) #'>
        :key (lambda (point)
               (datapoint-val point))))

(sera:-> sum! ((vector double-float)
               (vector double-float))
         (values (vector double-float) &optional))
(declaim (inline sum!))
(defun sum! (x y)
  (loop for i below (length x) do
    (incf (aref x i) (aref y i)))
  x)

(sera:-> centroid (list)
         (values (vector double-float) &optional))
(defun centroid (points)
  (declare (optimize (speed 3)))
  (let* ((length (length (datapoint-xs (car points))))
         (mean (make-array length
                          :element-type 'double-float
                          :initial-element 0d0)))
    (loop for point in points do
      (sum! mean (datapoint-xs point)))
    (loop for i below length do
      (setf (aref mean i)
            (/ (aref mean i) length)))
    mean))

(sera:-> evaluate-function (doubles->double (vector double-float))
         (values datapoint &optional))
(defun evaluate-function (f xs)
  (declare (optimize (speed 3)))
  (datapoint xs (funcall f xs)))

(sera:-> new-point ((vector double-float)
                    (vector double-float)
                    double-float)
         (values (vector double-float) &optional))
(defun new-point (x1 x2 c)
  (declare (optimize (speed 3)))
  (map '(vector double-float)
       (lambda (x1 x2)
         (+ x1 (* c (- x2 x1))))
       x1 x2))

(sera:-> shrink-points (doubles->double datapoint list)
         (values list &optional))
(defun shrink-points (f best datapoints)
  (declare (optimize (speed 3)))
  (mapcar
   (lambda (point)
     (if (eq best point) best
         (let ((x (new-point (datapoint-xs best)
                             (datapoint-xs point)
                             +σ+)))
           (datapoint x (funcall f x)))))
     datapoints))

(sera:-> argdiff ((vector double-float)
                  (vector double-float))
         (values double-float &optional))
(defun argdiff (xs1 xs2)
  (declare (optimize (speed 3)))
  (loop for x1 across xs1
        for x2 across xs2
        maximize (abs (- x1 x2))
        double-float))

(sera:-> nelder-mead (doubles->double list &key
                      (:max-iterations alex:positive-fixnum)
                      (:ftol           double-float)
                      (:xstol          double-float))
         (values (vector double-float) double-float alex:positive-fixnum &optional))
(defun nelder-mead (f simplex &key (max-iterations 10000) (ftol 1d-6) (xstol 1d-6))
  (declare (optimize (speed 3)))
  (labels ((%%go (datapoints)
             (let* ((datapoints (sort-datapoints datapoints))
                    ;; OMG OMG
                    (best (car (last datapoints)))
                    (worst (car datapoints))
                    (second-worst (cadr datapoints))
                    (centroid (centroid (cdr datapoints)))
                    (reflected (new-point centroid (datapoint-xs worst) +α+))
                    (value (funcall f reflected)))
               (values
                (cond
                  ;; best < reflected < second worst
                  ((< (datapoint-val best) value
                      (datapoint-val second-worst))
                   (cons (datapoint reflected value)
                         (cdr datapoints)))
                  ;; Better than the best
                  ((< value (datapoint-val best))
                   (let* ((expanded (new-point centroid reflected +γ+))
                          (%value (funcall f expanded)))
                     (cons
                      (if (< %value value)
                          ;; Expanded point is better
                          (datapoint expanded %value)
                          (datapoint reflected value))
                      (cdr datapoints))))
                  ;; Better than the worst
                  ((< value (datapoint-val worst))
                   (let* ((contracted (new-point centroid reflected +ρ+))
                          (%value (funcall f contracted)))
                     (if (< %value value)
                         ;; Contracted is better
                         (cons (datapoint contracted %value)
                               (cdr datapoints))
                         (shrink-points f best datapoints))))
                  ;; Worse than the worst
                  (t
                   (let* ((contracted (new-point centroid (datapoint-xs worst) +ρ+))
                          (value (funcall f contracted)))
                     ;; Contracted is better
                     (if (< value (datapoint-val worst))
                         (cons (datapoint contracted value)
                               (cdr datapoints))
                         (shrink-points f best datapoints)))))
                best)))
           (%go (datapoints best n)
             (declare (type fixnum n))
             (if (zerop n)
                 (values best 0)
                 (sera:mvlet* ((worst (car datapoints))
                               (datapoints best (%%go datapoints))
                               (new-worst (car datapoints))
                               (n (1- n)))
                   (if (or (< (abs (- (datapoint-val worst)
                                      (datapoint-val new-worst)))
                              ftol)
                           (< (argdiff (datapoint-xs worst)
                                       (datapoint-xs new-worst))
                              xstol))
                       (values best n)
                       (%go datapoints best n))))))
    (multiple-value-bind (best n)
        (%go (mapcar
              (lambda (xs)
                (evaluate-function f xs))
              simplex)
             nil max-iterations)
      (values (datapoint-xs best)
              (datapoint-val best)
              (- max-iterations n)))))
