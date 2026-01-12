(in-package :cl-optim)

(declaim (inline eye))
(defun eye (n)
  (let ((result (make-array (list n n)
                            :element-type 'double-float
                            :initial-element 0d0)))
    (loop for i below n do
      (setf (aref result i i) 1d0))
    result))

(declaim (inline mult-mv))
(defun mult-mv (m v)
  (let ((length (length v)))
    (em:reshape
     (em:mult
      m (em:reshape-unsafe v (list length 1)))
     (list length))))

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

(sera:-> column-*-row ((vector double-float)
                       (vector double-float))
         (values (matrix double-float) &optional))
(declaim (inline column-*-row))
(defun column-*-row (c r)
  (em:mult
   (em:reshape-unsafe c (list (length c) 1))
   (em:reshape-unsafe r (list 1 (length r)))))

(sera:-> to-magicl-vector ((simple-array double-float (*)))
         (values magicl:vector/double-float &optional))
(declaim (inline to-magicl-vector))
(defun to-magicl-vector (vector)
  "Convert a lisp vector to magicl vector (no copy)."
  (magicl:make-tensor 'magicl:vector/double-float
                      (list (length vector))
                      :storage vector))

(sera:-> diag-*-matrix (magicl:vector/double-float
                        magicl:matrix/double-float)
         (values magicl:matrix/double-float &optional))
(defun diag-*-matrix (diag matrix)
  "Convert a magicl vector to a diagonal matrix."
  (declare (optimize (speed 3)))
  (let* ((shape (magicl:shape matrix))
         (result (magicl:zeros shape :type 'double-float)))
    (assert (= (the fixnum (magicl:size diag))
               (the fixnum (first shape))))
    (loop for i fixnum below (first shape) do
          (loop for j fixnum below (second shape) do
                (setf (magicl:tref result i j)
                      (* (magicl:tref matrix i j)
                         (magicl:tref diag i)))))
    result))
