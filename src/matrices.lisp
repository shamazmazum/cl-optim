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
  (em:column
   (em:mult m (em:vector->column-unsafe v)) 0))

;; Helper functions
(sera:-> column-*-row ((vector double-float)
                       (vector double-float))
         (values (matrix double-float) &optional))
(declaim (inline column-*-row))
(defun column-*-row (c r)
  (em:mult
   (em:vector->column-unsafe c)
   (em:vector->row-unsafe    r)))

#|
(sera:-> diag-*-matrix ((vector double-float)
                        (matrix double-float))
         (values (matrix double-float) &optional))
(defun diag-*-matrix (diag matrix)
  (declare (optimize (speed 3)))
  (assert (= (length diag) (array-dimension matrix 0)))
  (let ((result (make-array (array-dimensions matrix)
                            :element-type 'double-float)))
    (loop for i fixnum below (array-dimension matrix 0) do
      (loop for j fixnum below (array-dimension matrix 1) do
        (setf (aref result i j)
              (* (aref matrix i j)
                 (aref diag i)))))
    result))
|#
