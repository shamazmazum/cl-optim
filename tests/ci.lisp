(defun do-all()
  (ql:quickload :cl-optim/tests :verbose t)
  (setq *random-state* (make-random-state t))
  (uiop:quit
   (if (uiop:call-function "cl-optim-tests:run-tests")
       0 1)))

(do-all)
