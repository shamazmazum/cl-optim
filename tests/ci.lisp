(defun do-all()
  (handler-case
      (asdf:load-system :cl-optim/tests)
    (error ()
      (uiop:quit 1)))
  (setq *random-state* (make-random-state t))
  (uiop:quit
   (if (uiop:call-function "cl-optim-tests:run-tests")
       0 1)))

(do-all)
