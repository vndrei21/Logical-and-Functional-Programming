;; Copy and paste the function definition directly into the Lisp interpreter
(defun process-list (my-list)
  (dolist (element my-list)
    (format t "Element: ~a~%" element)))
