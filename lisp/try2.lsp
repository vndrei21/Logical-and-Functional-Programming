(defun myf(arg)
(+ arg 10))
(defun andrei(l)
(cdr l))
;; myfunction.lisp
(defun process-list (my-list)
  (dolist (element my-list)
    (format t "Element: ~a~%" element)))
    