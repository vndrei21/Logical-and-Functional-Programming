(defun sterge(l x)
(cond
((equal l x) nil)
((atom l) (list l))
(t (apply #'append(mapcar #'(lambda(l )(sterge l x))l)))
)
)
(print (sterge '(1 2 3 5  4) 2))


(defun sort(l)
(cond
((null l) nil)
(t(append (list (apply #'min l))(sort(sterge l (apply #'min l)))))
)
)

(print (sort '(1 5 2 2 4 7 3)))