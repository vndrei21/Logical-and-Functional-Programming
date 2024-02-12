(defun rm(l e)
(cond
    ((equal l e)nil)
    ((atom l) (list l))
    (t(list (apply #'append(mapcar #'(lambda(l)(rm l e)) l))))
)
)

(defun atms(l)
(cond 
((and(null (cdr l)) (numberp (car l))) nil)
((null l) t)
((and(listp (car l))(numberp (car l))) nil)
((listp (car l)) (and (atms (car l)) (atms (cdr l))))
(t(atms (cdr l)))
)
)

(defun si(l)
(cond
((null l) t)
(t(and (car l) (si (cdr l))))
)
)



(defun lvl(l)
(cond
((atom l) 0)
((equal (atms l) t) (apply #'+ 1(mapcar #'lvl l)))
(t(apply #'+ (mapcar #'lvl l)))
)
)




