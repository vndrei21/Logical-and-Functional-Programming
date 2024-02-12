(defun este(l x)
(cond 
((equal (car l) x) t)
((atom l) nil)
(t(sau (mapcar #'(lambda(l)(este l x))(cdr l)) ))
)
)


(defun sau(l)
(cond
((null l) nil)
(t(or (car l) (sau(cdr l))))
)
)

(defun cale(l e)
(cond
((equal (car l) e) (list e))
((este l e) (cons (car l) (apply #'append (mapcar (lambda (l) (cale l e)) (cdr l)))))
(t nil)
)
)


(defun cale1(l x)
(cond
((equal (car l) x) (list x))
((este l x) (cons (car l) (apply #'append (mapcar (lambda (l) (cale l x)) (cdr l)))))
(t nil)
)
)



(defun primul(l)
(cond
((numberp l)(list l))
((atom l) nil)
(t (append (primul(car l))(primul(cdr l))))
)
)

(defun verifica(l)
(cond
((null l) nil)
((equal(mod(car l)2)1)t)
(t nil)
)
)


(defun liste(l)
(cond
((atom l) 0)
((verifica (primul l))(+ 1 (apply #'+ (mapcar #'liste l))))
(t(apply #'+ (mapcar #'liste l)))
)
)
