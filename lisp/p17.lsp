(defun dvz(l)
(cond
((and (numberp l)(equal (mod l 3) 0)) nil)
((atom l) (list l))
(t(list (apply #'append(mapcar #'dvz l))))
)
)

(defun fct(F L)
((lambda(s)(cond
((null l) nil)
((s)(cons (s (fct f (cdr l)))))
(t nil)
)
)(funcall f (car l))))


(defun G(L)
(mapcon #'list L)
)







(defun fct1(l lvl e)
(cond
((and (atom l)(equal lvl 1)) e)
((atom l) l)
(t(mapcar #'(lambda(l)(cond
((equal lvl 1)(fct1 l 0 e))
(t(fct1 l 1 e))
))l))
)
)



(defun G(L)
(LIST (CAR L)(CAR L))
)



(defun arr(l lvl n)
(cond
((and (equal lvl 1)(atom l)) n)
((atom l) l)
(t(mapcar #'(lambda(l)(cond
((equal lvl 1)(arr l 0 n))
(t(arr l 1 n))
))l))
)
)
