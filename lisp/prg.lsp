(defun p1(l lvl k)
(cond
((and(atom l)(equal lvl (+ 1 k)))0)
((atom l) l)
(t(mapcar #'(lambda(l)(p1 l (+ 1 lvl) k))l))
)
)

(defun sum(l)
(cond
((null l) 0)
((numberp (car l)) (+ (car l) (sum (cdr l))))
(t(sum (cdr l)))
))



(defun f(l par)
(cond
((atom l) 0)
((and(equal 0 (mod (sum l) 2)) (equal par 0))
(apply #'+ 1(mapcar #'(lambda(l)(f l 0))l)))
(t(apply #'+(mapcar #'(lambda(l)(cond
((equal par 0)(f l 1))
(t (f l 0))
)) l)))
)
)