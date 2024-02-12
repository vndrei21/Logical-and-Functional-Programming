(defun func(l lvl k)
(cond
((and(equal lvl k)(atom l)) 0)
((atom l) l)
(t(mapcar #'(lambda(l)(func l (+ 1 lvl) k))l))
)
)