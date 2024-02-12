(defun r(l lvl k)
(cond
((and(atom l)(equal lvl k)) 0)
((atom l) l)
(t(mapcar #'(lambda(l)(r l (+ 1 lvl) k)) l))
)
)


