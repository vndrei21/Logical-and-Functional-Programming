

(defun fct(l lvl e)
(cond
((and (atom l)(equal lvl 1)) e)
((atom l) l)
(t(mapcar #'(lambda(l)(cond
((equal lvl 1)(fct l 0 e))
(t(fct l 0 1))
))l))
)
)