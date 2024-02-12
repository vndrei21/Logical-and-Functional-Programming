(defun interschimbare(lst val nval)
(cond
((and (atom lst)(equal lst val)) nval)
((and (atom lst)(not (equal lst val))) lst)
(t(mapcar #'(lambda (lst) (interschimbare lst val nval)) lst))
)
)
(print(interschimbare '(a (b (c)) (d) (e (f))) 'x 'g))



