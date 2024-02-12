(defun suma(l)
    (cond
        ((null l) 0)
        ((atom l) l)
        (t(apply #'+ (mapcar #'suma l)))
    )
)
(defun functie(l)
    (cond
        ((null l) (list nil))
        ((atom l) nil)
        ((and (listp l) (equal (mod (suma l) 2) 0)) (apply #'append (list l) (mapcar #'functie l)))
        (t (apply #'append (mapcar #'functie l)) )
    )
)

