(defun sterge(l a)
(cond
((equal l a) nil)
((atom l) (list l))
(t(apply #'append (mapcar #'(lambda(l)(sterge l a)) l)))
)
)

(defun nr_ap(l a)
(cond
((equal l a) 1)
((atom l) 0)
(t(apply #'+ (mapcar #'(lambda(l)(nr_ap l a)) l)))
)
)

(defun bca(l1 l2)
(cond
((null l1) nil)
(t(cons (list (Car l1) (nr_ap l2 (car l1)))(bca (sterge l1 (car l1)) l2))
)
)
)
(defun m2(l)
(bca l l))


(defun creste(l k)
(cond
((null l)nil)
((and (not (equal nil (cadr l))) (> (car l) (cadr l))) (list k l))
(t(creste (cdr l) 1))
)
)
(defun des(l)
(cond
((null l) t)
((and (atom l) (not (equal l nil))) t)
((and (not (equal nil (cadr l)))(< (car l) (cadr l))) nil)
(t (des(cdr l)))
)
)


(Defun m44(l)
(cond 
((equal nil (creste l 0)) nil)
((equal 0 (car (creste l  0)))nil)
(t(des(cdr (creste l 0))))
)
)