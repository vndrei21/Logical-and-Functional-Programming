



(defun inloc(l n l1)
(cond
((equal l n) l1)
((atom l ) (list l))
(t(list (apply #'append(mapcar #'(lambda(l)(inloc l n l1))l))))
)
)
(defun invers(l)
(cond
((null l) nil)
(t(append (invers (cdr l)) (list (car l))))
)
)
(print (invers '(1 2 3 4)))

(defun aux(l1 l2 tr)
(cond
((null l1) nil)
((null l2) nil)
(t(append (list (floor (mod (+ tr (+ (car l1) (car l2))) 10)))(aux (cdr l1) (cdr l2)(floor(/ (+ tr (+ (car l1) (car l2))) 10) ))))))

(Defun mai(l1 l2)
 (invers (aux (invers l1) (invers l2) 0)))



(defun asc(l1 l2)
(cond
((null l1) nil)
((null l2) nil)
(t(append (list (cons (car l1) (car l2))) (asc (cdr l1) (cdr l2))))
)
)

(defun nr_subliste(l)
(cond
((null l) 0)
((atom l) 0)
(t(apply #'+ 1(mapcar #'nr_subliste l)))
)
)

(defun liniara(l)
(cond
((null l) t)
(t(and (atom (car l))(liniara (cdr l))))
)
)

(defun prc(l a)
(cond 
((null l) nil)
(t(append (cons (list a (Car l)) (prc(cdr l) a))(prc(cddr l) (cadr l))))
)
)

(defun m(l)
(cdr (prc l (car l)))
)

