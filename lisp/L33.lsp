(defun f(l)
(cond
((atom l) (list l))
((not (atom l)) (apply #'append (mapcar #'f l))))
)



(defun adancime(l)
(cond
    ((or (null l) (atom l)) 0)
    (t(+ 1 (adancime(cdr l))))
)
)


(defun v2(l)
(cond
((atom l) 0)
(t(+  1(max (mapcan #'v2 l))))
)
)



(defun sau(l)
(cond
((null l)nil)
(t(or (car l) (sau (cdr l))))
)
)

(defun f3(l a)
(cond
((null l) nil)
((and (atom l)(equal l a))  t)
(t (apply #'append (mapcar #'(lambda(l)(f3 l a)) (cdr l)))
)
)
)

(defun f4(l a)
(cond
((equal l a) t)
((atom l) nil)
((equal (car l) a) t)
(t (sau (mapcar #'(lambda(l)(f4 l a)) (cdr l))))
)
)


(defun f5(l)
(cond
((atom l) l)
(t (apply #'+ (mapcar #'f5 l)))
)
)


(defun si(l)
(cond
((null l) nil)
(t(or(car l)(si (cdr l))))
)
)


(defun f6(l n)
(cond
((equal (car l) n) t)
((equal l n) t)
((atom l) nil)
(t (si (mapcar #'(lambda(l)(f6 l n)) (cdr l))))
)
)

(defun f7(l)
(cond
((atom l) l)
(t(apply #'*(mapcar #'f7 l)))
)
)


(defun f8(l)
(cond
((atom l) l)
(t(apply #'max(mapcar #'f8 l)))
)
)




(defun sub(l n l2)
(cond
((equal (car l) n) (append l2(mapcar #'(lambda(l)(sub l n l2))(cdr l))))
((equal l n) l2)
((atom l) l)
(t(apply #'list (car l) (mapcar #'(lambda(l)(sub l n l2)) (cdr l))))
)
)


(defun spi(l)
(cond
((and (atom l) (equal (mod l 2) 0)) l)
((atom l) (* l -1))
(t(apply #'+(mapcar #'spi l)))
)
)

(defun f8(l)
(cond
((numberp l) l)
((atom l) 0)
(t (apply #'max (mapcar #'f8 l)))
)
)


(defun st(l e)
(cond
((equal l e) nil)
((atom l) (list l))
(t (remove-if #'null (list (apply #'append (mapcar #'(lambda(l)(st l e)) l)))))
)
)



(defun sterg(l e)
(remove-if #'null (st l e))
)


(defun inloc(l e c)
(cond
((equal l e) c)
((atom l) l)
(t(mapcar #'(lambda(l)(inloc l e c)) l)))
)


(defun adancime(l)
(cond
((null l) 0)
((atom l) 0)
(t(+ 1(mapcan #'adancime l)))
)
)

(defun aux(l)
(adancime(cdr l))
)

(defun nr(l)
(cond
((atom l) 1)
(t (apply #'+(mapcar #'nr l)))
)
)








