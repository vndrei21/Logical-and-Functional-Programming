(defun F(L)
(cond
((null l ) nil)
((Listp(car l)) (lambda(s)(append s(F(cdr l)(car s))) (F(car l))))
)
)


(defun sterg(l e)
(cond
((equal l e) nil)
((atom l) (list l))
(t (list (apply #'append (mapcar #'(lambda(l)(sterg l e)) l))))
)
)





(defun inloc(l)
(cond
((null l) nil)
((and (numberp(car l)) (= (mod (car l) 2) 0))(cons (+(car l) 1)(inloc (cdr l))))
((atom (car l)) (cons (car l) (inloc (cdr l))))
(t (apply #'append  (list (car l) ) (inloc(cdr l))))
)
)

(defun inloc2(l)
(cond
((and (numberp l)(= (mod l 2) 0))(+ 1 l))
((atom l) l)
(t(mapcar #'inloc2 l))
)
)
(defun atom_max(l max)
(cond
    ((null l) max)
    ((and (numberp (car l)) (> (car l) max))(atom_max (cdr l) (car l)))
    (t(atom_max (cdr l) max)
)
)
)

(defun nr_sub(l lvl)
(cond
((atom l) 0)
((and (= lvl 1)(= (mod (atom_max l 0) 2) 0)) (apply #'+ 1 (mapcar #'(lambda(l)(cond
((= lvl 1) (nr_sub l 0))
(t(nr_sub l 1))))l)))
(t (apply #'+(mapcar #'(lambda(l)(cond
((= lvl 1) (nr_sub l 0))
(t(nr_sub l 1))))l)))
)
)
(DEFUN F(L)
(MAX (CAR L) (CADDR L))
)
(SETQ F 10)
(SETQ G F)


(defun inl(l lvl k e)
(cond
((and (atom l) (equal lvl (+ k 1))) e)
((atom l) l)
(t (mapcar #'(lambda(l)(inl l (+ 1 lvl) k e)) l))
)
)

(defun nmrc(l)
(cond
((numberp l) (list l))
((atom l) nil)
(t(append(apply #'append (mapcar #'nmrc l))))
)
)

(defun functie(l)
(cond
((atom l) 0)
((equal (mod (car (nmrc l)) 2) 0) (apply #'+ 1(mapcar #'functie l)))
(t(apply #'+(mapcar #'functie l)))
)
)

(defun F(l)
(cond
((atom l) -1)
((lambda(s)(cond
((> s 0)(+ (car l) s))
(t(F (CDR l))))(f (car l))))
))


(defun inloc(lst lvl k)
(cond
((and (= lvl k) (atom lst)) 0)
((atom lst) lst)
(t(mapcar #'(lambda(l)(inloc l (+ 1 lvl) k)) lst))
)
)


(Defun amin(l lvl)
(cond
((and(numberp l)(= (mod lvl 2) 0)) (list l))
((atom l) (list 9999))
(t (apply #'append
(mapcar #'(lambda(l)(cond
((= lvl 0) (amin l 1))
(t (amin l 0))
)) l)))))

(defun sol(l lvl)
(cond
((and(equal (mod lvl 2) 0)(equal (mod (apply #'min (amin l lvl)) 2)0)) 1)
((atom l) 0)
(t(apply #'+(mapcar #'(lambda(l)(sol l(+ 1 lvl))) l)))
)
)

(DEFUN inc(x)(+ 1 x))


(defun cauta(l n)
(cond
    ((equal l n) t)
    ((atom l) nil)
    (t(sau (mapcar #'(lambda(l)(cauta l n)) l))
))
)

(defun sau(l)
(cond
((null l) nil)
(t(or (car l) (sau(cdr l))))
)
)


(defun arb(l n)
(cond
((equal (cauta l n) t)(apply #'append (list(car l))(mapcar #'(lambda(l)(arb l n))(cdr l))))
(t (append (apply #'append(mapcar #'(lambda(l)(cauta l n))(cdr l)))))
)
)

(defun dnf(l n k)
(cond
((null l) nil)
((= k n) (append (list (car l) (car l))(dnf(cdr l)(+ n k)(+ k 1))))
(t(append (list (car l))(dnf (cdr l) n (+ 1 k))))
)
)

(defun l_atomi(l)
(cond
((atom l) (list l))
(t(append (apply #'append(mapcar #'l_atomi l))))
)
)

(defun l1(l)
(cond
((atom l) 0)
((not (numberp(Car (l_atomi l))))(apply #'+ 1(mapcar #'l1  l)))
(t(apply #'+ (mapcar #'l1 l))))
)


(defun l5(l e)
(cond
((equal l e) nil)
((atom l) (list l))
(t(list (apply #'append (mapcar #'(lambda(l)(l5 l e))l))))
)
)
(defun sum1(l)
(cond
((null l) 0)
((numberp (car l))(+ (car l) (sum1 (cdr l))))
(t(sum1 (cdr l)))
)
)
(defun nv(l lvl)
(cond
((null l) 0)
((atom l) 0)
((and (equal (mod (sum1 l) 2) 0)(equal lvl 1))(apply #'+ 1(mapcar #'(lambda(l)(nv l 1))l)))
(t(apply #'+ (mapcar #'(lambda(l)(cond
((= lvl 1)(nv l (- lvl 1)))
(t(nv l (+ lvl 1)))
)) l)))
)
)

































