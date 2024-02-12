(defun inserare(l e k)
(cond
((null l) nil)
((equal (mod k 2) 0) (append (list (car l) e) (inserare (cdr l) e (+ 1 k))))
(t(append (list (car l)) (inserare (cdr l) e (+ 1 k))))
)
)

(print (inserare '(1 2 3 4 5 6) 'x 1))


(defun invers(l)
(cond
((null l) nil)
((atom (car l)) (append (invers (cdr l)) (list (CAR L))))
(t(append (invers(cdr l))(invers (car l))))
)
)
(print (invers '(((A B) C)(D E))))


(defun cmmdc(a b)
(cond
((equal b 0) a)
(t (cmmdc b (mod a b)))
)
)
(print (cmmdc 14 18))

(defun functie(l d)
(cond
((null l) d)
((atom (car l)) (functie (cdr l) (cmmdc (car l) d)))
(t(cmmdc (functie (car l) d) (functie (cdr l) d)))
)
)

(defun wrr(l)
(functie l (car l))
)
(print (wrr '(12  (16 24) 44 32)))

(defun na(l n)
(cond
((equal l n) 1)
((atom l) 0)
(t(apply #'+ (mapcar #'(lambda(l)(na l n)) l)))
)
)

(print  (na '(a b(c d(a f) a) (a e) a ) 'a))
 
(defun membru1(l n)
(cond
((equal l n) t)
((atom l) nil)
(t(sau1 (mapcar #'(lambda(l)(membru1 l n))l)))
)
)
(defun sau1(l)
(cond
((null l) nil)
(t(or (car l) (sau1 (cdr l))))
)
)
(print (membru1 '(a (b 2 (c d) (e(f(g))))) 'x))
