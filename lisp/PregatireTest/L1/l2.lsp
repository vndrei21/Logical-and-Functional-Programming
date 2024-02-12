;2 b)

;
(defun subl(l)
(cond
    ((atom l) nil)
    (t(list (apply #'append l(mapcar #'subl l))))
)
)
(print (subl '(a (b c(d)) e f(g))))

(defun meme(l n)
(cond
((equal l n) 1)
((atom l) 0)
(t(apply #'+ (mapcar #'(lambda(l)(meme l n))l)))
)
)

(defun listtoset(l x)
(cond
    ((and (atom l) (equal (meme x l) 1))(list l))
    ((atom l) nil)
    (t(apply #'append (mapcar #'(lambda(l)(listtoset l x))l)))
)
)
(print (listtoset '(1 2 2 3 4 4 5)  '(1 2 2 3 4 4 5) ))

(defun toint(l nr)
(cond
((null l) nr)
(t(toint (cdr l)(+ (* nr 10) (car l))))
)
)
(defun main(l)
(toint l 0))
(print (toint '(3 4 5 1 3) 0))

(defun cerinta(l1 l2)
(* (main l1) (main l2))
)

(print (cerinta '(3 4 5 2) '(1 0 0 0)))




(defun adancime1(l)
(cond
((null l) 0)
((atom (car l)) (adancime1(cdr l)))
((listp (car l))(max (+ 1 (adancime1(car l))) (adancime1 (cdr l))))
)
)

(defun adancime(L)
(cond
((null l) nil)
((atom (car l)) (adancime (cdr l)))
((max (adancime1 (car l)) (adancime1 (cdr l))))
)
)

(print (adancime1 '(1 2 3(4 (5 6 (3)) 5) 1 2 (4 5) 2 4)))



(defun sterge(l x)
(cond
((equal l x) nil)
((atom l) (list l))
(t (apply #'append(mapcar #'(lambda(l )(sterge l x))l)))
)
)
(print (sterge '(1 2 3 5  4) 2))


(defun sort(l)
(cond
((null l) nil)
(t(append (list min(l))(sort(sterge l (min l)))))
)
)

(print (sort'(1 5 2 4 7 3)))


)