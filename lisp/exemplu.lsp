(defun fct(l)
(cdr l))


;a)
;lst-lista 
;k-intreg
(defun element1(lst k)
(cond
    ((= k 1) (car lst))
    ((null lst) nil)
    (t(element1 (cdr lst) (- k 1)))
)
)
(print (element1 '(1 2 3 4 (5 6) 7 8 9 10) '6))
;b)
;lst -lista de elemente
;k-intreg elemenentul care e verificat daca este in lista
(defun is_in_list(lst k)
(cond
    ((null lst) nil)
    ((and (atom (car lst)) (equal k (car lst))) t)
    ((atom (car lst)) (is_in_list(cdr lst) k))
    (t (or (is_in_list (car lst) k) (is_in_list(cdr lst) k)))
)
)
(print (is_in_list '(1 2 (3 (4 a) (6 7)) 8 (9 10)) 'a))

;(defun sublist(lst)
;(cond
;((and (null (cdr lst)) (listp(car lst))) (list (car lst)))
;((listp (car lst))(cons (car lst) (sublist (car lst))))
;(t(sublist(cdr lst)))
;)
;)

;(defun sublist(lst)
;(cond
;((not (list lst)) nil)
;((and (null (cdr lst)) (listp(car lst))) (list (car lst)))
;((listp (car lst))(cons (car lst) (cons (sublist (car lst)) (sublist(cdr lst)))))
;((listp (car lst))(list (car lst) (sublist (car lst)) (sublist(cdr lst))))
;((listp (car lst))(cons (car lst) (sublist (cdr lst)))(cons '(sublist(car lst)) '(sublist(cdr lst))))
;(t(cons (sublist(car lst)) (sublist(cdr lst))))
;)
;)

;(defun sublist(lst)
;(cond
;((atom lst) nil)
;(t(apply 'append (list lst) (mapcar 'sublist lst)))
;)
;)

;(defun sublist(lst)
;(cond
;((null lst) nil)
;listp
;
;)

;)

;c)
;lst-lista 
(defun 2d(lst)
(cond
((null lst) nil)
((atom (car lst)) (2d (cdr lst)))
((listp (car lst)) (append (cons (car lst) (2d (car lst))) (2d (cdr lst))))
)
)

;functie wrapper
;lst-lista
(defun submultime(lst)
(cons lst (2d lst))
)



;d)
;functie care sterge toate aparitiile unui element din lista
;lst -lista de elemente
;e-elementul care va fi sters complet din lista
(defun remove_ap(lst e)
(cond
((null lst) nil)
((equal e (car lst))(remove_ap(cdr lst) e))
(t(cons (car lst) (remove_ap(cdr lst) e)))
)
)
(print (submultime '(1 2 (3 (4 5) (6 7)) 8 (9 10))))


;functie care transforma lista intr-o multime
;lst- lista de elemente
(defun transform_set(lst)
(cond
((null lst) nil)
(t(cons (car lst) (transform_set (remove_ap lst (car lst)))))
)
)
(print (transform_set '(1 2 3 44 2 4  5 2 3 3 3 3 4 4 5 77)))

