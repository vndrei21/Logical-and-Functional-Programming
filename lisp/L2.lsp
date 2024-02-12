(defun adancime(lst e nod)
(cond
((equal nod (car lst)) 0)
((and (NOT(= e 2))(= (cadr lst) 0))(- (adancime (cddr lst) (cadr lst) nod) 1))
((and (= (cadr lst) 0)  (= e 2))(adancime (cddr lst) (cadr lst) nod))
(t(+  1 (adancime (cddr lst) (cadr lst) nod)))
)
)
(defun is_in_list(lst k)
(cond
    ((null lst) nil)
    ((and (atom (car lst)) (equal k (car lst))) t)
    ((atom (car lst)) (is_in_list(cdr lst) k))
    (t (or (is_in_list (car lst) k) (is_in_list(cdr lst) k)))
)
)

(defun adancime_aux(lst nod)
(cond
    ((is_in_list lst nod) (adancime lst 0 nod))
    (t nil)
)
)