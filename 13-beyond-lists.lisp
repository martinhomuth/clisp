;;; Trees

;; copy list does not copy the internal lists
(defparameter *inner1* (list (list 1 2) (list 3 4)))
(defparameter *inner2* (list (list 4 5) (list 6 7)))
(defparameter *outer* (append *inner1* *inner2*))
*outer*
(defparameter *list-copied* (copy-list *outer*))
*list-copied*
(setf (cadar *inner2*) 9)
*list-copied*

(subst 10 1 '(1 2 (3 2 1) ((1 1) (2 2))))
;; => (10 2 (3 2 10) ((10 10) (2 2)))
;; this does not work on lists itself?
(substitute 10 1 '(1 2 (3 2 1) ((1 1) (2 2))))
;; => (10 2 (3 2 1) ((1 1) (2 2)))

(defparameter *set* ())
*set*
(adjoin 1 *set*)
*set*
(setf *set* (adjoin 1 *set*))
*set*
(pushnew 2 *set*)
*set*
(pushnew 2 *set*)
*set*

(defparameter *membertest* (list 1 2 (list 3 4) (list 2 2)))
*membertest*
(member 2 *membertest*) ; returns cons from the first 2
(member 4 *membertest*) ; nil - does not search in sublists

(subsetp '(1 2 3) '(4 3 2 1)) ; => T
(subsetp '(2 4 2 1) '(3 2 1)) ; => NIL

;;; alists and plists

;; retrieve key given a value
;; shadow mapping which can be removed to reveal the original mapping
