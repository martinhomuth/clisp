(cons 1 2)  ; first is CAR, second is CDR

(car (cons 1 2))
(cdr (cons 1 2))

;;; also SETFable
(defparameter *cons* (cons 1 2))
*cons*
(setf (car *cons*) 10)
*cons*

(cons 1 nil)
(cons 1 (cons 2 nil))
(cons 1 (cons 2 (cons 3 nil)))


(defparameter *list* (list 1 2 3 4))

*list*
(cdr *list*)
(rest *list*)
(car *list*)
(first *list*)
(second *list*)
(third *list*)

(defparameter *newlist* (list "foo" (list 1 2) 10))

(car *newlist*)
(car (cdr *newlist*))

(append (list 1 2) (list 3 4))

(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))
(setf (first *list-2*) 0)
*list-3*
(setf (first *list-1*) 9)
*list-3* ; only *list-2* is shared with the 'new' *list-3*

(setf *list* (list 1 2 3 4))
(setf *list* (reverse *list*))
*list* ; original cons cells are garbage collected

(setf *list* (list 1 2 3 4))
(setf *list* (nreverse *list*))
*list* ; no new cons cells allocation

(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list* (append *list-1* *list-2*))


*list*
(reverse *list*)
*list-2*

*list*
(nreverse *list*)
*list-2* ; magic

(defparameter *lelist* (list 1 2 3 4 5))
(delete 3 *lelist*)
*lelist*

(defparameter *lelist* (list 1 2 3 4 5))
(remove 3 *lelist*) ; non-destructive
*lelist*

(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(upto 10)

(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))
(setf (first *list-2*) 0)

*list-2*
*list-3*
(setf *list-3* (delete 4 *list-3*))

(defparameter *list* (list 2 3 5 6 2 23))
(sort *list* '<)
*list* ; destructive
(defparameter *list* (list 2 3 5 6 2 23))
(defparameter *sorted-list* (sort (copy-list *list*) '<))
*list*
*sorted-list*

(defparameter *list* (list (list 1 2) 2 3 4 5 6))
(caar *list*) === (car (car *list*)) ; up to four As and Ds
(cadadr *list*) === (car (cdr (car (cdr *list*))))

(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3))
(mapcar #'+ (list 1 2 3) (list 10 20 30))
(map 'vector #'+ (list 1 2 3) (list 10 20 30))
