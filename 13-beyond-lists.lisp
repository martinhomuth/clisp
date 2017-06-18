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
;; assoc - lookup
(assoc 'a '((a . 1) (b . 2) (c . 3)))
(assoc 'c '((a . 1) (b . 2) (c . 3)))
(assoc 'd '((a . 1) (b . 2) (c . 3)))
;; retrieve the value
(cdr (assoc 'a '((a . 1) (b . 2) (c . 3))))

(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=)
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3))) ; => NIL
;; assoc iterates over the  list from front to back
(assoc 'a '((a . 10) (b . 20) (a . 1) (c . 23))) ; => (A . 10)
;; add pair to the front
(cons (cons 'new-key 'new-value) alist)
(acons 'new-key 'new-value alist)
;; no modification to the list
(setf alist (acons 'new-key 'new-value alist))
;; or
(push (cons 'new-key 'new-value) alist)
;; reverse search for value
(car (rassoc 1 '((a . 1) (b . 2) (c . 3))))

(defparameter *alist-1* '((a . 1) (b . 2) (c . 3)))
(defparameter *alist-copied* (copy-alist *alist-1*))
(setf (cdr (assoc 'b *alist-1*)) 9)
*alist-1*
*alist-copied* ;; does not reference the same objects

(pairlis '(a b c) '(1 2 3))

;; plist
(defparameter *plist* '(A 1 B 2 C 3))
(getf *plist* 'b)

(defparameter *plist* ())
*plist*
(setf (getf *plist* :a) 1)
*plist*
(setf (getf *plist* :a) 2)
*plist*
(setf (getf *plist* :b) 3)
*plist*

(remf *plist* :a)
*plist*
(remf *plist* :z)
*plist*
;; !!! GETF and REMF always use EQ !!!

;; extract multiple properties
(defun process-properties (plist keys)
  (loop while plist do
       (multiple-value-bind (key value tail) (get-properties plist keys)
	 (when key (process-property key value))
	 (setf plist (cddr tail)))))

(get 'symbol 'key) === (getf (symbol-plist 'symbol) 'key)
(setf (get 'some-symbol 'my-key) "information")
(remprop 'symbol 'key) === (remf (symbol-plist 'symbol key))

;; destructuring-bind

(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z))

(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z))
