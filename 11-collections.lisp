
;;; vectors

(vector)
(vector 1)
(vector 1 2)

#(1 2) ; literal notation for vectors, not defined to be modified

(make-array 0)
(make-array 5 :initial-element 3)

(make-array 5 :fill-pointer 0) ; appears empty but has space for 5 elements

(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*) ; => 0
*x*
(vector-push 'b *x*) ; => 1
*x*
(vector-push 'c *x*) ; => 2
(vector-push 'd *x*) ; => 3
(vector-push 'e *x*) ; => 4
(vector-push 'f *x*) ; => nil - full

(vector-pop *x*) ; => E
*x*

(defparameter *y* (make-array 5 :fill-pointer 0 :adjustable t))

(vector-push 'a *y*)
(vector-push-extend 'e *y*)

(vector-push-extend 'e *x*)
; VECTOR-PUSH-EXTEND works only on adjustable arrays, not on #(A B C D E)

(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'bit)

(defparameter *mystring* (vector 'a 'b 'c 'd))
  *mystring*
(length *mystring*)
(elt *mystring* 1)
(elt *mystring* 2)

; elt is setf-able position
(setf (elt *mystring* 2) 'x)
*mystring*

;;; sequence functions

(count 1 #(1 2 1 2 3 1 2 3 4))  ; => 3
(remove 1 #(1 2 3 4 5 1 1 2 3)) ; => #(2 3 4 5 2 3)
(remove 1 '(1 2 1 2 3 1 2 3 4)) ; => (2 2 3 2 3 4)
(remove #\a "foobarbaz")        ; => "foobrbz"

;;; specify the comparison function using :test keyword
(count 1 #(1 2 1 2 3 1 2 3 4) :test #'<)
;;; specify function to extract key from elements using :key keyword
(find 'c #((a 10) (b 20) (c 30) d(40)) :key 'first)

;;; :from-end t   - search from the end
;;; :start x      - starting index
;;; :end x        - ending index

(defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
(defun verbose-first (x) (format t "Looking at ~s~%" x) (first x))
(count 'a *v* :key 'verbose-first)
