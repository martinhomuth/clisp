
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

(count-if 'zerop #(0 1 2 3 4))
(remove-if 'numberp #(0 1 2 3 4 "hans"))
(position-if-not 'digit-char-p #(#\w #\b #\2 #\,))

(count-if 'evenp #((a 10) (b 25) (c 10) (d 43)) :key 'second)
(remove-if-not 'alpha-char-p #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0)))

(defparameter *someseq* #(1 2 3 4 5))
(defparameter *someotherseq* (copy-seq *someseq*))
(equalp *someseq* *someotherseq*)
(eql *someseq* *someotherseq*)

(reverse *someseq*)

(concatenate 'list #(1 2 3) #(3 4 5))
(concatenate 'string "abs" "olut" "egarbage")

(sort (vector "foo" "bar" "baz") 'string<)

(defparameter *somevec* (vector "foo" "bar" "baz"))
(sort *somevec* 'string>)
*somevec* ; modified

;;; better:
(defparameter *somevec* (vector "foo" "bar" "baz"))
(sort (copy-seq *somevec*) 'string>)

(merge 'string "asdj" "ertn" 'string<)

(subseq "thisissomething" 4)

(defparameter *x* "somethingsfishy")

(setf (subseq *x* 3 7) "xxxx")
*x*

(fill *x* #\w :start 7)

(mismatch "foobarbaz" "foom")
(mismatch #((a 10) (b 20) (c 30)) #((a 10) (b 30) (c 60)) :key 'second)

(every 'evenp #(1 2 3 4 5))
(every 'evenp #(2 2 4 6))

(some 'evenp #(1 2 3 4 5))

(notany 'evenp #(1 2 3 4 5))
(notany 'evenp #(1 3 5))

(notevery 'evenp #(2 4))

(every '< #(1 2 3 4) #(5 6 7 8))

(map 'vector '* #(1 2 3 4) #(5 6 7 8))

(defparameter a (vector 1 2 3))
(defparameter b (vector 4 3 2))
(defparameter c (vector 9 3 2))

(map-into a '+ a b c)

(reduce '+ #( 1 2 3 4 5 6 7 8))


