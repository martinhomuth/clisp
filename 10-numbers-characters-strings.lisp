
1.0

(- 5 4)

(/ 3 2)
(mod 10 4)
(rem 10 4)

(setf a (1+ a))

(tan (max 3 5 43 344 34342 23442443 2))

(char= a b)

"foo\"bar"

(format t "foo\"bar")

(string= "foo" "FOO")   ; nil
(string-equal "foo" "FOO") ; t

(string< "foo" "BAR")
(string-lessp "foo" "BAR")

;;; additional restriction arguments

(string= "thisisgod" "thatispeter" :start1 4 :start2 4 :end1 6 :end2 6)

(string<= "thisisgod" "thisisjack")

(string< "foobar" "baz" :start1 3) ;;; 5, not 2 as it's the index of the full word
