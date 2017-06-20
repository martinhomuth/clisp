(let ((in (open "testfile")))
  (format t "~a~%" (read-line in))
  (close in)
  )

(let ((in (open "testfile" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)
    )
  )


;;; Output

(let ((out (open "newfile" :direction :output)))
  (when out
    (format t "got an out stream~%")))

(let ((out (open "testfile" :direction :output :if-exists 'nil))) ;
  (when out
    (format t "got an out stream~%")))
;; can be :supersede (overwrite) or :append

(let ((out (open "write-char-file" :direction :output)))
  (when out
    (write-char #\w out)) ; optional argument specifies stream
  (close out))

(let ((out (open "write-line-file" :direction :output)))
  (when out
    (write-line "This is a total line" out)
    (terpri out)
    (close out)))

(let ((out (open "write-string-file" :direction :output)))
  (when out
    (write-string "This is a total line" out)
    (close out)))

(let ((out (open "print-file2" :direction :output :if-exists :supersede))
      (something (list 1 2 3 (list 3 4 5 "hans") #\b "flammenwerfer")))
  (when out
    (format t "printing: ")
    (print something out)
    (terpri out)
    (princ something out)
    (terpri out)
    (pprint something out)
    (close out)))
