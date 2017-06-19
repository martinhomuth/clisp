(let ((in (open "testfile")))
  (format t "~a~%" (read-line in))
  (close in))

(let ((in (open "not-existent" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

(let ((in (open "not-existent-will-create" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
       while line do (format t "~a~%" line))
    (close in)))

(let ((in (open "not-existent-error" :if-does-not-exist :error)))
  (format t "~a~%" (read-line in))
  (close in))

(let ((in (open "testfile" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
       while line do (format t "~a~%" line))
    (close in)))


(defparameter *s* (open "something.txt"))
(read *s*)
(close *s*)

;;; Reading binary data
(let ((stream (open "something.txt" :element-type '(unsigned-byte 8))))
  (loop for character = (read-byte stream nil 'eof)
     while (not (eq character 'eof)) do (format t "~a" character))
  (format t "~%")
  (close stream))

;;; Bulk Reads
(let ((stream (open "numbers.txt" :if-does-not-exist nil)))
  (when stream
    (let ((seq (make-array 15 :fill-pointer 0)))
      (loop for sequence = (read-sequence seq stream)
	 while sequence do (format t "~a~%" sequence)))
    )
  (close stream))
