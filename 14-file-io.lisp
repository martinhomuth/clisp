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

;;; automatic close of opened streams
(with-open-file (stream "testfile" :direction :output :if-exists :append)
  (when stream
    (write-line "this is an appended line" stream)))

;;; Filenames (or Lisps portable pathname objects
(pathname "/etc/resolv.conf")

(pathname-directory (pathname "/etc/resolv.conf"))
(pathname-name (pathname "/etc/resolv.conf"))
(pathname-type (pathname "/etc/resolv.conf"))
(pathname-version (pathname "/etc/resolv.conf"))

;; convert back
(defparameter *pname* (pathname "/etc/resolv.conf"))
(namestring *pname*)
(directory-namestring *pname*)
(file-namestring *pname*)

;;; Pathname Construction
(make-pathname
 :directory '(:absolute "foo" "bar")
 :type "txt"
 :name "baz")

;; better not to build from scratch
(make-pathname :name "peter"
	       :defaults *pname*) ; copies from another file

(delete-file "testfile")

(let ((name "testfile"))
  (with-open-file (out (ensure-directories-exist name)
		       :direction :output
		       :if-exists :append)
    (write-line "this is a test line" out)
    (format t "~a, ~a, ~a"
	    (file-write-date out)
	    (file-author out)
	    (file-length out))))
