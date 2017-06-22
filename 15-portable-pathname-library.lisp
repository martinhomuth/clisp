;;; Read Time Conditionalization

*FEATURES*

;; #+ someboolean => if someboolean is true, the next expression is
;; evaluated, otherwise treated as whitespace
;; #- the inverse

(defun foo()
  #+allegro (format t "allegro stuff~%")
  #+clisp (format t "clisp stuff~%")
  #+cmu (format t "cmu stuff~%")
  #+sbcl (format t "sbcl stuff~%")
  #-(or allegro clisp cmu sbcl) (error "Not implemented"))

(foo)

(defun list-directory ()
  "Portable wrapper around DIRECTORY."
  (format t "peter~%"))

(list-directory)

(directory (make-pathname :name :wild :type :wild :defaults "/home/mhomuth"))

; Checks whether COMPONENT is neither NIL nor :UNSPECIFIC.
(defun component-present-p (_component)
  (and _component (not (eql _component :unspecific))))

; Tests whether PATHNAME is already in directory form.
(defun directory-pathname-p (_pathname)
  (and
   (not (component-present-p (pathname-name _pathname)))
   (not (component-present-p (pathname-type _pathname)))
   _pathname))

; Converts any PATHNAME to a directory form pathname.
(defun pathname-as-directory (_pathname)
  (let ((pathname (pathname _pathname))) ; yes, this looks weird
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames"))
    (if (not (directory-pathname-p _pathname))
	(make-pathname
	 :directory (append (or (pathname-directory pathname) (list :relative))
			    (list (file-namestring pathname)))
	 :name nil
	 :type nil
	 :defaults pathname)
	pathname)))

;; Returns proper wildcard for the given implementation
(defun directory-wildcard (_dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory _dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun list-directory (_dirname)
  (when (wild-pathname-p _dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard _dirname)))

    #+(or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #-(or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implemented")))

(list-directory "/home/martin")


