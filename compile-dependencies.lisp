
(in-package #:cl-user)

(defpackage #:eta.compile
    (:use :cl))

(in-package #:eta.compile)

(load (make-pathname :directory (pathname-directory *load-pathname*) :defaults "eta.asd"))

(ql:quickload
 (asdf:system-depends-on
  (asdf:find-system "eta")))

(setf uiop:*image-entry-point* #'(lambda ()
				   (format t "~%About to load \"compile.lisp\".")
				   (load "compile.lisp")))

(uiop:dump-image "dependencies.core"
   :executable t
   #+sbcl :compression #+sbcl t)
