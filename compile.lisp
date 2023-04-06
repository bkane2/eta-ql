
(in-package #:eta.compile)

(format t "~%About to load system \"eta\".")
(ql:quickload '#:eta/run)

(setf uiop:*image-entry-point* 'eta/run:run)

(format t "~&eta loaded, about to create executable.")
(uiop:dump-image "/eta/lispapp"
   :executable t
   #+sbcl :compression #+sbcl t)