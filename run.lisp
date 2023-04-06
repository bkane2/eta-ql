(defpackage :eta/run
  (:use :cl :eta)
  (:export #:run))

(in-package :eta/run)

(defun run ()
  (in-package :eta)
  (start))