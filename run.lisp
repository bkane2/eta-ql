(defpackage :eta/run
  (:use :cl :eta)
  (:export #:run))

(in-package :eta/run)

(defvar *port* nil)
(defvar *server* nil)

(defun run ()
  (in-package :eta)

  (setf
    *port*
      (let ((port (uiop:getenv "PORT")))
        (if port
          (parse-integer port)
          8080))
    *server*
      (make-instance 'hunchentoot:easy-acceptor :port *port*))

  (hunchentoot:start *server*)
  (format t "~&Server started on port ~d." *port*)
  (force-output)
  ;; (loop :do (sleep 10))

  (start))