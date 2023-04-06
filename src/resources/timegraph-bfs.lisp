;; (in-package #:timegraph)

;; Breadth first traversal on timegraph
;; The semantics of the search are determined by overlap?, reverse?, and start-at-end?
;; overlap? and reverse? false => returns all episodes which begin after e1.
;; overlap? true, reverse? false => returns all episodes which end after e1.
;; overlap? false, reverse? true => returns all episodes which end before e1.
;; overlap? true, reverse? true => returns all episodes which begin before e1.
;; The start-at-end? option determines where the search starts from. By default (nil)
;; the search starts from the beginning of e1. If T, then the search starts from the
;; end of e1.

(in-package :eta)

(defun bfs (e1 tg &key (overlap? t) (reverse? nil) (start-at-end? nil))
  (let ((seen (make-hash-table :test #'equal))
        (ret ())
        (ref-func (cond ((and overlap? reverse?) #'tp-brefs)
                        (overlap? #'tp-erefs)
                        (reverse? #'tp-erefs)
                        (t #'tp-brefs)))
        (next-func (if reverse? #'get-ancestors #'get-successors)))
    (labels ((%bfs (queue)
               (unless queue
                 (return-from bfs ret))
               (let ((next (remove-if (lambda (tp) (gethash tp seen))
                                      (funcall next-func (car queue)))))
                 (dolist (tp next)
                   (setf (gethash tp seen) t))
                 (setf ret (reduce #'cons (funcall ref-func (car queue))
                                   :initial-value ret
                                   :from-end t))
                 (%bfs (concatenate 'list (cdr queue) next)))))
      (%bfs (list (if start-at-end? (get-end tg e1) (get-beg tg e1)))))))