;; Aug 5/2020
;; ================================================
;;
;; Contains utility functions used as an interface
;; with the TTT library.
;;

(in-package :eta)

(defun ttt-non-initial-var? (x)
;```````````````````````````````
; Is x a TTT match variable starting with '_'?
;
  (let (chars)
    (cond
      ((not (symbolp x)) nil)
      (t (setq chars (explode x))
        (and
          (char-equal (car chars) #\_)
          (find (second chars)
            '(#\! #\? #\+ #\*) :test 'char-equal))))
)) ; END ttt-non-initial-var? 



(defun ttt-initial-var? (x)
;````````````````````````````
; Is x a TTT match variable starting with on of {! ? + * ^}
; or with <> or {}?
;
  (let (chars)
    (cond
      ((not (symbolp x)) nil)
      (t (setq chars (explode x))
        (or
          (find (car chars)
            '(#\! #\? #\+ #\* #\^) :test 'char-equal)
          (and
            (char-equal (car chars) #\<)
            (char-equal (second chars) #\>))
          (and
            (char-equal (car chars) #\{)
            (char-equal (second chars) #\})))))
)) ; END ttt-initial-var?



(defun ttt-var? (x)
;```````````````````
; Is x a TTT match variable?
;
  (or (ttt-non-initial-var? x) (ttt-initial-var? x))
) ; END ttt-var?



(defun ensure-bound! (x)
; ```````````````````````
; Ensures that x isn't a TTT pred (i.e. any symbol with ? as the last character).
;
  (if (and (symbolp x) (char-equal (car (last (explode x))) #\?)) nil x)
) ; END ensure-bound!



(defun ttt-match-vars (patt)
;````````````````````````````
; Form a list of distinct TTT match-variables that occur in 'patt';
; Duplicate variables that occur earlier in a left-to-right scan are
; discarded.
;
  (let (var vars)
    (cond
      ; Base case - if patt is a symbol, return the pattern if it is a
      ; non-initial var, or nil otherwise
      ((symbolp patt)
        (if (ttt-var? patt) `(,patt) nil))
      ; Recursive case
      (t
        (remove-duplicates
          (remove nil (mapcan #'ttt-match-vars patt))
          :test #'equal))))
) ; END ttt-match-vars



(defun bindings-from-ttt-match (patt expr)
;```````````````````````````````````````````
; From the TTT pattern 'patt', create a rule that generates the
; binding list for the match variables of 'expr', when matched
; to that expression. Apply the rule to 'expr', hence obtain
; a list of bindings. A non-sticky match is assumed.
;
(setq patt (hide-ttt-ops patt))
(setq expr (hide-ttt-ops expr))
(let ((vars (ttt-match-vars patt)) vals)
  (if (null vars) (return-from bindings-from-ttt-match nil))
  (setq vals (ttt:apply-rule `(/ ,patt ,(mapcar #'list vars)) expr :shallow t))
  ; For rules that don't match a given expr, 'ttt:apply-rule' 
  ; returns a result 'eq' to the expr.  Since that's a failure 
  ; case, return nil for it:
  (if (eq vals expr) (return-from bindings-from-ttt-match nil))
  ; Otherwise return the variables matched with their values:
  (mapcar #'list vars (mapcar #'unhide-ttt-ops vals))
)) ; END bindings-from-ttt-match



(defun bindings-from-ttt-match-deep (patt expr)
;``````````````````````````````````````````````````
; Tries to apply bindings-from-ttt-match to all sublists of a list.
; Returns first match.
;
  (let ((res (bindings-from-ttt-match patt expr)))
    (cond
      (res res)
      ((atom expr) nil)
      (t (or (bindings-from-ttt-match-deep patt (car expr))
             (bindings-from-ttt-match-deep patt (cdr expr))))))
) ; END bindings-from-ttt-match-deep



(defun hide-ttt-ops (wff)
;`````````````````````````````````````
; TAKEN FROM Gene's cl-util/ttt.lisp.
; Wrap [..] around symbols like /, !, +, ?, *, @, ~, {}, or <>, or
; ones starting this way, which we may want to use in some patterns
; (e.g., in wff-patterns involving *, **, @, or ~), but can't
; because of their special meanings in TTT. We're assuming that
; the wffs we want to process don't *already* contain symbols in
; square brackets, starting as above inside the brackets, and which
; shouldn't have the brackets removed when we ultimately "unhide"
; the hidden symbols in a formula.
;
  (let (str chars)
       (cond ((symbolp wff)
              (setq str (string wff))
              (setq chars (coerce str 'list))
              (cond ((or (equal chars '(#\^ #\*)) (equal chars '(#\^)))
                     wff)
                    ((or (equal chars '(#\!)) (equal chars '(#\+))
                         (equal chars '(#\?)) (equal chars '(#\*)))
                     wff)
                    ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~ #\/ #\^))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\{) (eq (second chars) #\}))
                     (intern (concatenate 'string "[" str "]")))
                    ((and (eq (car chars) #\<) (eq (second chars) #\>))
                     (intern (concatenate 'string "[" str "]")))
                    (t wff)))
             ((atom wff) wff)
             (t (cons (hide-ttt-ops (car wff))
                      (hide-ttt-ops (cdr wff)))))
)) ; END hide-ttt-ops



(defun unhide-ttt-ops (wff)
;`````````````````````````````````````
; TAKEN FROM Gene's cl-util/ttt.lisp.
; Remove the square brackets that have been added around ttt symbols
; in wff by 'hide-ttt-ops':
;
 (let (str chars)
      (cond ((symbolp wff)
             (setq str (string wff))
             (setq chars (coerce str 'list))
             (cond ((or (not (eq (car chars) #\[))
                        (not (eq (car (last chars)) #\]))) wff)
                   (t (setq chars (cdr (butlast chars)))
                      (setq str (coerce chars 'string))
                      (cond ((null chars) wff)
                            ((member (car chars) '(#\! #\+ #\? #\* #\@ #\~ #\/ #\^))
                             (intern str))
                            ((and (eq (car chars) #\{) (eq (second chars) #\}))
                             (intern str))
                            ((and (eq (car chars) #\<) (eq (second chars) #\>))
                             (intern str))
                            (t wff)))))
            ((atom wff) wff)
            (t (cons (unhide-ttt-ops (car wff))
                     (unhide-ttt-ops (cdr wff)))))
)) ; END unhide-ttt-ops



(defun get-single-binding (bindings)
;````````````````````````````````````
; Retrieves a single bound symbol from the first match variable.
;
  (car (second (car bindings)))
) ; END get-first-single-binding



(defun get-multiple-bindings (bindings)
;```````````````````````````````````````
; Retrieves multiple bound symbols from the first match variable.
;
  (second (car bindings))
) ; END get-first-multiple-bindings