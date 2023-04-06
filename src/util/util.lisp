;; July 10/19 
;; ================================================
;;
;; Various utility functions used by Eta
;;

(in-package :eta)



;``````````````````````````````````````````````````````
;
; [*] GENERAL UTILITY FUNCTIONS
;
;``````````````````````````````````````````````````````



(defun cdr1 (x)
;````````````````
; Adaptive form of cdr which returns cdr if list contains more than
; two atoms, otherwise it returns the second atom.
;
  (if (cddr x) (cdr x) (cadr x))
) ; END cdr1



(defun copy1 (list)
;````````````````````
; Creates top-level copy of list
;
  (cond
    ((atom list) list)
    (t (cons (car list) (copy1 (cdr list)))))
) ; END copy1



(defun conc1 (l1 l2)
;````````````````````
; Graft l2 onto top-level copy of l1
; Avoids unnecessary CONSing (unlike CONC)
;
  (nconc (copy1 l1) l2)
) ; END conc1



(defun pad (l1 l2)
;``````````````````
; Pads either l1 or l2 to make them both the same length
;
  (let ((len1 (length l1)) (len2 (length l2)))
    (cond
      ((= len1 len2) (list l1 l2))
      ((> len1 len2) (pad l1 (append l2 '(nil))))
      ((< len1 len2) (pad (append l1 '(nil)) l2))))
) ; END pad



(defun shuffle (l)
;``````````````````
; Shuffles top-level elements of a list
;
  (cond
    ((or (null l) (atom l)) l)
    ((= 1 (length l)) l)
    ((= 2 (length l))
      (if (= (random 2) 1)
        (list (first l) (second l))
        (list (second l) (first l))))
    (t (let* ((mid (floor (length l) 2))
              (l1 (butlast l mid)) (l2 (last l mid)))
      (if (= (random 2) 1)
        (append (shuffle l1) (shuffle l2))
        (append (shuffle l2) (shuffle l1))))))
) ; END shuffle



(defun choose-random-element (l)
;``````````````````````````````````
; Chooses a random element from a list
;
  (if (and (listp l) (>= (length l) 1))
    (nth (random (length l)) l))
) ; END choose-random-element



(defun flatten (lst)
;````````````````````````
; Flattens an arbitrary list using mapcar. Note that we can do this fairly easily
; using recursion: use mapcar to flatten each sublist (or if an atom is reached, create a
; list consisting of that atom). Then just append all of the flattened sublists together.
;
  (if (or (null lst) (not (listp lst)))
    (return-from flatten nil))

  ; Recursively flatten list
  (labels
    ((flatten-recur (p)
      (if (atom p)
        (list p)
        (apply #'append (mapcar #'flatten-recur p)))))
    (flatten-recur lst))
) ; END flatten



(defun intersection1 (l)
;`````````````````````````
; Intersection of all sub-lists in l
;
  (cond
    ((null l) nil)
    ((= 1 (length l)) (car l))
    (t (intersection (car l) (intersection1 (cdr l)) :test #'equal)))
) ; END intersection



(defun union1 (l)
;``````````````````
; Union of all sub-lists in l
;
  (cond
    ((null l) nil)
    ((= 1 (length l)) (car l))
    (t (union (car l) (union1 (cdr l)) :test #'equal)))
) ; END union1



(defun replace-n (n l e)
; ```````````````````````
; Replaces the nth element of list l with e
;
  (if (< n (length l))
    (append (butlast l (- (length l) n)) (list e) (last l (- (length l) n 1)))
    l)
) ; END replace-n



(defun >inf (x y)
;``````````````````
; > with symbol 'inf interpreted as infinity, and 'ninf' as negative infinity.
;
  (cond
    ((equal x 'inf)
      (cond ((equal y 'inf) nil) ((equal y 'ninf) t) (t t)))
    ((equal x 'ninf)
      (cond ((equal y 'inf) nil) ((equal y 'ninf) nil) (t nil)))
    (t
      (cond ((equal y 'inf) nil) ((equal y 'ninf) t) (t (> x y)))))
) ; END >inf



(defun >=inf (x y)
;``````````````````
; >= with symbol 'inf interpreted as infinity, and 'ninf' as negative infinity.
;
  (cond
    ((equal x 'inf)
      (cond ((equal y 'inf) t) ((equal y 'ninf) t) (t t)))
    ((equal x 'ninf)
      (cond ((equal y 'inf) nil) ((equal y 'ninf) t) (t nil)))
    (t
      (cond ((equal y 'inf) nil) ((equal y 'ninf) t) (t (>= x y)))))
) ; END >=inf



(defun <inf (x y)
;``````````````````
; < with symbol 'inf interpreted as infinity, and 'ninf' as negative infinity.
;
  (not (>=inf x y))
) ; END <inf



(defun <=inf (x y)
;``````````````````
; <= with symbol 'inf interpreted as infinity, and 'ninf' as negative infinity.
;
  (not (>inf x y))
) ; END <=inf



(defun chars-to-int (chars)
; ``````````````````````````````
; Converts list of chars to integer
;
  (read-from-string (coerce chars 'string))
) ; END chars-to-int



(defun explode (s)
;``````````````````
; The list of the characters making up symbol s
;
  (coerce (string (if (numberp s) (write-to-string s) s)) 'list)
) ; END explode



(defun implode (l)
;``````````````````
; (IMPLODE LIST-OF-CHARACTERS) => a symbol
; The symbol is interned in the current package and has as name a string 
; with the characters in LIST-OF-CHARACTERS.
;
  (intern  (coerce l 'string))
) ; END implode



(defun decompress (input)
;``````````````````````````
; Replaces contractions (e.g. 'don't' or 'dont' by 'do not')
;
  (cond
    ((null input) nil)
    ((and
        (symbolp (car input))
        (get (car input) 'twowords))
      (conc1 (get (car input) 'twowords) (decompress (cdr input))))
    (t (cons (car input) (decompress (cdr input)))))
) ; END decompress



(defun compress (input)
;``````````````````````````
; Replaces auxiliary-NOT combinations by -N'T contractions
;
  (cond
    ((null input) nil)
    ((null (cdr input)) input)
    (t (cond
      ((or
          (not (eq (cadr input) 'not))
          (null (get (car input) 'neg)))
        (cons (car input) (compress (cdr input))))
      (t (cons (get (car input) 'neg) (compress (cddr input)))))))
) ; END compress



(defun find-car (x list)
;`````````````````````````
; Finds a sublist of list which has x as its car (or matches x if atom)
;
  (find x list :test (lambda (y l)
    (or (and (atom l) (equal y l)) (and (listp l) (equal y (car l))))))
) ; END find-car



(defun find-cars-var (x list)
;``````````````````````````````
; Finds a list of sublists of list which have x as its car,
; where x may be either a specific symbol, or a variable (possibly
; with a restrictor).
; NOTE: Currently unused
;
  (if (and x (every #'listp list)) (cond
    ((nnp? x) (remove-if-not (lambda (l) (equal x (car l))) list))
    ((variable? x) list)
    ((restricted-variable? x)
      (let ((r (cadr x))) (remove-if-not
        (lambda (l) (equal (funcall (first r) (car l)) (second r))) list)))
    (t list)))
) ; END find-car-var



(defun find-cars-list (x list)
;```````````````````````````````
; Finds a list of sublists of list which have x as its car (if x is an atom),
; or have some member of x as its car (if x is a list).
;
  (if (and x (every #'listp list)) (cond
    ((atom x) (remove-if-not (lambda (l) (equal (car l) x)) list))
    (t (remove-if-not (lambda (l) (member (car l) x :test #'equal)) list))))
) ; END find-cars-list



(defun front (x &optional (n 1))
;````````````````````````````````
; Does the opposite of last.
;
  (reverse (last (reverse x) n))
) ; END front



(defun get-keyword-contents (lst keys)
;``````````````````````````````````````
; Gets the contents corresponding to a list of keywords in a record structure
; (assuming the contents are a single element immediately following the keyword).
;
  (mapcar (lambda (key)
    (if (keywordp key)
      (second (member key lst)))) keys)
) ; END get-keyword-contents



(defun sym-split (sym n &key front)
;````````````````````````````````````
; Splits a symbol into two symbols at index n from the end.
; (or if :front t is given, n from the front)
;
  (if (numberp sym) (setq sym (write-to-string sym))) ; if sym is a number
  (if (and (atom sym) (> (length (string sym)) n))
    (let ((lex (string sym)))
      (if front
        (list (intern (subseq lex 0 n))
          (intern (subseq lex n (length lex))))
        (list (intern (subseq lex 0 (- (length lex) n)))
          (intern (subseq lex (- (length lex) n) (length lex)))))))
) ; END sym-split



(defun sym-join (lst chr)
;```````````````````````````
; Given a list of symbols, join into one symbol around delimiter symbol.
; 
  (intern (str-join (mapcar #'string lst) chr))
) ; END sym-join



(defun list-split (l sym)
;```````````````````````````
; Given a list, split into multiple lists based on delimiter sym.
; e.g., (list-split '(a b c d b e f) 'b) => '((a) (c d) (e f))
;
  (labels
    ((list-split-recur (l sym temp)
      (cond
        ((equal (car l) sym)
          (append (list (reverse temp)) (list-split-recur (cdr l) sym nil)))
        ((null l) (list (reverse temp)))
        (t (list-split-recur (cdr l) sym (cons (car l) temp))))))
  (list-split-recur l sym nil))
) ; END list-split



(defun str-split (str chr)
;```````````````````````````
; Given a string, split into multiple strings based on a delimiter character.
;
  (mapcar (lambda (l) (coerce l 'string))
    (list-split (explode str) chr))
) ; END str-split



(defun str-join (lst str-or-chr)
;`````````````````````````````````
; Given a list of strings, join into one string around delimiter string/character.
;
  (let ((ret ""))
    (dolist (str lst)
      (setq ret (concatenate 'string ret str (string str-or-chr))))
    (coerce (butlast (explode ret) (length (explode (string str-or-chr)))) 'string)
)) ; END str-join



(defun str-replace (str substr1 substr2)
;`````````````````````````````````````````
; Given a string, and two substrings, replace all occurrences of the first substring
; with the second substring.
;
  (let ((chr (explode str)) (rep (explode substr1)) (new (explode substr2)))
    (labels ((replace-recur (l1 l2 buff)
      (cond
        ((and (null l1) (null l2)) new)
        ((and (null l1) l2) nil)
        ((and l1 (null l2))
          (append new (replace-recur l1 rep nil)))
        ((char-equal (car l1) (car l2))
          (replace-recur (cdr l1) (cdr l2) (cons (car l1) buff)))
        (t (append (reverse buff) (list (car l1))
          (replace-recur (cdr l1) rep nil))))))
      (coerce (replace-recur chr rep nil) 'string)
))) ; END str-replace



(defun str-repeat (str n)
; ``````````````````````````
; Repeats a string n times
;
  (format nil "~v@{~A~:*~}" n str)
) ; END str-repeat



(defun sym-contains (sym char)
;```````````````````````````````
; Returns true if a symbol contains the character given by char.
;
  (if (member char (explode sym) :test #'char-equal) t)
) ; END sym-contains



(defun global-var-to-pred (var &key (pred-type 'v))
;`````````````````````````````````````````````````````
; Converts a global variable (e.g., *test*) to a predicate (e.g., test.v).
;
  (implode (append (cdr (butlast (explode var))) (cons #\. (explode pred-type))))
) ; END global-var-to-pred



(defun print-hash (ht)
; `````````````````````
; Print the contents of a hash table
;
  (maphash (lambda (k v) (format t "k: ~a~%  v: ~a~%" k v)) ht)
) ; END print-hash



(defun parse-chars (chars) 
;```````````````````````````
; Parses a list of chars by forming a list of character sublists,
; where each sublist is made into an atom (taking into account
; special characters)
;
; Takes a character list as input. Then tokenize into a list of
; upper-case atoms, treating (i) any nonblank character following a
; blank, (ii) any non-blank nonalphanumeric character other than
; #\', #\-, #\_, #\[, #\] following an alphanumeric character, and
; (iii) any alphanumeric character following a nonalphanumeric character
; other than #\', #\-, #\_, #\[, #\] as the start of a new atom.
;
  ; remove any newline characters from chars
  (setq chars (remove #\Linefeed (remove #\Return chars)))

  (let (prevch chlist chlists)
    (if (null chars) (return-from parse-chars nil))
    ; Form a list of character sublists, each sublist to be made
    ; into an atom; (the list & sublists will at first be backward,
    ; and so have to be reversed before interning & output)
    (setq prevch #\Space)
    (dolist (ch chars)
      ; Do we have the start of a new word?
      (if
        (or
          (and
            (char-equal prevch #\Space) 
            (not (char-equal ch #\Space)))
          (and
            (alphanumericp prevch)
            (not (alphanumericp ch))
            (not (member ch '(#\Space #\' #\- #\_ #\[ #\]) :test #'char-equal)))
          (and
            (not (alphanumericp prevch))
            (not (member prevch '(#\' #\- #\_ #\[ #\]) :test #'char-equal))
            (alphanumericp ch)))
        ; If so, push the current chlist (if nonempty) onto 
        ; chlists, and start a new chlist containing ch
        (progn (if chlist (push (reverse chlist) chlists))
          (setq chlist (list (char-upcase ch))))
        ; If not, push ch (if nonblank) onto the current chlist
        (if (not (char-equal ch #\Space))
          (push (char-upcase ch) chlist)))
      (setq prevch ch))
        
    ; Push the final chlist (if nonempty) onto chlists (in reverse)
    (if chlist (push (reverse chlist) chlists))
    ; Return the reverse of chlists, where each sublist has been
    ; interned into an atom
    (reverse (mapcar (lambda (x) (intern (coerce x 'string))) chlists))
)) ; END parse-chars



(defun str-to-output (str)
; ``````````````````````````
; Converts a string to a list of words/punctuation to output
; TEST: "The next step be putting the Twitter block on the Texaco block."
; 
  (let ((char-list (coerce str 'list)) word words number-flag res)
    (dolist (c char-list)
      (cond
        ; If space, add accumulated word to word list and clear word
        ((member c '(#\ ) :test #'char-equal)
          (if word (setq words (cons (reverse word) words)))
          (setq word nil))
        ; If character is digit, ignore periods until the next non-digit
        ((digit-char-p c)
          (setq number-flag t)
          (setq word (cons c word)))
        ; If punctuation, add accumulated word to word list, clear word,
        ; and add punctuation to word list
        ((member c '(#\, #\' #\") :test #'char-equal)
          (if word (setq words (cons (reverse word) words)))
          (setq word nil)
          (setq words (cons (intern (coerce (list c) 'string)) words)))
        ; If period, check whether currently reading a number
        ((and (member c '(#\.) :test #'char-equal) (not number-flag))
          (if word (setq words (cons (reverse word) words)))
          (setq word nil)
          (setq words (cons (intern (coerce (list c) 'string)) words)))
        ; If non-digit and currently reading a number, add accumulated word
        ; to word list, splitting off the final period (if any)
        ((and (not (digit-char-p c)) (not (equal #\. c)) number-flag)
          (cond
            ((and word (equal #\. (car word)))
              (if (cdr word) (setq words (cons (reverse (cdr word)) words)))
              (setq words (cons (intern ".") words)))
            (t (setq words (cons (reverse word) words))))
          (setq word (list c))
          (setq number-flag  nil))
        ; Otherwise, add current character to accumulated word
        (t
          (setq word (cons c word)))))
    ; Add any remainder word
    (cond
      ((and word (equal #\. (car word)))
        (if (cdr word) (setq words (cons (reverse (cdr word)) words)))
        (setq words (cons (intern ".") words)))
      (word (setq words (cons (reverse word) words))))

    ; Remove any NIL values from words
    (setq words (remove nil words))

    ; Read list of word symbols from list of strings.
    (setq res (reverse (mapcar (lambda (w)
      (if (listp w) (read-from-string (coerce w 'string)) w)) words)))
    ; Ensure any numbers are symbols
    (mapcar (lambda (w) (if (numberp w) (intern (write-to-string w)) w)) res))
) ; END str-to-output



(defun deepcopy-hash-table (old)
;```````````````````````````````
; Deep copy a hash table.
;
  (when (not (hash-table-p old))
    (return-from deepcopy-hash-table (copy-tree old)))
  (let ((new (make-hash-table
              :test (hash-table-test old)
              :size (hash-table-size old))))
    (maphash (lambda (k v)
        (setf (gethash (copy-tree k) new) (copy-tree v)))
      old)
    new
)) ; END deepcopy-hash-table



;``````````````````````````````````````````````````````
;
; [*] TYPE-CHECKING PREDICATES
;
;``````````````````````````````````````````````````````



(defun symbol? (x)
;``````````````````
; Returns t if x is a symbol, nil otherwise.
;
  (symbolp x)
) ; END symbol?



(defun fbound? (x)
;`````````````````````
; Returns t if x is a bound function, nil otherwise.
;
  (and (symbolp x) (fboundp x))
) ; END fbound?



(defun not-fbound? (x)
;`````````````````````````
; Returns t if x is not a function, nil otherwise.
;
  (not (fbound? x))
) ; END not-fbound?



(defun global-var? (x)
;`````````````````````````
; Returns t if x is a global var (e.g., *variable*), nil otherwise.
;
  (and (equal (car (explode x)) #\*) (equal (car (last (explode x))) #\*))
) ; END global-var?
 


(defun ulf-symbol? (atm)
;```````````````````````
; Check whether a symbol is a ULF symbol.
; (A bit hacky; a proper way would be to enumerate
; all the type extensions supported by ULF.)
;
  (let ((parts (list-split (explode atm) #\.)))
    (and (= 2 (length parts)) (first parts) (second parts)
         (not (equal (car (second parts)) #\ )))
)) ; END ulf-symbol?



(defun restricted-variable? (atm)
;``````````````````````````````````
; Check whether a symbol is a variable, or a variable with a restriction following it.
;
  (or (variable? atm) (and (listp atm) (variable? (car atm))))
) ; END restricted-variable?



(defun variable? (atm)
;`````````````````````
; Check whether a symbol is a variable, i.e. starts with '?', '!'.
; NOTE: this excludes indexical variables, such as '^you'.
;
  (and (symbolp atm) (member (car (explode atm)) '(#\? #\!) :test #'char-equal)
    (not (or (equal atm '?) (equal atm '!))))
) ; END variable?



(defun lambda-descr? (lst)
;```````````````````````````
; Check whether a list is a lambda abstract, e.g.,
; (:l (?x ?y) (and (?x on.p ?y) (not (?x red.a))))
;
  (and (listp lst) (= 3 (length lst)) (equal :l (first lst))
       (listp (second lst)) (every #'variable? (second lst))
       (listp (third lst)))
) ; END lambda-descr?



(defun record-structure? (lst)
;```````````````````````````````
; Checks whether a list is a record structure, e.g.,
; ($ loc :x 1 :y 2 :z 0)
  (and (listp lst) (cddr lst) (equal '$ (car lst))
       (symbolp (second lst)))
) ; END record-structure?



(defun get-variables (lst)
;``````````````````````````
; Returns a list of all unique variables in lst.
;
  (remove-duplicates (remove nil
    (mapcar #'(lambda (x) (if (variable? x) x nil)) 
      (flatten lst))))
) ; END get-variables



(defun answer-list? (list)
;``````````````````````````
; Check whether a list is a list of propositional answers with associated certainties.
; TODO: needs cleaning
;
  (and (listp list) (every (lambda (l)
      (and (listp l) (= 3 (length l)) (numberp (third l)) (equal (second l) 'certain-to-degree)
          (listp (first l)) (equal (first (first l)) 'that)
          (or (np? (second (first l))) (symbolp (second (first l)))
              (and (listp (second (first l))) (symbolp (car (second (first l)))))
              (and (listp (second (first l))) (np? (car (second (first l))))))))
    list))
) ; END answer-list?



(defun prop-var? (atm)
;`````````````````````
; Check whether a symbol is a proposition variable, i.e. ends with .
;
  (and (variable? atm) (char-equal #\. (car (last (explode atm)))))
) ; END prop-var?



(defun ep-var? (atm)
;`````````````````````
; Check whether a symbol is an episode variable, i.e. does not end with .
;
  (and (variable? atm) (not (prop-var? atm)))
) ; END ep-var?



(defun function? (atm)
;``````````````````````
; Check whether a symbol is a function, i.e. ends with '.f'
;
  (and (symbolp atm) (equal '(#\. #\F) (last (explode atm) 2)))
) ; END function?



(defun ground-wff? (wff)
;`````````````````````````
; Check whether a wff is a ground wff, i.e. it has no variables
;
  (cond
    ((atom wff) (not (variable? wff)))
    (t (every #'ground-wff? wff)))
) ; END ground-wff?



(defun quoted-list? (list)
;````````````````````````````````````
; Is list of form (quote (...))?
;
  (if (and
        (listp list)
        (eq (car list) 'quote)
        (listp (second list))) t nil)
) ; END quoted-list?



(defun sentence? (list)
;```````````````````````````
; A list is a sentence if it is a flat list of atoms
; without any ULF type extensions.
;
  (if (and
        (listp list)
        (every #'atom list)
        (every (lambda (atm) (not (ulf-symbol? atm))) list)) t nil)
) ; END sentence?



(defun quoted-sentence? (list)
;````````````````````````````````````````````````
; Is list of form (quote (word1 word2 ... wordn)) ?
;
  (if (and
        (quoted-list? list)
        (sentence? (second list))) t nil)
) ; END quoted-sentence?



(defun quoted-sentence-list? (list)
;````````````````````````````````````````````````
; Is list of the form (quote ((word1 ... wordn) ... (word1 ... wordm))) ?
;
  (if (and
        (quoted-list? list)
        (every #'sentence? (second list))) t nil)
) ; END quoted-sentence-list?



(defun question? (sentence)
;```````````````````````````````````````
; Is sentence of form (quote (<word> ... <word> ?)), or with the
; question mark attached to the last word? (One could make more
; elaborate checks that would also work w/o a question mark, using
; patterns (... <aux> you{r} ...), (... <wh-word> <word> you{r} ...),
; etc.). But we assume we have ensured that output questions end in
; "?".
;
  (let (word)
    ;; (format t "~% ****** question? first line = ~a **** ~%" (listp sentence))
    ;; (format t "~% ****** question? third line = ~a **** ~%" sentence) ; DEBUGGING
    (if (and (listp sentence) (every #'atom sentence))
      (setq word (car (last sentence)))
      (return-from question? nil))
    (or
      (eq word '?)
      (char-equal #\? (car (last (explode word))))))
) ; END question?



(defun quoted-question? (sentence)
;```````````````````````````````````````
; Is sentence of form (quote (<word> ... <word> ?)), or with the
; question mark attached to the last word? (One could make more
; elaborate checks that would also work w/o a question mark, using
; patterns (... <aux> you{r} ...), (... <wh-word> <word> you{r} ...),
; etc.). But we assume we have ensured that output questions end in
; "?".
;
  (let (word)
    ;; (format t "~% ****** quoted-question? first line = ~a **** ~%" (listp sentence))
    ;; (format t "~% ****** quoted-question? third line = ~a **** ~%" sentence) ; DEBUGGING
    (if (quoted-sentence? sentence)
      (setq word (car (last (second sentence))))
      (return-from quoted-question? nil))
    (or
      (eq word '?)
      (char-equal #\? (car (last (explode word))))))
) ; END quoted-question?



(defun punct? (atm)
;```````````````````````
; Checks whether an atom is a punctuation symbol.
;
  (and (symbolp atm) (member atm '(? ! \, \. \: \;)))
) ; END punct?



(defun nonpunct? (atm)
;```````````````````````
; Checks whether an atom is not a punctuation symbol.
;
  (and (symbolp atm) (not (punct? atm)))
) ; END nonpunct?



(defun presubst-nonblocker? (atm)
;`````````````````````````````````
; Checks whether an atom is not a particular conjunction or subordinating
; verb that blocks 'you from being substituted with 'you2 (for use by
; the presubst function).
;
  (and (symbolp atm) (not (or (punct? atm) (member atm
    '(and or but that because if so when then why think see guess
      believe hope do can would should than know i you - --)))))
) ; END presubst-nonblocker?



(defun relative-speech-act? (list)
;`````````````````````````````````````
; Checks whether a given list is a relative speech act WFF;
; that is, contains an episode constant as an argument.
; e.g., (^you reply-to.v E1)
;
  (and (= (length list) 3)
       (member (second list) *speech-acts*)
       (or (symbolp (third list)) (is-set? (third list))))
) ; END relative-speech-act?



(defun tag? (atm)
;````````````````````````````````````````````````
; If symbol is surrounded by [...], classify it as a tag.
;
  (if (and (symbolp atm)
        (equal #\[ (car (explode atm)))
        (equal #\] (car (last (explode atm)))))
    t nil)
) ; END tag?



(defun emotion-tag? (atm)
;````````````````````````````````````````````````
; If symbol is included in *emotions-list*,
; it qualifies as an emotion tag when inside an output list.
;
  (if (and (symbolp atm) (member atm *emotions-list*)) t nil)
) ; END emotion-tag?



;``````````````````````````````````````````````````````
;
; [*] DISCOURSE UTIL
;
;``````````````````````````````````````````````````````

(defparameter *underlying-feat* (make-hash-table)) ; contains feat for dot-predicates of form .feat
(defparameter *isa-features* (make-hash-table)) ; the table with 'isa' data, linking atoms to dot-features



(defun isa (x feat)
;````````````````````
; x:    a symbol (e.g., 'surgeon' or 'doctor')
; feat: another symbol (e.g., 'professional'). In general, there is
;       no clear presumed logical relation between x and feat -- feat
;       simply "indicates" the sort of thing x refers to, in some sense.
;
; NB: an atom is always assumed to have itself as a feature; e.g., 
;     (isa 'this 'this) is true. So using '.occupation' in a pattern
;     will match 'occupation', even if the pattern coder wrote down
;       (occupation physicist cobbler poet teacher ...)
;     with the intention of using '.occupation' to match 'physicist', etc.,
;     in a sentence like "Alice is a physicist", and concluding "Alice
;     is employed". The sentence "Computer programming is an occupation"
;     would then likewise lead to the conclusion "Computer programming is
;     employed"! The moral is, if you want to use a match-predicate that
;     is true of various expressions but not itself, define it as a
;     !predicate rather than a feature.
;
; Method:
;     Look up x in the *isa-features* hash table and see if feat appears in the
;     retrieved list or in the lists (in the same table) associated with the
;     retrieved features, etc., while guarding against loops. Return T if 
;     the feature is found and NIL otherwise. Case is ignored.
;
  (prog (ff f closed fff)
        (if (eq x feat) (return t))
        ; The next 3 lines aren't needed for the 'match' function, which
        ; looks in hash table *underlying-feat* to find the feature corres-
        ; ponding to a dot-atom. However, in the syntax tree preprocessing 
        ; rules, we use some predicates that directly call "isa", where the
        ; second argument is a dotted atom. The next few lines remove the dot.
        (if (dot-atom x) 
            (setq x (intern (string-left-trim "." (string x)))))
        (if (dot-atom feat) 
            (setq feat (intern (string-left-trim "." (string feat)))))
        (if (eq x feat) (return t))
        ; frequent case: feat is on the list retrieved from *isa-features* for x:
        (setq ff (gethash x *isa-features*))
        (if (find feat ff) (return t))
        ; maybe a feature on list ff has feature feat, etc. (iteration)
        (if (null ff) (return nil))
        (setq closed (list x feat)); ff will serve as the open list
        (loop (setq f (pop ff))
              (when (not (find f closed))
                    (setq fff (gethash f *isa-features*))
                    (when fff (if (find feat fff) (return-from isa t))
                              (push f closed)
                              (setq ff (union fff ff))
                              (setq ff (remove f ff))); just in case
                    (if (null ff) (return-from isa nil))))
)) ; END isa



(defun attachfeat (feat-xx)
;````````````````````````````
; feat-xx: a list of form (feat x1 x2 ... xk)
;       where feat is a symbol, regarded as a feature
;       & x1, x2, ... are symbols (perhaps allowing expressions in future?) 
;       that will hereby be assigned feat, i.e., (isa xi feat) will be
;       true for each xi among x1, x2, ..., xk.
; We store feat as a feature of x1, x2, ..., xk in the *isa-features* table.
; We avoid duplication, for any xi that already has that feature.
; We also store feat under key .feat (the dot-prefixed version of feat)
; in table *underlying-feat*, for easy access to feat from .feat in the 
; 'match' function. 
;
 (let* ((feat (car feat-xx)) 
        (dot-pred (intern (concatenate 'string "." (string feat)))))
       (setf (gethash dot-pred *underlying-feat*) feat)
       (dolist (x (cdr feat-xx))
          (if (not (isa x feat))
              (push feat (gethash x *isa-features*))))
)) ; END attachfeat



(defun store-gist (gist keys kb)
;`````````````````````````````````
; Put 'gist' into the 'kb' (a hash table) using the given keys.
; Avoid duplication of already stored gists. This is intended 
; primarily for acquired gists (as gist clauses) about the user,
; but should also be usable for gists about Eta, that Eta
; could consult in answering questions from the user.
;
  (let ((gists (remove nil (mapcar (lambda (key) (gethash key kb)) keys))))
    (if (not (member gist gists :test #'equal))
      (mapcar (lambda (key)
          (setf (gethash key kb) (cons gist (gethash key kb))))
        keys))
)) ; END store-gist



(defun find-prev-turn-of-agent (agent)
;``````````````````````````````````````````````
; Finds the most recent turn in the conversation log from the given agent.
;
  (car (remove-if (lambda (turn) (not (equal (dialogue-turn-agent turn) agent)))
    (ds-conversation-log *ds*)))
) ; END find-prev-turn-of-agent



(defun detach-final-punctuation (wordlist)
;```````````````````````````````````````````
; Ensures that the punctuation at the end of the word list is
; a separate token, and not attached to the final word.
; 
  (let* ((lastword (car (last wordlist))) (chars (explode lastword))
        ch punc)
    (if (null wordlist) (return-from detach-final-punctuation nil))
    (if (= (length chars) 1)
      (return-from detach-final-punctuation wordlist))
    (cond
      ((setq ch (find (car (last chars)) '(#\. #\? #\! #\;)))
        (setq lastword (implode (butlast chars)))
        (setq punc (implode (list ch)))
	      (append (butlast wordlist) (list lastword) (list punc)))
      (t wordlist))
)) ; END detach-final-punctuation



(defun nil-gist-clause? (gist-clause)
;`````````````````````````````````````
; Return t if gist-clause is the nil gist clause (i.e. '(NIL Gist ...)).
;
  (and (>= (length gist-clause) 2) (equal (subseq gist-clause 0 2) '(NIL GIST)))
) ; END nil-gist-clause?



(defun nil-gist-question? (gist-clause)
;`````````````````````````````````````
; Return t if gist-clause is the nil gist question (i.e. '(NIL Question ...)).
;
  (and (>= (length gist-clause) 2) (equal (subseq gist-clause 0 2) '(NIL QUESTION)))
) ; END nil-gist-question"



(defun purify-func (user-gist-clauses)
;````````````````````````````````````````
; Remove user gist clauses identical with '(NIL Gist ...) or '(NIL Question ...), unless it is the only
; gist clause (note: prefer nil question to nil gist).
; Also remove duplicate gist clauses.
;
  (remove-duplicates
  (let ((purified-gist-clauses (remove-if (lambda (x) (or (nil-gist-clause? x) (nil-gist-question? x))) user-gist-clauses)))
    (if purified-gist-clauses purified-gist-clauses
      (let ((purified-gist-questions (remove-if (lambda (y) (not (nil-gist-question? y))) user-gist-clauses)))
        (if purified-gist-questions (list (car purified-gist-questions)) (list (car user-gist-clauses))))))
  :test #'equal)
) ; END purify-func



(defun gist-contradiction (current-gist-list gist-clause)
;`````````````````````````````````````````````````````
; Finds gist clauses which contradict one another.
;
  (let (cont-flag)
    (loop for ix from 1 to (list-length current-gist-list) do
      (if (equal (car (or
                  (set-difference (nth (- ix 1) current-gist-list)
                    gist-clause)
                  (set-difference gist-clause
                    (nth (- ix 1) current-gist-list))))
          'NOT)
        (setf cont-flag t)))
  cont-flag)
) ; END gist-contradiction



(defun remove-contradiction (gist-list)
;`````````````````````````````````````````````````````
; Remove any contradicting gist clauses (get rid of the
; latter one, which appears later in the conversation).
;
  (cond
    ((<= (list-length gist-list) 1) gist-list)
    ((gist-contradiction (butlast gist-list) (car (last gist-list)))
      (butlast gist-list))
    (t (append
      (remove-contradiction (butlast gist-list))
      (last gist-list))))
) ; END remove-contradiction



(defun split-sentences (words)
;```````````````````````````````
; Given the list of words 'words', split into multiple lists of words for each sentence,
; delimited by punctuation.
;
  (let (result cur)
    ; Loop through each word, keeping a buffer which is emptied once punctuation is reached
    (mapcar (lambda (word)
        (cond
          ((member word '(|.| ? !))
            (setq result (cons (reverse (cons word cur)) result))
            (setq cur nil))
          (t
            (setq cur (cons word cur)))))
      words)
    ; Empty buffer (in case the last sentence is missing punctuation)
    (when cur
      (setq result (cons (reverse cur) result)))
  (reverse result))
) ; END split-sentences



(defun form-chunks (words) ; NOTE: currently unused
;```````````````````````````
; Given the list of words 'words', form 10-word chunks overlapping by
; 5 words; if there are just 15 words or less, form a single chunk,
; i.e., a singleton list containing the list of words. For non-initial
; chunks whose first word is preceded in 'words' by a negative word,  
; add that word to the beginning of the chunk (so that negatives
; won't be mistaken for positives).
;
  (let ((n (length words)) chunks chunk negword changed result)
    (cond
      ; If less than 15 words, return only 1 chunk
      ((< n 15) 
        (return-from form-chunks (list words)))
      ; Otherwise, form multiple word chunks, 10 words long,
      ; overlapping by 5 words
      (t
        (loop
          (setq n (- n 5))
          (setq chunk (butlast words (- n 5)))
          (setq words (last words n))
          (push chunk chunks)
          (when (<= n 10)
            (push words chunks)
            (return nil)))
        (setq chunks (reverse chunks))
        ; 'negword' copy-over:
        (dolist (chunk chunks)
          ;; (format t "~%negword = ~a" negword) ; DEBUGGING
          ;; (format t "~%  chunk = ~a" chunk) ; DEBUGGING
          (when negword (push negword chunk)
                        (setq changed t))
          (push chunk result)
          (setq negword
            (find (car (last chunk 6))
                  '(no not don\'t cannot can\'t won\'t couldn\'t
                    wouldn\'t never hardly))))
        (if changed (reverse result) chunks)))
)) ; END form-chunks



(defun swap-duals (wordlist)
;``````````````````````````````
; Swaps dual words in a word list; e.g., changes YOU ARE to I AM, and
; I AM to YOU ARE, and similarly for other words.
;
  (dual (presubst wordlist))
) ; END swap-duals



(defun tag-emotions (resp)
;````````````````````````````
; Prepends each response utterance with a default [NEUTRAL] tag unless it
; already has an explicit emotion tag. If *emotions* is NIL, strip
; all emotion tags from response, otherwise return tagged response.
; Also remove non-emotion tags (at the moment)
;
  (let ((tagged-resp (if (some #'emotion-tag? resp) resp (cons '[NEUTRAL] resp))))
    (remove-unsupported-tags
      (if *emotions* tagged-resp (remove-if #'emotion-tag? tagged-resp)))
)) ; END tag-emotions



(defun split-emotion-tag (resp)
;`````````````````````````````````
; Splits an emotion tag (if any) from a response.
;
  (list (car (remove-if-not #'emotion-tag? resp)) (remove-if #'emotion-tag? resp))
) ; END split-emotion-tag



(defun remove-unsupported-tags (resp)
;```````````````````````````````````````
; Removes tags from an utterance that aren't currently supported.
; Note that we also need to remove cases where the tag might be split up
; across multiple symbols (this is sometimes the case with GPT3-generated responses),
; although perhaps this should be dealt with in the parse-chars function instead.
;
  (let (ret (include t))
    (setq resp (remove-if (lambda (word) (and (tag? word) (not (emotion-tag? word)))) resp))
    (dolist (word resp)
      (if (and (equal #\[ (car (explode word)))
               (not (equal #\] (car (last (explode word))))))
        (setq include nil))
      (if include (push word ret))
      (if (equal #\] (car (last (explode word)))) (setq include t)))
    (reverse ret)
)) ; END remove-unsupported-tags



(defun untag-emotions (resp)
;```````````````````````````````
; Removes emotion tags from an utterance.
;
  (remove-if #'emotion-tag? resp)
) ; END untag-emotions



(defun get-emotion (resp)
;````````````````````````````
; Gets the emotion of a response from the tag, as a string.
;
  (if *emotions*
    (string-downcase (implode (cdr (butlast (explode (car resp))))))
    "neutral")
) ; END get-emotion



(defun dual (sentence)
;``````````````````````
; Replaces 'I' by 'you', 'you' by 'I', 'my' by 'your', etc.
;
  (cond
    ((null sentence) nil)
    ((numberp (car sentence))
      (cons (car sentence) (dual (cdr sentence))))
    ((null (get (car sentence) 'subst))
      (cons (car sentence) (dual (cdr sentence))))
    (t (cons (get (car sentence) 'subst)
      (dual (cdr sentence)))))
) ; END dual



(defun duals (word1 word2)
;``````````````````````````
; Forms duals between two words
;
  (progn (setf (get word1 'subst) word2)
         (setf (get word2 'subst) word1))
) ; END duals



(defun presubst (response)
;`````````````````````````````````````
; This function is applied to eta's responses before 
; their "dual" is formed and printed out. It helps avoid 
; outputs like
;      WHY DO YOU SAY I ARE STUPID
; (as the dual of WHY DO I SAY YOU ARE STUPID), while
; still correctly producing
;      WHY DO YOU SAY YOUR BROTHERS ARE STUPID
; (as the dual of WHY DO I SAY YOUR BROTHERS ARE STUPID).
;
; It replaces ARE by ARE2 when preceded or followed by YOU
; (In turn, DUAL will replace YOU by I and ARE2 by AM, so
; that YOU ARE becomes I AM (whereas WE ARE, THEY ARE, etc.,
; remain unchanged). Similarly, it replaces WERE by WERE2,
; and WAS by WAS2.
;
; It also replaces YOU by YOU2 when it is the last word, or
; when it is not one of the first two words and is not preceded
; by certain conjunctions (AND, OR, BUT, THAT, BECAUSE, IF, WHEN,
; THEN, WHY, ...) or by certain subordinating verbs (THINK, BELIEVE,
; KNOW,...), or when it follows 'to'.
;
; This is in preparation for replacement of YOU2 by ME
; (rather than I) when DUAL is applied.
;
  (ttt:apply-rules '(
    ; are -> are2
    (/ (_*1 you are _*2) (_*1 you1 are2 _*2))
    (/ (_*1 are you _*2) (_*1 are2 you1 _*2))
    ; was -> was2
    (/ (_*1 I was _*2) (_*1 I was2 _*2))
    (/ (_*1 was I _*2) (_*1 was2 I _*2))
    ; were -> were2
    (/ (_*1 you were _*2) (_*1 you1 were2 _*2))
    (/ (_*1 were you _*2) (_*1 were2 you1 _*2))
    ; you -> you2
    (/ (_*1 you punct? _*2) (_*1 you2 punct? _*2))
    (/ (_*1 to you _*2) (_*1 to you2 _*2))
    (/ (_!1 (! presubst-nonblocker?) you _*) (_!1 ! you2 _*))
    (/ (_!1 _!2 (* nonpunct?) (! presubst-nonblocker?) you _*) (_!1 _!2 * ! you2 _*))
    (/ (_*1 punct? _!1 (! presubst-nonblocker?) you _*2) (_*1 punct? _!1 ! you2 _*2))
    (/ (_*1 punct? _!1 _!2 (* nonpunct?) (! presubst-nonblocker?) you _*2) (_*1 punct? _!1 _!2 * ! you2 _*2)))
  response)
) ; END presubst



(defun print-gist-kb (&key filename)
;`````````````````````````````````````
; Prints all gist clauses in the gist-kb for a user.
; If a file is specified, append to that file.
;
  (if filename
    (with-open-file (outfile filename :direction :output :if-exists :supersede :if-does-not-exist :create)))

  (maphash (lambda (key val)
    (if (not (nil-gist-clause? (car val)))
      (if filename
        (with-open-file (outfile filename :direction :output :if-exists
                                          :append :if-does-not-exist :create)
          (format outfile "~% ~a   ~a" key val))
        (format t "~% ~a   ~a" key val))))
  (ds-gist-kb-user *ds*))
) ; END print-gist-kb



;``````````````````````````````````````````````````````
;
; [*] MEMORY UTIL
;
;``````````````````````````````````````````````````````



(defun print-memory (&key verbose key)
;````````````````````````````````````````
; Prints facts in the memory. When verbose is given as true,
; print all keys and associated facts as well. If key is given,
; print only the list of facts stored under that key.
;
  (let (l1 l2)
    (maphash (lambda (k v)
      (if (equal v t) (setq l1 (cons k l1))
        (setq l2 (cons (list k v) l2)))) (ds-memory *ds*))
    (mapcar (lambda (f)
      (format t "~a~%" f)) l1)
    (when (or verbose key)
      (format t "------------------------------------ ~%")
      (mapcar (lambda (f)
        (if (or (null key) (equal (first f) key))
          (format t "~a: ~a~%~%" (first f) (second f)))) l2)))
) ; END print-memory



(defun store-in-memory (wff)
;```````````````````````````````
; Stores a wff in memory.
; 
  (let ((wff1 (if (equal (car wff) 'quote) (eval wff) wff)))
    (store-fact wff (ds-memory *ds*)))
) ; END store-in-memory



(defun get-from-memory (pred-patt)
;````````````````````````````````````
; Retrieves a fact from memory.
;
  (get-matching-facts pred-patt (ds-memory *ds*))
) ; END get-from-memory



(defun get-from-memory-characterizing-episode (pred-patt ep-name)
;``````````````````````````````````````````````````````````````````
; Given an episode name, find all facts in memory characterizing (or partially characterizing)
; that episode, and return the ones matching pred-patt.
; 
  (let (facts matches)
    (setq facts (mapcar #'first
      (append (get-from-memory `(?p ** ,ep-name)) (get-from-memory `(?p * ,ep-name)))))
    (dolist (fact facts)
      (when (fact-matches-p pred-patt fact)
        (setq matches (cons fact matches))))
    matches
)) ; END get-from-memory-characterizing-episode



(defun get-episode-from-contextual-fact (pred-patt)
;```````````````````````````````````````````````````````````````````
; Given a fact, find the episode-name that that fact characterizes (or
; partially characterizes). The fact must be currently true in context
; for the corresponding episode to be returned.
;
  (let (ep-names)
    (setq ep-names (mapcar #'third
      (append (get-from-memory `(,pred-patt ** ?e)) (get-from-memory `(,pred-patt * ?e)))))
    ; Remove any episodes that aren't currently true in context
    (setq ep-names
      (remove-if-not (lambda (ep-name) (get-from-memory `(NOW* during ,ep-name))) ep-names))
    ; If there are multiple episodes, return the first one in the list
    ; TODO: this should be modified to return the most temporally recent episode (using timegraph
    ; or direct episode relations in memory) eventually.
    (car ep-names)
)) ; END get-episode-from-contextual-fact



(defun get-utterances-characterizing-episode (ep-name)
;``````````````````````````````````````````````````````````````````
; Given an episode name, find all surface utterances characterizing it.
;
  (unwrap-utterances (get-from-memory-characterizing-episode 'say-to.v ep-name))
) ; END get-utterances-characterizing-episode



(defun get-gist-clauses-characterizing-episode (ep-name)
;``````````````````````````````````````````````````````````````````
; Given an episode name, find all gist-clauses characterizing it.
; NOTE: split any gist-clauses with [SEP] delimiters into multiple gist-clauses;
; currently does nothing but useful if multiple gist-clauses need to be concatenated
; before storage and split later.
;
  (apply #'append (mapcar (lambda (clause) (list-split clause '[SEP])) 
    (unwrap-gist-clauses (get-from-memory-characterizing-episode 'paraphrase-to.v ep-name))))
) ; END get-gist-clauses-characterizing-episode



(defun get-semantic-interpretations-characterizing-episode (ep-name)
;``````````````````````````````````````````````````````````````````
; Given an episode name, find all semantic interpretations characterizing it.
;
  (unwrap-semantic-interpretations (get-from-memory-characterizing-episode 'articulate2-to.v ep-name))
) ; END get-semantic-interpretations-characterizing-episode



(defun store-gist-clause-characterizing-episode (gist-clause ep-name subj obj)
;```````````````````````````````````````````````````````````````````````````````
; Given a gist-clause and episode name, store a paraphrase-to.v fact with given
; subject and object (e.g. ^me and ^you) partially characterizing the episode.
; Store the wff in context as well.
;
  (when (null gist-clause) (return-from store-gist-clause-characterizing-episode nil))
  (let ((wff `(,subj paraphrase-to.v ,obj ',gist-clause)))
    (store-in-memory `(,wff * ,ep-name))
    (store-in-context wff)
)) ; END store-gist-clause-characterizing-episode



(defun store-semantic-interpretation-characterizing-episode (wff ep-name subj obj)
;```````````````````````````````````````````````````````````````````````````````````
; Given a wff and episode name, store an articulate2-to.v fact with given
; subject and object (e.g. ^me and ^you) partially characterizing the episode.
; Store the wff in context as well.
;
  (when (null wff) (return-from store-semantic-interpretation-characterizing-episode nil))
  (let ((wff `(,subj articulate2-to.v ,obj ',wff)))
    (store-in-memory `(,wff * ,ep-name))
    (store-in-context wff)
)) ; END store-semantic-interpretation-characterizing-episode



(defun remove-from-memory (pred-patt)
;```````````````````````````````````````
; Removes a fact from memory.
;
  (let ((facts (get-from-memory pred-patt)))
    (if (and facts (not (listp facts)))
      (remove-fact pred-patt (ds-memory *ds*))
      (remove-facts facts (ds-memory *ds*))))
) ; END remove-from-memory



(defun find-all-instances-memory (descr)
;```````````````````````````````````````````
; Given a lambda description, find all instances
; from memory (see 'find-all-instances').
;
  (find-all-instances descr (ds-memory *ds*))
) ; END find-all-instances-memory



(defun store-init-time-of-episode (ep-name &key time-record)
;``````````````````````````````````````````````````````````````
; Stores the initialization time of episode (assumed to be current time,
; unless a time record structure is given as time-record) in both memory
; and the timegraph structure.
;
  (if (null time-record) (setq time-record (get-time)))
  ; Store temporal propositions related to episode in memory
  (store-in-memory `(NOW* during ,ep-name))
  (store-in-memory `(,time-record during ,ep-name))
  ; Store episode in timegraph, with lower bound
  (add-episode-to-timegraph ep-name)
  (update-lower-bound-timegraph ep-name time-record)
) ; END store-init-time-of-episode



(defun store-end-time-of-episode (ep-name &key time-record)
;``````````````````````````````````````````````````````````````
; Stores the end time of episode (assumed to be current time, unless a time
; record structure is given as time-record) in both memory and the timegraph structure.
;
  (if (null time-record) (setq time-record (get-time)))
  ; Update temporal propositions related to episode in memory
  (remove-from-memory `(NOW* during ,ep-name))
  (store-in-memory `(,time-record during ,ep-name))
  ; Add upper bound to episode in timegraph
  (update-upper-bound-timegraph ep-name time-record)
) ; END store-end-time-of-episode



;``````````````````````````````````````````````````````
;
; [*] CONTEXT UTIL
;
;``````````````````````````````````````````````````````



(defun print-context (&key verbose key)
;```````````````````````````````````````
; Prints facts in the context. When verbose is given as true,
; print all keys and associated facts as well. If key is given,
; print only the list of facts stored under that key.
;
  (let (l1 l2)
    (maphash (lambda (k v)
      (if (equal v t) (setq l1 (cons k l1))
        (setq l2 (cons (list k v) l2)))) (ds-context *ds*))
    (mapcar (lambda (f)
      (format t "~a~%" f)) l1)
    (when (or verbose key)
      (format t "------------------------------------ ~%")
      (mapcar (lambda (f)
        (if (or (null key) (equal (first f) key))
          (format t "~a: ~a~%~%" (first f) (second f)))) l2)))
) ; END print-context



(defun store-in-context (wff)
;```````````````````````````````
; Stores a wff in context.
; 
  (let ((wff1 (if (equal (car wff) 'quote) (eval wff) wff)))
    (store-fact wff (ds-context *ds*)))
) ; END store-in-context



(defun get-from-context (pred-patt)
;````````````````````````````````````
; Retrieves a fact from context, by checking whether something matching
; pred-patt is in the context hash table (i.e., the table containing
; facts true at NOW*).
;
  (get-matching-facts pred-patt (ds-context *ds*))
) ; END get-from-context



(defun remove-from-context (pred-patt)
;```````````````````````````````````````
; Removes a fact from context.
;
  (let ((facts (get-from-context pred-patt)))
    (if (and facts (not (listp facts)))
      (remove-fact pred-patt (ds-context *ds*))
      (remove-facts facts (ds-context *ds*))))
) ; END remove-from-context



(defun find-all-instances-context (descr)
;```````````````````````````````````````````
; Given a lambda description, find all instances
; from context (see 'find-all-instances').
;
  (find-all-instances descr (ds-context *ds*))
) ; END find-all-instances-context



(defun store-contextual-fact-characterizing-episode (wff ep-name &key partial)
;`````````````````````````````````````````````````````````````````````````````````
; Stores a contextual fact characterizing episode ep-name. e.g., if
; wff = (^you reply-to.v E1) and ep-name = E2, store <wff> in context
; and (<wff> ** E2) in memory.
;
  (let ((wff1 (if (equal (car wff) 'quote) (eval wff) wff)))
    (if partial
      (store-in-memory `(,wff1 * ,ep-name))
      (store-in-memory `(,wff1 ** ,ep-name)))
    (store-in-context wff1)
    ep-name
)) ; END store-contextual-fact-characterizing-episode



(defun store-new-contextual-facts (wffs)
;`````````````````````````````````````````
; An episode name, say, E1 is generated for the list of fact(s) given as input.
; For each wff, we store the following fact in memory:
;
; 1. (wff ** E1)    (if a single wff)
;    (wff * E1)     (if multiple wffs)
;
; As well as storing each wff in a special context hash table,
; containing all wffs which are true at NOW*.
;
; Furthermore, we store the following facts in memory:
;
; 2. (NOW* during E1)
; 3. (<timestamp> during E1)
;
; where <timestamp> is a record structure of the current system time.
; Note that we don't necessarily know that <timestamp> is the beginning of E1, but
; rather just that it marks a point where we know that wff is true.
;
; Returns the episode name that was created.
;
  (let ((ep-name (episode-name)))
    (store-init-time-of-episode ep-name)
    ; Store each fact in memory and context
    (cond
      ((= 1 (length wffs))
        (store-contextual-fact-characterizing-episode (car wffs) ep-name))
      (t (mapcar (lambda (wff) (store-contextual-fact-characterizing-episode wff ep-name :partial t)) wffs)))
    ep-name
)) ; END store-new-contextual-facts



(defun remove-old-contextual-fact (pred-patt)
;``````````````````````````````````````````````
; Removes a fact (or any number of matching facts) from context when that
; fact is found to be no longer true. To do this, we retrieve the episode
; name characterized by the wff (generally, a list of episode names), say,
; E1. We have to remove the fact (NOW* during E1) from memory, and add
; (<timestamp> during E1), where <timestamp> is a record structure of the
; current system time. The wff is then removed from context (i.e., the NOW*
; hash table).
;
; TODO: should we add (<timestamp> during E1), or (<timestamp> ends E1)?
;
  (let ((facts (get-from-context pred-patt))
        ep-wffs-* ep-wffs-** ep-names-* ep-names-**
        all-wffs-* time-record)
    (if (and facts (not (listp facts)))
      (setq facts (list pred-patt)))
    ; Remove each matching fact from context
    (dolist (fact facts)
      (setq time-record (get-time))
      ; Get all formulas with fact and ** operator, and extract ep-names
      (setq ep-wffs-** (get-from-memory `(,fact ** ?e)))
      (setq ep-wffs-* (get-from-memory `(,fact * ?e)))
      (setq ep-names-** (apply #'append (mapcar #'last ep-wffs-**)))
      (setq ep-names-* (apply #'append (mapcar #'last ep-wffs-*)))
      ; For each ep-name characterized by fact, remove (NOW* during ep-name) fact
      ; from memory and add ending timestamp
      (dolist (ep-name ep-names-**)
        (store-end-time-of-episode ep-name :time-record time-record))
      ; For each ep-name partially characterized by fact, if there are no other facts
      ; partially characterizing ep-name, remove (NOW* during ep-name) from memory
      (dolist (ep-name ep-names-*)
        (setq all-wffs-* (get-from-memory `(?fact * ,ep-name)))
        (if (and (= 1 (length all-wffs-*)) (equal fact (first (car all-wffs-*))))
          (store-end-time-of-episode ep-name :time-record time-record)))
      ; Remove fact from context
      (remove-from-context fact)))
) ; END remove-old-contextual-fact



(defun flush-context ()
;````````````````````````````````
; Flushes context of telic verbs (e.g. saying events) which are only
; "instantaneously" true and are assumed to become false after a predetermined
; amount of time.
; These telic verbs are currently recorded manually in 'resources/verbs-telic.lisp'.
; 
  (mapcar #'remove-old-contextual-fact *verbs-telic*)
) ; END flush-context



;``````````````````````````````````````````````````````
;
; [*] KNOWLEDGE BASE UTIL
;
;``````````````````````````````````````````````````````



(defun print-kb (&key verbose key)
;```````````````````````````````````````
; Prints facts in the knowledge base. When verbose is given as true,
; print all keys and associated facts as well. If key is given,
; print only the list of facts stored under that key.
;
  (let (l1 l2)
    (maphash (lambda (k v)
      (if (equal v t) (setq l1 (cons k l1))
        (setq l2 (cons (list k v) l2)))) (ds-kb *ds*))
    (mapcar (lambda (f)
      (format t "~a~%" f)) l1)
    (when (or verbose key)
      (format t "------------------------------------ ~%")
      (mapcar (lambda (f)
        (if (or (null key) (equal (first f) key))
          (format t "~a: ~a~%~%" (first f) (second f)))) l2)))
) ; END print-kb



(defun store-in-kb (wff)
;```````````````````````````````
; Stores a wff in knowledge base.
; 
  (let ((wff1 (if (equal (car wff) 'quote) (eval wff) wff)))
    (store-fact wff (ds-kb *ds*)))
) ; END store-in-kb



(defun get-from-kb (pred-patt)
;````````````````````````````````````
; Retrieves a fact from knowledge base, by checking whether something matching
; pred-patt is in the kb hash table.
;
  (get-matching-facts pred-patt (ds-kb *ds*))
) ; END get-from-kb



(defun get-all-from-kb ()
;```````````````````````````
; Retrieves all facts from knowledge base.
;
  (let (ret)
    (maphash (lambda (k v)
      (if (equal v t) (setq ret (cons k ret)))) (ds-kb *ds*))
    ret
)) ; END get-all-from-kb



(defun remove-from-kb (pred-patt)
;```````````````````````````````````````
; Removes a fact from knowledge base.
;
  (let ((facts (get-from-kb pred-patt)))
    (if (and facts (not (listp facts)))
      (remove-fact pred-patt (ds-kb *ds*))
      (remove-facts facts (ds-kb *ds*))))
) ; END remove-from-kb



;``````````````````````````````````````````````````````
;
; [*] BUFFER UTIL
;
;``````````````````````````````````````````````````````



(defun enqueue-in-buffer (item buffer &optional (importance 1))
;````````````````````````````````````````````````````````````````
; Adds an item to the priority queue for a given buffer, given an
; importance value (1 by default if not given).
;
  (if (numberp importance) (priority-queue:pqueue-push item importance buffer))
) ; END enqueue-in-buffer



(defun enqueue-in-buffer-ordered (items buffer &key (inc-val 0.0001))
;``````````````````````````````````````````````````````````````````````
; Enqueues a list of items in a given buffer, using a small increment to
; break ties and ensure that items appear in original order when not assigned
; custom importance. Also, assume that any items without explicit importance
; are at least as important as the previous front of the queue.
; 'items' will be a list of either items or (item importance) pairs.
;
  (let ((cur-max (max-importance-in-buffer buffer)) (inc inc-val) importance)
    (dolist (item (reverse items))
      (cond
        ((and (listp item) (= 2 (length item)) (numberp (second item)))
          (setq importance (second item))
          (setq item (first item)))
        (t (setq importance cur-max)))
      (incf importance inc)
      (enqueue-in-buffer item buffer importance)
      (incf inc inc-val))
)) ; END enqueue-in-buffer-ordered



(defun buffer-empty-p (buffer)
;````````````````````````````````
; Checks if a buffer is empty.
;
  (priority-queue:pqueue-empty-p buffer)
) ; END buffer-empty-p



(defun pop-item-from-buffer (buffer)
;``````````````````````````````````````
; Pops the most important item from the given buffer.
;
  (if (not (buffer-empty-p buffer))
    (priority-queue:pqueue-pop buffer))
) ; END pop-item-from-buffer



(defun pop-item-from-buffer (buffer)
;``````````````````````````````````````
; Pops the most important item from the given buffer.
;
  (if (not (buffer-empty-p buffer))
    (priority-queue:pqueue-pop buffer))
) ; END pop-item-from-buffer



(defun pop-item-from-buffer-with-importance (buffer)
;``````````````````````````````````````````````````````
; Pops the most important item from the given buffer as
; an (item, importance) tuple.
;
  (let (item importance)
    (when (not (buffer-empty-p buffer))
      (setq item (priority-queue:pqueue-front-value buffer))
      (setq importance (priority-queue:pqueue-front-key buffer))
      (priority-queue:pqueue-pop buffer)
      (list item importance))
)) ; END pop-item-from-buffer-with-importance



(defun pop-all-from-buffer (buffer)
;``````````````````````````````````````
; Pops all elements from a buffer and returns them as a list.
;
  (loop while (not (buffer-empty-p buffer))
    collect (pop-item-from-buffer buffer))
) ; END pop-all-from-buffer



(defun pop-all-from-buffer-with-importance (buffer)
;``````````````````````````````````````````````````````
; Pops all elements from a buffer and returns them as a list
; of (item, importance) tuples.
;
  (loop while (not (buffer-empty-p buffer))
    collect (pop-item-from-buffer-with-importance buffer))
) ; END pop-all-from-buffer-with-importance



(defun get-item-from-buffer (buffer)
;``````````````````````````````````````
; Gets the most important item from the given buffer without removing.
;
  (if (not (buffer-empty-p buffer))
    (priority-queue:pqueue-front-value buffer))
) ; END get-item-from-buffer



(defun max-importance-in-buffer (buffer)
;`````````````````````````````````````````
; Gets the current maximum importance value in the given buffer.
;
  (if (not (buffer-empty-p buffer))
    (priority-queue:pqueue-front-key buffer)
    0)
) ; END max-importance-in-buffer



(defun clear-buffer (buffer)
;`````````````````````````````
; Clears the given buffer.
;
  (priority-queue:pqueue-clear buffer)
) ; END clear-buffer



(defun iterate-buffer (buffer &optional f)
;````````````````````````````````````````````
; "Iterates" a buffer by popping all elements from the
; buffer, adding them to a list, and re-inserting into the buffer.
; If a function is given, invoke that function on each buffer element.
;
  (let ((dequeued-elems (pop-all-from-buffer-with-importance buffer)))
    (mapcar (lambda (e) (enqueue-in-buffer (first e) buffer (second e))) dequeued-elems)
    (if (and f (functionp f))
      (mapcar (lambda (e) (funcall f (first e))) dequeued-elems)
      (mapcar (lambda (e) (first e)) dequeued-elems))
)) ; END iterate-buffer



(defun deepcopy-buffer (buffer)
;``````````````````````````````````
; Copies a buffer to a new object by re-inserting all items in the buffer.
;
  (priority-queue::copy-pqueue buffer)
) ; END deepcopy-buffer



;``````````````````````````````````````````````````````
;
; [*] NAME/CONCEPT/ALIAS/RECORD STRUCTURE UTIL
;
;``````````````````````````````````````````````````````

; Parameters for pre-loading aliases
(defparameter *concept-aliases* nil)
(defparameter *concept-sets* nil)


(defun get-record-structure (canonical-name)
;``````````````````````````````````````````````
; Gets the record structure aliased to a canonical name,
; if one exists.
;
  (find-if #'record-structure? (get-aliases canonical-name))
) ; END get-record-structure



(defun record-structure! (canonical-name)
;``````````````````````````````````````````
; TTT predicate for substituting record structure for name.
;
  (get-record-structure canonical-name)
) ; END record-structure!



(defun get-slot-record-structure (slot record)
;```````````````````````````````````````````````
; Returns the value of a particular key in a record structure (or nil if it doesn't exist).
;
  (cadr (member slot record))
) ; END get-slot-record-structure



(defun add-alias (alias canonical-name)
;````````````````````````````````````````
; Adds a given alias for a canonical name to the equality sets hash table,
; i.e., indexing on the canonical name.
; For example, (add-alias '(k BW-arch.n) '|BW-concept-3|) creates the equality
; set (|BW-concept-3| (k BW-arch.n)) hashed on |BW-concept-3|, or else appends
; (k BW-arch.n) to the existing set under that index.
;
  (if (member alias (gethash canonical-name (ds-equality-sets *ds*)) :test #'equal)
    (return-from add-alias nil))
  (when (not (gethash canonical-name (ds-equality-sets *ds*)))
    (push canonical-name (gethash canonical-name (ds-equality-sets *ds*))))
  (push alias (gethash canonical-name (ds-equality-sets *ds*)))
) ; END add-alias



(defun get-aliases (canonical-name)
;````````````````````````````````````
; Gets a list of aliases for a particular canonical name.
;
  (gethash canonical-name (ds-equality-sets *ds*))
) ; END get-aliases



(defun remove-alias (alias canonical-name)
;```````````````````````````````````````````
; Removes a given alias for a canonical name.
;
  (when (gethash canonical-name (ds-equality-sets *ds*))
    (setf (gethash canonical-name (ds-equality-sets *ds*)) 
      (remove alias (gethash canonical-name (ds-equality-sets *ds*)) :test #'equal)))
) ; END remove-alias



(defun print-aliases ()
;````````````````````````
; Prints all aliases in equality sets hash table.
;
  (maphash (lambda (canonical-name aliases)
      (format t "~a: ~a~%" canonical-name aliases))
    (ds-equality-sets *ds*))
) ; END print-aliases



(defun get-generic-name (canonical-name)
;``````````````````````````````````````````
; Gets the generic name (i.e. a reified noun, such as (k BW-arch.n)).
;
  (find-if #'kind? (get-aliases canonical-name))
) ; END get-generic-name



(defun generic-name-to-np (generic-name)
;`````````````````````````````````````````
; Converts a generic name to a noun-phrase (also "normalizing" it by removing
; the BW-prefix in the case of any BW-specific names).
;
  (let ((noun (second generic-name)))
    (if (null noun) (return-from generic-name-to-np nil))
    (when (BW-concept? noun)
      (setq noun (second (sym-split noun 3 :front t))))
    (butlast (ulf-to-english (create-indefinite-np noun)))
)) ; END generic-name-to-np



(defun BW-concept? (noun)
;``````````````````````````
; Checks if a symbol is a BW-concept, i.e. anything prefixed with "BW-".
;
  (and (atom noun) (equal 'BW- (car (sym-split noun 3 :front t))))
) ; END BW-concept?



(defun BW-concept-to-common-name! (noun)
;`````````````````````````````````````````
; Maps a blocksworld concept to a common name (e.g. |BW-arch|.n to arch.n).
;
  (if (BW-concept? noun)
    (read-from-string (format nil "~a"
      (second (sym-split noun 3 :front t)))))
) ; END BW-concept-to-common-name!



(defun concept-noun-phrase! (x)
; ````````````````````````````````
; Maps a concept name to an English noun phrase.
;
  (let ((np (generic-name-to-np (get-generic-name x))))
    (when (null np)
      (return-from concept-noun-phrase! '(an unnamed concept)))
    np)
) ; END concept-noun-phrase!



(defun concept-noun! (x)
; ``````````````````````````
; Maps a concept name to an English noun.
;
  (let ((name (get-generic-name x)))
    (when (null name)
      (return-from concept-noun! '(unnamed concept)))
    (cdr (generic-name-to-np name)))
) ; END concept-noun!



(defun create-aliases-of-concept (concept-predicate canonical-name obj-type)
;``````````````````````````````````````````````````````````````````````````````
; Pre-load an alias set to be added to the agent's aliases upon initialization.
;
  (push (list concept-predicate canonical-name obj-type) *concept-aliases*)
) ; END create-aliases-of-concept



(defun store-aliases-of-concept (concept-predicate canonical-name obj-type)
;```````````````````````````````````````````````````````````````````````````
; Stores an object schema (record structure) as an alias of an associated
; canonical name (e.g., |BW-Stack|), as well as a generic name (e.g., (k BW-arch.n)).
; Also store the type of the canonical name (e.g., BW-concept-primitive.n) in context.
;
  (let (generic-name schema-record)
    (setq schema-record (cons '$ (obj-schema-contents (get-stored-schema concept-predicate))))
    (setq generic-name (list 'k concept-predicate))
    (add-alias generic-name canonical-name)
    (add-alias schema-record canonical-name)
    (store-in-context (list canonical-name obj-type))
)) ; END store-aliases-of-concept



(defun create-concept-set (set-type canonical-name concept-set)
;`````````````````````````````````````````````````````````````````
; Pre-load a concept set to be added to the agent's aliases upon initialization.
;
  (push (list set-type canonical-name concept-set) *concept-sets*)
) ; END create-concept-set



(defun store-concept-set (set-type canonical-name concept-set)
;```````````````````````````````````````````````````````````````
; Stores a concept set (i.e., set of object schema names) with an associated
; canonical name. Also stores the generic name, i.e. a set of the generic
; names of the objects in the set.
;
  (let (generic-name)
    (setq generic-name (make-set (mapcar (lambda (concept)
        (find-if (lambda (alias)
          (equal (car alias) 'k)) (get-aliases concept)))
      concept-set)))
    (add-alias generic-name canonical-name)
    (store-in-context (list canonical-name set-type))
    (mapcar (lambda (concept)
        (store-in-context
          (list concept 'member-of.p canonical-name)))
      concept-set))
) ; END store-concept-set



;``````````````````````````````````````````````````````
;
; [*] FACT HASH TABLE STORAGE UTIL
;
;``````````````````````````````````````````````````````



(defun storage-keys (fact)
;``````````````````````````
; Find the list of keys for hash-table storage of 'fact': the fact as a whole, 
; the predicate, and the predicate with one argument at a time, if there
; is more than one (while other arguments are set to nil).
; 
; We also allow facts that are atoms or of form (<atom>); in the 1st case
; the atom is the only key on the output list, and in the second both 
; <atom> and (<atom>) are on the output list; (we always want to be able
; to retrieve via the fact as a whole, and via the predicate)..
;
  (let (keys key n)
    (cond ((atom fact) (list fact))
          ((null (cdr fact)) (list (car fact) fact)); e.g., (pred (pred))
          (t (setq keys (list fact (second fact))); entire fact, & pred
            (when (cddr fact); more than one argument?
              (setq n (length (cddr fact))); no. of non-subject args
              ; push key for subject arg first
              (setq key (list (second fact) (first fact))); in reverse
              (dotimes (i n) (push nil key))
              (setq key (reverse key))
              (push key keys)
              ; push keys for all other args
              (dotimes (i n)
                (setq key (list (second fact) nil)); in reverse
                (dotimes (j n)
                  (if (= i j) (push (nth i (cddr fact)) key)
                              (push nil key)))
                (setq key (reverse key))
                (push key keys)))
            keys))
)) ; END storage-keys



(defun store-fact (fact ht)
;````````````````````````````
; Store the 'fact' in hash table 'ht' (that uses 'equal' as test) using as keys: 
; - the entire fact, 
; - the predicate (unless the fact is atomic or a 1-element list 
;   (i.e., pred with 0 arguments)
; - if the pred has >= 2 arguments, patterns of form  (pred arg1 nil nil ...), 
;   (pred nil arg2 nil nil ...), (pred nil nil arg3 nil nil ...), etc.
; Return T if the fact was new, and NIL otherwise.
;
  (let ()
    (setq fact (eval-functions fact))
    (if (gethash fact ht) (return-from store-fact nil))
    (dolist (key (storage-keys fact))
      (if (equal key fact)
        (setf (gethash key ht) t); just store t in the case where key is the whole fact
        (push fact (gethash key ht))))
    T
)) ; END store-fact



(defun store-facts (facts ht)
;`````````````````````````````
  (dolist (fact facts) (store-fact fact ht))
) ; END store-facts



(defun remove-fact (fact ht)
;````````````````````````````
; Delete 'fact' from hash table 'ht', under all its keys
;
  (setq fact (eval-functions fact))
  (if (gethash fact ht); is 'fact' actually in ht? 
    (prog2 (dolist (key (storage-keys fact))
            (if (or (equal key fact) (= 1 (length (gethash key ht))))
              ; If the key is either the fact itself, or the fact is the
              ; only remaining element of the key, remove the hash itself
              (remhash key ht)
              ; Otherwise, remove the fact from the list of facts at that key
              (setf (gethash key ht) 
                     (remove fact (gethash key ht) :test #'equal))))
            t) ; signal that the fact was removed
     nil ; fact wasn't present in ht
)) ; END remove-fact



(defun remove-facts (facts ht)
;```````````````````````````````
  (dolist (fact facts) (remove-fact fact ht))
) ; END remove-facts



(defun fact-matches-p (pred-patt fact)
;````````````````````````````````````````
; Returns true if 'fact' matches 'pred-patt' (either the predicate constant of 'fact')
; or a proposition template containing variables).
;
  (or
    ; pred-patt is either a 0-arity predicate or the predicate constant
    (and (atom pred-patt)
         (or (equal pred-patt fact)
             (and (listp fact) (>= (length fact) 2) (equal pred-patt (second fact)))))
    ; pred-patt is a proposition of the same arity as fact (possibly with variables)
    (and (listp pred-patt) (listp fact) (= (length pred-patt) (length fact))
         (every (lambda (arg-p arg-f) (or (variable? arg-p) (equal arg-p arg-f))) pred-patt fact)))
) ; END fact-matches-p



(defun get-matching-facts (pred-patt ht)
;``````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred-patt: e.g., (B between.p ?x C); any vars in pred-patt are
;    assumed to be distinct;
; Note that we need to use retrieval key (B between nil nil) or
;    (nil between nil C), and filter the results, because ground
;    wffs are stored under the entire wff as key, under the pred,
;    and under the pred in combination with one argument at a time.
;
; Retrieve the list of facts matching pred-patt from hash table ht.
; This is dependent on the restricted set of keys that are used in 
; storing facts: If there is a variable in pred-patt (as in the
; example), we need to make the variable and all but one non-var
; argument "don't-cares" (nil) in the retrieval. The we filter out
; instances that don't have the required non-variable arguments.
;
; NOTE: here we use the equality sets, so that any match to a constant
; or alias of that constant is successful.
;
  (setq pred-patt (eval-functions pred-patt))
  (if (atom pred-patt) ; special (unexpected) cases of arg-less preds
    (if (symbolp pred-patt) ; facts are stored as list elements
      (return-from get-matching-facts (gethash pred-patt ht))
      ; not a symbol
      (return-from get-matching-facts nil)))
  (if (null (cdr pred-patt)); of form (p)
    (if (symbolp (car pred-patt)) ; use p as key
      (return-from get-matching-facts (gethash (car pred-patt) ht))
      ; of form (p) where p is not a symbol
      (return-from get-matching-facts nil)))
    
  ; at this point we have something of form (x0 p x1 ... xk) where x1 ... xk
  ; are optional and some of the xi may be variables (the expected case).
  (let (arglist pred (nvars 0) nconst key const facts select-facts)
    (setq arglist (cons (car pred-patt) (cddr pred-patt)))
    (setq pred (second pred-patt))
    (dolist (arg arglist)
      (if (variable? arg) (incf nvars)))
    (setq nconst (- (length arglist) nvars))
    ; 4 cases: no vars; no consts; vars & 1 const; vars & > 1 consts
    (cond
      ; no vars
      ((zerop nvars) (gethash (cons (car arglist) (cons pred (cdr arglist))) ht))
      ; no facts
      ((zerop nconst) (reverse (gethash pred ht)))
      ; vars & 1 const
      ((= nconst 1) 
        (setq key (mapcar #'(lambda (x) (if (variable? x) nil x)) pred-patt))
        (reverse (gethash key ht)))
      ; vars & > 1 consts
      (t 
        ; for the key, set all var's and all but one const. to nil
        ; Pick a constant to retain in the key:
        (setq const (find-if #'(lambda (x) (not (variable? x))) 
                      arglist))
        (setq key 
          (let ((arglist-const
                  (mapcar #'(lambda (x) (if (not (eq x const)) nil x)) arglist)))
            (cons (car arglist-const) (cons pred (cdr arglist-const)))))
        (setq facts (gethash key ht)); (in reverse store-order)
        ; filter out facts whose constant args don't match those
        ; of pred-patt:
        (dolist (fact facts)
          (if (not (member nil
                (mapcar #'(lambda (x y) (or (variable? x) (equal x y)))
                  arglist (cons (car fact) (cddr fact)))))
            (push fact select-facts))); this causes correct order
        select-facts))
)) ; END get-matching-facts



(defun find-all-instances (descr curr-state-ht)
;``````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; 'descr' is a lambda abstract, using ':l' for lambda.
;    The body of 'descr' consists of conjoined +ve or -ve literals, one
;    of which must be +ve & contain all variables, including the lambda 
;    variable(s).
;    e.g., (:l (?x ?y) (and (?x between.p ?z ?y) (?x red.a) (?y blue.a)));
;    e.g., (:l (?x) (?x on.p ?y)); everything that's on something
;    NB: A non-equality constraint like "?y is not the table" would
;        have to be expressed as something like (not (?y table.n)),
;        where we've added (is-table table) to the set of facts.
; 'curr-state-ht' is a hash table for positive facts comprising the 
;    current state, indexed under the fact as a whole, the predicate
;    alone, and the predicate in combination with one argument (for
;    each argument, if there's more than one).
; We find all instances of the :l-variable(s)? as follows: 
; (simplest possible method) Start with all the instances of the
; positive predication (in the lambda-description) that contains 
; all the variables, and successively restrict the tuples with the
; remaining conjuncts. Use predication templates, in addition to the
; current set of instances, as arguments in 'constrain-relation', 
; so that argument correspondences will be clear, w/o any indexing.
; In the end, project the resulting list of predicate instances onto
; the :l-variable "dimensions"
;
  (let (body lambda-vars vars main-conjunct facts neglist poslist ind inds)
    (setq lambda-vars (second descr))
    (setq body 
      (if (not (eq (car (third descr)) 'and)); just one predication?
        (list (third descr)); list it, for uniformity
        (cdr (third descr)))); drop the "and"
    (setq body (eval-functions body))
    (setq vars (get-variables
      (mapcar #'(lambda (x) (if (eq (car x) 'not) (second x) x))
        body)))
    ; find positive conjunct containing all variables
    (dolist (conjunct body)
      (when (subsetp vars
              (get-variables (cons (car conjunct) (cddr conjunct))))
            (setq main-conjunct conjunct)
            (return nil))) ; exit loop
    (when (null main-conjunct)
          (format t "~%*** Description ~s~%      given to 'find-all-instances' didn't contain ~%      a predication that covers all variables" descr)
          (return-from find-all-instances nil))

    ; Retrieve the facts in curr-state-ht matching main-conjunct
    (setq facts (get-matching-facts main-conjunct curr-state-ht))
    ; Now apply the remaining conjuncts as constraints on facts;
    ; first place negative facts after positive ones (also omitting 
    ; main-conjunct):
    (dolist (conjunct body)
        (if (eq (car conjunct) 'not) 
            (push conjunct neglist)
            (if (not (equal conjunct main-conjunct))
                (push conjunct poslist))))
    (setq body (append (reverse poslist) (reverse neglist)))
    (dolist (conjunct body)
        (setq facts 
          (constrain-relation main-conjunct conjunct facts curr-state-ht)))

    ; Now project 'facts" onto the dimensions picked out by the :l-variables
    ; 'Project-relation' uses indices for argument positions to be picked
    ; out by the projection, so we compute these first by assigning
    ; index properties to the variables (the properties will be global, 
    ; but harmless):
    (setq ind 1)
    (when (cdr main-conjunct)
      (setf (get (car main-conjunct) 'index) ind)
      (incf ind))
    (dolist (arg (cddr main-conjunct))
        (incf ind) (if (variable? arg) (setf (get arg 'index) ind)))
    (setq inds (mapcar #'(lambda (x) (get x 'index)) lambda-vars))
    ; Return the projecton of facts onto the positions corr. to the
    ; Lambda variables:
    (project-relation facts inds)
)) ; END find-all-instances



(defun constrain-relation (pred1-patt {~}pred2-patt rel1 kb-ht)
;````````````````````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred1-patt: of form (arg1 pred1 ... argn); in general, argi may
;     be a variable.
; {~}pred2-patt: of form (arg'1 pred2 ... arg'm) or 
;     (not (arg'1 pred2 ... arg'm)), where m =< n and all variables 
;     of pred2-patt occur in pred1-patt; {~}pred2-patt also allows
;     for equalities or negated equalities, using one of {=,eq,equal};
; rel1: a list of relation instances, a subset of pred1-patt instances;
;
; If {-}pred2-patt is unnegated, the result is a restricted subset
; of the rel1-instances, where only those are retained and returned 
; whose arguments, when used to instantiate {~}pred2-patt, lead to 
; existing hash-table entries.
;
; If {-}pred2-patt is negated, the procedure is similar, except that
; only rel1-instances are retained where the pred2 hash-table lookup
; does *not* lead to existing entries.
;
  (let (positive key rel2)
    ; deal with equality constraints first
    (if (and (listp {~}pred2-patt) (member (second {~}pred2-patt) '(= eq equal)))
        (return-from constrain-relation
            (constrain-relation-by-equality pred1-patt {~}pred2-patt rel1)))
    ; now inequality constraints
    (if (and (listp {~}pred2-patt) (eq (car {~}pred2-patt) 'not)
              (listp (second {~}pred2-patt)) 
              (member (second (second {~}pred2-patt)) '(= eq equal)))
        (return-from constrain-relation 
            (constrain-relation-by-inequality pred1-patt {~}pred2-patt rel1)))
    ; other constraints (=> require consulting '{~}pred2-patt'-facts in kb-ht)
    (setq positive (not (and (listp {~}pred2-patt) 
                              (eq (car {~}pred2-patt) 'not))))
    (dolist (item rel1)
      ; set variables occurring in pred1-patt to corresponding
      ; elements of the item predication:
      (mapcar #'(lambda (x y) (if (variable? x) (set x y)))
              (cons (car pred1-patt) (cddr pred1-patt))
              (cons (car item) (cddr item)))
      ; construct a retrieval key for pred2 accordingly:
      (setq key (mapcar #'(lambda (x) (if (variable? x) (eval x) x))
                    (if positive {~}pred2-patt (second {~}pred2-patt))))
      (if (gethash key kb-ht)
          (if positive (push item rel2))
          (if (not positive) (push item rel2))))
    (reverse rel2)
    ; It will come out preserving the ordering in rel1.
)) ; END constrain-relation



(defun constrain-relation-by-equality (pred-patt equality rel)
;```````````````````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred-patt: provides variable argument positions for the predications in rel;
; equality: e.g., (?y = B3), i.e., we fix the value of a variable in pred-patt
;           and hence of corresponding arguments in the rel predications;
;           the reverse form, e.g., (B3 = ?y), is also handled;
; rel: a set of ground predications (of form  pred-patt), to be filtered with
;           the equality; NB: no error check is done to ensure that pred-patt
;           and rel use the same predicate. Also no check is made on the form
;           of equality. The calling program might use 'eq' or 'equal', instead
;           of '=', but the initial symbol in the given equality is ignored.
;    
  (let (var val (i -1) result)
    ; find the position of the variable in the equality in pred-patt, and
    ; then go through the elements of rel, retaining for output just those
    ; that have the constant specified by the equality at the position
    ; determined from the pred-patt:
    (setq var (first equality); e.g., (?y = B3) ... the expected form
          val (third equality))
    (if (not (variable? var))
        (setq var (third equality); e.g., (B3 = ?y)
              val (first equality)))
    (cond ((atom pred-patt) rel); unexpected: pred-patt should be (pred ...)
          (t (dolist (x pred-patt)
              (incf i) (if (equal x var) (return nil))); exit loop (i is set)
              (dolist (r rel)
                (if (equal (nth i r) val) (push r result)))
              result))
)) ; END constrain-relation-by-equality



(defun constrain-relation-by-inequality (pred-patt inequality rel)
;```````````````````````````````````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; pred-patt: provides variable argument positions for the predications in rel;
; inequality: e.g., (not (?y = B3)), i.e., we preclude the value of a variable 
;           in pred-patt and hence of corresponding arguments in the rel pred-
;           ications; the reverse form, e.g., (not (B3 = ?y)), is also handled;
; rel: a set of ground predications (of form  pred-patt), to be filtered with
;           the inequality; NB: no error check is done to ensure that pred-patt
;           and rel use the same predicate. Also no check is made on the form
;           of inequality.
;    
  (let (var val (i -1) result)
    ; find the position of the variable in the equality in pred-patt, and
    ; then go through the elements of rel, retaining for output just those
    ; that have the constant specified by the equality at the position
    ; determined from the pred-patt:
    (setq var (first (second inequality)); e.g., (not (?y = B3)) ... expected 
          val (third (second inequality)))
    (if (not (variable? var))
        (setq var (third (second inequality)); e.g., (not (B3 = ?y))
              val (first (second inequality))))
    (cond ((atom pred-patt) rel); unexpected: pred-patt should be (pred ...)
          (t (dolist (x pred-patt)
              (incf i) (if (equal x var) (return nil))); exit loop (i is set)
              (dolist (r rel)
                (if (not (equal (nth i r) val)) (push r result)))
              result))
)) ; END constrain-relation-by-inequality



(defun project-relation (rel indices)
;``````````````````````````````````````
; TAKEN FROM 'planning-via-schemas.lisp', MODIFIED FOR INFIX SYNTAX.
; 'rel' is assumed to be a set of k-tuples, with k  >= 1. (If it is 1,
; the list 'rel' is returned unchanged.) 'Indices' is the list of argument
; indices indicating what "dimensions" of rel should be retained (projecting
; other dimensions onto them), and in what order they should be returned.
; The result is a set k-tuples, where k =|indices|, except that if k=1,
; the 1-tuples are reduced to individual atoms, not singleton lists.
;
; Method: Run through the 'rel' tuples, and for each tuple, pull out the
; elements corresponding to the 'indices', and if this "subtuple" has not yet
; been encountered before (as registered in an ad-hoc hash table), push it
; onto the result list and register it in the ad-hoc hash table.
; Return the reverse of the final result list.
; 
  (let (ht key result)
    ; Is 'rel' unary, i.e., a list of atoms or single-element lists?
    (if (or (null rel) (atom (car rel)) (null (cdar rel)))
        (return-from project-relation rel))
    (setq ht (make-hash-table :test #'equal))
    (dolist (tuple rel)
        (setq key nil)
        (dolist (i indices)
            (push (nth (- i 1) tuple) key))
        (setq key (reverse key))
        (when (null (gethash key ht))
            (setf (gethash key ht) T) 
            (push key result)))
    (if (null (cdr indices)); if just one index, "flatten" the result
        (setq result (apply #'append result)))
    (reverse result)
)) ; END project-relation



(defun unwrap-utterance (say-to-fact)
;``````````````````````````````````````
; Given a fact of the form (?x say-to.v ?y '(<expr>)),
; return the unquoted surface expression.
;
  (eval (fourth say-to-fact))
) ; END unwrap-utterance

(defun unwrap-utterances (say-to-facts)
  (mapcar #'unwrap-utterance say-to-facts)
) ; END unwrap-utterances



(defun unwrap-gist-clause (gist-fact)
;``````````````````````````````````````
; Given a fact of the form (?x paraphrase-to.v ?y '(<gist-clause>)),
; return the unquoted gist-clause.
;
  (eval (fourth gist-fact))
) ; END unwrap-gist-clause

(defun unwrap-gist-clauses (gist-facts)
  (reverse (mapcar #'unwrap-gist-clause gist-facts))
) ; END unwrap-gist-clauses



(defun unwrap-semantic-interpretation (semantic-fact)
;``````````````````````````````````````````````````````
; Given a fact of the form (?x articulate2-to.v ?y '(<wff>)),
; return the wff.
;
  (second (fourth semantic-fact))
) ; END unwrap-semantic-interpretation

(defun unwrap-semantic-interpretations (semantic-facts)
  (mapcar #'unwrap-semantic-interpretation semantic-facts)
) ; END unwrap-semantic-interpretations



;``````````````````````````````````````````````````````
;
; [*] EPISODE/VARIABLE UTIL
;
;``````````````````````````````````````````````````````



(defun eval-func! (f &rest args)
;````````````````````````````````
; Evaluate some ELF function <some-func>.f and its args.
;
  (apply f args)
) ; END eval-func!



(defun eval-functions (wff)
;```````````````````````````
; Evaluates all ELF functions (<some-func>.f arg1 arg2 ...) in a wff.
;
  (ttt:apply-rule '(/ (function? _*) (eval-func! function? _*)) wff)
) ; END eval-functions



(defun skolem (name)
;````````````````````
; Creates a unique skolem constant given a name.
;
  (intern (format nil "~a.SK" (gentemp (string-upcase (string name)))))
) ; END skolem



(defun skolem? (sk-name)
;``````````````````````````
; Checks if sk-name is a skolem constant.
;
  (and (atom sk-name) (equal (second (sym-split sk-name 3)) '.SK))
) ; END skolem?



(defun duplicate-var (var)
;````````````````````````````
; Duplicates a variable name, returning a new symbol that hasn't
; been used elsewhere (for readability's sake, base the new symbol
; on the text of the original, splitting off any trailing numbers)
;
  (gentemp (coerce (reverse
    (member-if-not #'digit-char-p
      (reverse (coerce (string var) 'list)))) 'string))
) ; END duplicate-var



(defun get-all-variables (x)
;``````````````````````````````
; Gets all variables occurring in an expression.
;
  (let (vars)
    (labels ((get-all-variables-recur (y)
      (cond
        ((and (atom y) (variable? y))
          (push y vars))
        ((listp y) (mapcar #'get-all-variables-recur y)))))
    (get-all-variables-recur x)
    (remove-duplicates vars)
))) ; END get-all-variables



(defun duplicate-vars (x)
;``````````````````````````
; Given a symbol or list, replace all variables with duplicate variables.
; Returns both the new symbol/list, and a list of all substitutions made.
;
  (let (vars new-var pairs (ret x))
    (setq vars (reverse (get-all-variables x)))
    (dolist (var vars)
      (setq new-var (duplicate-var var))
      (setq ret (subst new-var var ret))
      (push (list var new-var) pairs))
    (list ret pairs)
)) ; END duplicate-vars



(defun episode-var ()
;````````````````````````````
; Creates an episode variable starting with '?E'.
;
  (intern (format nil "?~a" (string (gensym "E"))))
) ; END episode-var



(defun episode-name ()
;`````````````````````````````
; Return a unique episode name, e.g., E38
;
  (gentemp "E")
) ; END episode-name



(defun dual-var (ep-var)
;`````````````````````````
; Given an episode variable like ?e1, return the non-fluent dual
; of that variable, e.g., !e1 (and vice-versa if !e1 is given).
;
  (when (variable? ep-var)
    (if (equal #\? (car (explode ep-var)))
      (implode (cons #\! (cdr (explode ep-var))))
      (implode (cons #\? (cdr (explode ep-var))))))
) ; END dual-var



(defun episode-and-proposition-name (dual-var)
;````````````````````````````````````````````````
; NOTE: function deprecated as of 6/9/2020, after it was decided that we
; would abandon the proposition variable format (i.e., '?a1.') for the
; episode schemas. -B.K.
; 'dual-var' is normally a variable symbol starting with '?' and ending
; in '.'. As such, it actually stands for 2 variables: a reified-
; proposition variable (when the period is included) and implicitly,
; for an episode variable (when the period is dropped). E.g., '?a1.'
; is a (reified) proposition variable, but implicitly it also specifies
; an episode variable '?a1' (no period). Correspondingly, we create 
; two new names (constants): one for an episode (e.g., 'EP38'), and 
; one for the corresponding reified proposition (e.g., 'EP38.'); we 
; then return the two variables with the two new names, e.g.,
;   ((?a1 . EP38) (?a1. . EP38.))
; If there is no final period, we just return an episode name for the
; variable, e.g. if dual-var is just '?a1', we return
;   ((?a1 . EP38))
;
  (let ((chars (explode dual-var)) ep-var ep-name prop-name result)
    (when (not (char-equal #\? (car chars)))
      (format t "~%***Attempt to form episode and proposition name from~%   non-question-mark variable ~a" dual-var)
      (return-from episode-and-proposition-name nil))
    (setq ep-var dual-var) ; will be used if there's no final period
    (setq ep-name (episode-name))
    (when (char-equal #\. (car (last chars))) ; form proposition name
      (setq ep-var (implode (butlast chars))) ; implicit ep-var
      (setq prop-name (implode (append (explode ep-name) '(#\.))))
      (setq result (list (cons dual-var prop-name))))
    (push (cons ep-var ep-name) result)
)) ; END episode-and-proposition-name



(defun create-say-to-wff (content &key reverse)
;```````````````````````````````````````````````
; Creates and returns a wff consisting of a (^me say-to.v ^you '(...))
; action, or a (^you say-to.v ^me '(...)) action if :reverse t is given.
;
  (if (not reverse)
    `(^me say-to.v ^you (quote ,content))
    `(^you say-to.v ^me (quote ,content)))
) ; END create-say-to-wff



(defun create-paraphrase-to-wff (content &key reverse)
;``````````````````````````````````````````````````````
; Creates and returns a wff consisting of a (^me paraphrase-to.v ^you '(...))
; action, or a (^you paraphrase-to.v ^me '(...)) action if :reverse t is given.
;
  (if (not reverse)
    `(^me paraphrase-to.v ^you (quote ,content))
    `(^you paraphrase-to.v ^me (quote ,content)))
) ; END create-paraphrase-to-wff



;``````````````````````````````````````````````````````
;
; [*] PATTERN UTIL
;
;``````````````````````````````````````````````````````



(defun readrules (rootname packet)
;``````````````````````````````````
; This reads in the set of decomposition and output rules. 
; It embeds these rules in a tree whose root node is 'rootname',
; and where first children are reached via the 'children' property,
; and children are connected via the 'next' property. Rule node
; names are generated by GENSYM. Decomposition and output patterns
; are stored under the 'pattern' property, and output rules are
; distinguished by having a non-NIL 'directive' property.
;
; READRULES also puts a numerical property called 'latency' 
; on the property list of output rules. This is the number
; of outputs that must be generated before the rule can be
; used again. As indicated below, in the data set the numeric
; value of latency and the directive symbol are supplied jointly
; as a 2-element list, but these become separate properties
; in the choice tree that is built.
;
; 'Packet' is of form (depth pattern optional-pair
;	                depth pattern optional-pair ...)
; where "depth" is a number =1 for top-level rules, =2 for
; direct descendants of top-level rules, etc.; "pattern" is
; a decomposition pattern or other output; and optional-pair
; is present iff "pattern" is a reassembly pattern or other 
; output. The first element of optional-pair, if present, is 
; the latency of the rule. The second element (the directive
; symbol) is :out (for reassembly), :subtree (for continuing
; recursion in a subtree), :subtrees (for a list of subtrees
; to be returned as output), :subtree+clause (for a recursive
; continuation where a choice tree root plus a clause constructed
; by a reassembly rule are used), :schema (when the output is
; the name of a schema), :schema+args (when the schema name is 
; accompanied by an argument list whose elements depend on
; reassembly), and :gist (when the output is a decontextualized
; user sentence -- a "gist clause").
;
  (let (stack rest n name)
    ; Overwrite previous rule tree if this is called more than once for a given rootname
    (setf (symbol-plist rootname) nil)
    
    (setf (get rootname 'pattern) (cadr packet))
    ; The root is at depth 1, its 'next' or 'children' properties
    ; will be set if/when a rule at the same or lower depth is encountered
    (setq stack `((1 . ,rootname)))
    ; Advance past the 1st depth-# and pattern
    (setq rest (cddr packet))

    ; Loop until full rule tree is built
    (loop while rest do
      ; Save next depth number (or optional pair)
      (setq n (car rest))
      ; Advance past depth number to next pattern, or past the
      ; optional pair to the next depth number
      (setq rest (cdr rest))

      (cond
        ; If n is a number, it is the depth of a new rule
        ((numberp n)
          (setq name (gensym "RULE"))
          ;; (format t "~%New rule ~a " name) ; DEBUGGING
          (setf (get name 'pattern) (car rest))
          ;; (format t "~%'pattern' property of ~a is ~%  ~a" 
          ;;               name (get name 'pattern)) ; DEBUGGING
          ; Advance past the current pattern
          (setq rest (cdr rest))

          (cond
            ; New rule at same depth?
            ((equal n (caar stack))
              ; Let 'next' of previous rule point to new rule
              (setf (get (cdar stack) 'next) name)
              ;; (format t "~%@@ 'next' pointer of ~s has been set to ~s" (get (cdar stack) 'next) name) ; DEBUGGING
              (rplaca stack (cons n name)))

            ; New rule at greater depth?
            ((> n (caar stack))
              ; Let 'children' of previous rule point to new rule
              (setf (get (cdar stack) 'children) name)
              ;; (format t "~%@@ 'children' pointer of ~s has been set to ~s" (get (cdar stack) 'children) name) ; DEBUGGING
              ; Push new (level . rule name) pair onto stack
              (setq stack (cons (cons n name) stack)))

            ; New rule at lower depth?
            (t
              ; Depth differential
              (setq n (- (caar stack) n))
              ; Pop a number of stack elements equal to depth differential
              (dotimes (dummyvar n) (setq stack (cdr stack)))
              ; Resulting top element must be same depth, so set 'next' pointer to new rule
              (setf (get (cdar stack) 'next) name)
              ;; (format t "~%@@ 'next' pointer of ~s has been set to ~s" (get (cdar stack) 'next) name) ; DEBUGGING
              (rplaca stack (cons (caar stack) name)))))

        ; If n is a (latency directive) pair rather than depth number
        (t
          ; Set the 'latency' of the rule at the top of the stack to car of pair
          (setf (get (cdar stack) 'latency) (car n))
          ;; (format t "~%'latency' property of ~a is ~%  ~a"
          ;;           (cdar stack) (get (cdar stack) 'latency)) ; DEBUGGING
          ; Set the 'time-last-used' value to be more negative than any latency, so that
          ; no rule will be blocked initially
          (setf (get (cdar stack) 'time-last-used) -10000)
          ; Set the 'directive' property to the second element of the pair
          (setf (get (cdar stack) 'directive) (second n))
          ;; (format t "~%'directive' property of ~a is ~%  ~a"
          ;;           (cdar stack) (get (cdar stack) 'directive)) ; DEBUGGING
        )))
  "RULE TREE HAS BEEN BUILT")
) ; END readrules



(defun reset-rule (rule)
;`````````````````````````
; Reset 'time-last-used' to -100 in all assembly rules dominated by rule
;
  (cond
    ((null rule) nil)
    (t (reset-rule (get rule 'children))
      (reset-rule (get rule 'next))
      (if (get rule 'time-last-used)
        (setf (get rule 'time-last-used) -100))))
) ; END reset-rule



(defun print-matched-rules (tagged-clause rule-node matched-nodes result)
;`````````````````````````````````````````````````````````````````````````````
; Pretty-prints the sequence of pattern nodes that were matched in the process of
; applying a particular rule node to a tagged clause.
;
  (let* ((i 1) (rule-node-str (concatenate 'string " " (string rule-node) " "))
        (len-rule-node-str (length rule-node-str))
        (len-padding (floor (/ (- 100 len-rule-node-str) 2))))
  ; Format header for debug message
  (format t "~%~%~%~v@{~a~:*~}" len-padding "*")
  (format t "~a" rule-node-str)
  (format t "~v@{~a~:*~}" len-padding "*")
  ; Print input, matched nodes, result, and footer
  (format t "~% ---- INPUT: ~a~%" tagged-clause)
  (dolist (node matched-nodes)
    (format t "~%~a.  ~s" i node)
    (incf i))
  (format t "~%~% ---- RESULT: ~a" result)
  (format t "~%~v@{~a~:*~}~%~%~%" (+ (* 2 len-padding) len-rule-node-str) "*")
)) ; END print-matched-rules



;``````````````````````````````````````````````````````
;
; [*] GPT-3 GENERATION UTIL
;
;``````````````````````````````````````````````````````



(defun shortname (name &key firstname)
;``````````````````````````````````````
; Gets the short version of a name string (if name includes a title,
; shortname is title + last name, otherwise shortname is first name).
; If :firstname t is given, return only first name.
;
  (let ((parts (str-split name #\ )))
    (cond
      ; Includes title, first name, and last name
      ((and (>= (length parts) 3)
            (member #\. (explode (first parts))))
        (if firstname
          (second parts)
          (concatenate 'string (first parts) " " (str-join (cddr parts) #\ ))))
      ; Includes title and last name
      ((and (= (length parts) 2)
            (member #\. (explode (first parts))))
        (if firstname
          ""
          (concatenate 'string (first parts) " " (second parts))))
      (t (first parts)))
)) ; END shortname



(defun generate-prompt-turn-start (name &key (short t) (newline t))
;````````````````````````````````````````````````````````````````````
; Generates a turn start prefix for the prompt.
;
  (if newline
    (format nil "[N]~a:" (if short (shortname name) name))
    (format nil "~a:" (if short (shortname name) name)))
) ; END generate-prompt-turn-start



(defun standardize-case+punctuation (str)
;```````````````````````````````````````````````
; Given a string, standardize the case and punctuation.
; 
  ; capitalize beginning of each sentence and fix periods
  (setq str (str-join (mapcar (lambda (substr)
        (format nil "~@(~a~)" (string-trim " " substr)))
      (str-split str #\.)) ". "))
  ; fix question marks and exclamation points
  (setq str (str-join (mapcar (lambda (substr)
      (string-trim " " substr))
    (str-split str #\?)) "? "))
  (setq str (str-join (mapcar (lambda (substr)
      (string-trim " " substr))
    (str-split str #\!)) "! "))
  ; fix commas
  (setq str (str-join (mapcar (lambda (substr)
      (string-trim " " substr))
    (str-split str #\,)) ", "))
  ; capitalize "I"
  (setq str (str-replace str " i " " I "))
  ; fix final punctuation
  (if (equal (last (explode str) 3) '(#\  #\. #\ ))
    (setq str (coerce (append (butlast (explode str) 3) '(#\.)) 'string)))
  (if (equal (last (explode str) 2) '(#\. #\ ))
    (setq str (coerce (append (butlast (explode str) 2) '(#\.)) 'string)))
  (if (equal (last (explode str)) '(#\ ))
    (setq str (coerce (append (butlast (explode str)) '(#\.)) 'string)))
  (if (not (member (car (last (explode str))) '(#\. #\? #\!)))
    (setq str (coerce (append (explode str) '(#\.)) 'string)))
  str
) ; END standardize-case+punctuation



(defun expr-to-str (expr)
;```````````````````````````
; Converts an expression (which is either a
; word list or ULF) to a string.
; 
  (cond
    ((atom expr) (string expr))
    ((sentence? expr) (words-to-str expr))
    (t (ulf-to-str expr)))
) ; END expr-to-str



(defun words-to-str (wordlist)
;````````````````````````````````
; Converts a list of word symbols to a string.
;
  (let (ret)
    (setq ret (format nil "~{~a ~}" wordlist))
    (standardize-case+punctuation ret)
)) ; END words-to-str



(defun ulf-to-str (ulf)
;```````````````````````````
; Converts a ULF to a string, replacing indexical
; pronouns and possessives.
;
  (if (member "ulf2english" *dependencies* :test #'equal)
    (ulf2english:ulf2english (preprocess-ulf-pronouns-for-prompt1 ulf)))
) ; END ulf-to-str



(defun get-pron-case (name case)
;```````````````````````````````````
; Gets the appropriate third person pronoun case based on
; the lists of female and male first names in resources.
;
; TODO: in the future we will likely want to have the
; user enter their pronouns, to avoid potential misgendering.
;
  (let ((firstname (intern (shortname name :firstname t))))
    (cond
      ; Subjective -> he/she/they
      ((member case '(subjective subj)) (cond
        ((equal (get firstname 'entity-type) 'male.n)
          'he.pro)
        ((equal (get firstname 'entity-type) 'female.n)
          'she.pro)
        (t 'they.pro)))
      ; Objective -> him/her/them
      ((member case '(objective obj)) (cond
        ((equal (get firstname 'entity-type) 'male.n)
          'him.pro)
        ((equal (get firstname 'entity-type) 'female.n)
          'her.pro)
        (t 'them.pro)))
      ; Possessive -> his/her/their
      ((member case '(possessive poss)) (cond
        ((equal (get firstname 'entity-type) 'male.n)
          'his.d)
        ((equal (get firstname 'entity-type) 'female.n)
          'her.d)
        (t 'their.d))))
)) ; END get-pron-case



(defun preprocess-ulf-pronouns-for-prompt1 (ulf)
;``````````````````````````````````````````````````
; Wrapper function for preprocess-ulf-pronouns-for-prompt
;
  (first (preprocess-ulf-pronouns-for-prompt ulf))
) ; END preprocess-ulf-pronouns-for-prompt1



(defun preprocess-ulf-pronouns-for-prompt (ulf &key me-pron you-pron)
;``````````````````````````````````````````````````````````````````````
; Preprocess a ULF for prompt generation by replacing indexical variables
; ^me and ^you with either the shortname of the corresponding agent, or
; the appropriate anaphoric pronoun if the name has previously been used.
;
; TODO: this function is a bit messy; it should be optimized in the future.
;
  (cond
    ; If ^me is encountered as an object (non-car of a list),
    ; replace with them/her/him (if anaphoric) or the value of ^me.
    ((equal ulf '^me)
      (list (if me-pron
        (get-pron-case *^me* 'obj)
        (intern (shortname *^me*)))
      t you-pron))
    ; If ^you is encountered as an object (non-car of a list),
    ; replace with them/her/him (if anaphoric) or the value of ^you.
    ((equal ulf '^you)
      (list (if you-pron
        (get-pron-case *^you* 'obj)
        (intern (shortname *^you*)))
      me-pron t))
    ; Non-indexical atom.
    ((atom ulf)
      (list ulf me-pron you-pron))
    ; If ^me is encountered as a subject (car of a list),
    ; replace with they/she/he (if anaphoric) or the value of ^me.
    ((equal (car ulf) '^me)
      (list (cons (if me-pron
              (get-pron-case *^me* 'subj)
              (intern (shortname *^me*)))
        (first (preprocess-ulf-pronouns-for-prompt (cdr ulf) :me-pron t :you-pron you-pron)))
      t you-pron))
    ; If ^you is encountered as a subject (car of a list),
    ; replace with they/she/he (if anaphoric) or the value of ^you.
    ((equal (car ulf) '^you)
      (list (cons (if you-pron
              (get-pron-case *^you* 'subj)
              (intern (shortname *^you*)))
        (first (preprocess-ulf-pronouns-for-prompt (cdr ulf) :me-pron me-pron :you-pron t)))
      me-pron t))
    ; If possessive subject with ^me and anaphoric, replace with their/her/his.
    ((and (equal (car ulf) '(^me 's)) me-pron)
      (list (cons (get-pron-case *^me* 'poss)
        (first (preprocess-ulf-pronouns-for-prompt (cdr ulf) :me-pron t :you-pron you-pron)))
      t you-pron))
    ; If possessive subject with ^you and anaphoric, replace with their/her/his.
    ((and (equal (car ulf) '(^you 's)) you-pron)
      (list (cons (get-pron-case *^you* 'poss)
        (first (preprocess-ulf-pronouns-for-prompt (cdr ulf) :me-pron me-pron :you-pron t)))
      me-pron t))
    ; Otherwise, recur on each element in list
    (t (list (mapcar (lambda (part)
        (let ((result (preprocess-ulf-pronouns-for-prompt part :me-pron me-pron :you-pron you-pron)))
          (setq me-pron (second result))
          (setq you-pron (third result))
          (first result)))
        ulf)
      me-pron you-pron)))
) ; END preprocess-ulf-pronouns-for-prompt



(defun generate-prompt-preprocess-history (history)
;```````````````````````````````````````````````````
; Preprocesses dialogue history into a single string.
; History is a list of lists (agent turn), where agent
; and turn are both strings.
;
  (let (turn-strs)
    (setq turn-strs (mapcar (lambda (turn)
      (concatenate 'string (shortname (first turn)) ": " (second turn))) history))
    (str-join turn-strs "[N]")
)) ; END generate-prompt-preprocess-history



(defun generate-prompt-preprocess-paraphrase-examples (examples)
;``````````````````````````````````````````````````````````````````
; Preprocesses a list of examples into a single string.
; Examples is a list of 3-tuples of strings.
; 
  (let (example-strs)
    (setq example-strs (mapcar (lambda (example)
      (concatenate 'string
        (format nil "\"~a " (generate-prompt-turn-start "Person A" :short nil :newline nil))
        (first example)
        (format nil "~a " (generate-prompt-turn-start "Person B" :short nil))
        (second example)
        "\"[N]"
        (format nil "~a " (generate-prompt-turn-start (string *^you*)))
        (first example)
        (format nil "~a " (generate-prompt-turn-start (string *^me*)))
        (third example)))
      examples))
    (str-join example-strs "[N][N]")
)) ; END generate-prompt-preprocess-paraphrase-examples



(defun generate-prompt-preprocess-gist-examples (examples)
;````````````````````````````````````````````````````````````
; Preprocesses a list of examples into a single string.
; Examples is a list of 3-tuples of strings.
; 
  (let (example-strs)
    (setq example-strs (mapcar (lambda (example)
      (concatenate 'string
        "Context: \"" (first example) "\"[N]"
        "Utterance: \"" (second example) "\"[N]"
        (if (equal (third example) ".")
          "Rewritten: None"
          (format nil "Rewritten: \"~a\"" (third example)))))
      examples))
    (str-join example-strs "[N][N]")
)) ; END generate-prompt-preprocess-gist-examples



(defun generate-prompt-emotion (utterance history emotions)
;``````````````````````````````````````````````````````````````````````````````````
; Generates a GPT-3 prompt for classifying an utterance string as one of the emotions in
; emotions (a list of strings), given a dialogue history (a list of strings).
;
  (let (prompt)
    (setq prompt
      (format nil "From the following list, which emotional state most closely describes ~a's feelings?[N]"
        (shortname (string *^me*))))
    (setq prompt (concatenate 'string prompt
      (str-join emotions ", ")
      "[N][N]"
      (generate-prompt-preprocess-history history)
      (generate-prompt-turn-start (string *^me*))
      " "
      utterance
      "[N][N]Emotional state:"))
)) ; END generate-prompt-emotion



(defun generate-prompt-paraphrase (facts examples prev-utterance gist-clause)
;``````````````````````````````````````````````````````````````````````````````````
; Generates a GPT-3 prompt for paraphrasing from facts, which is a list of strings,
; a list of examples (3-tuples of strings), a previous utterance string, and a
; gist-clause string.
;
  (let (prompt)
    (setq prompt (format nil "~:(~a~) is having a conversation with ~:(~a~). " *^you* *^me*))
    (setq prompt (concatenate 'string prompt (str-join facts " ")))
    (setq prompt (concatenate 'string prompt
      (format nil "[N][N]Rewrite the following conversations as conversations between ~a and ~a:[N][N]"
        (shortname (string *^you*)) (shortname (string *^me*)))))
    (setq prompt (concatenate 'string prompt
      ; Add examples to prompt
      (generate-prompt-preprocess-paraphrase-examples examples)
      "[N][N]"
      ; Add prev-utterance and gist-clause to prompt
      (format nil "\"~a " (generate-prompt-turn-start "Person A" :short nil :newline nil))
      prev-utterance
      (format nil "~a " (generate-prompt-turn-start "Person B" :short nil))
      gist-clause
      "\"[N]"
      (format nil "~a " (generate-prompt-turn-start (string *^you*)))
      prev-utterance
      (generate-prompt-turn-start (string *^me*))))
    prompt
)) ; END generate-prompt-paraphrase



(defun generate-prompt-unconstrained (facts history)
;````````````````````````````````````````````````````````
; Generates a GPT-3 prompt for unconstrained generation from facts,
; which is a list of strings, and history, which is a list of
; lists (agent turn) where agent and turn are both strings.
;
  (let (prompt)
    (setq prompt (format nil "Write a conversation between ~:(~a~) and ~:(~a~). " *^you* *^me*))
    (setq prompt (concatenate 'string prompt (str-join facts " ")))
    (setq prompt (concatenate 'string prompt "[N]"
      ; Add initial greeting from user to prompt to calibrate GPT-3
      (format nil "~a Hi, ~a." (generate-prompt-turn-start (string *^you*)) (shortname (string *^me*)))
      ; If the initial dialogue turn is not Eta's, add initial greeting from Eta to calibrate GPT-3
      (if (not (equal (first (car history)) (string *^me*)))
        (format nil "~a Hi, ~a." (generate-prompt-turn-start (string *^me*)) (shortname (string *^you*)))
        "")
      (if history "[N]" "")
      ; TODO: only add the second line by Sophie if not detected in conversation log
      (generate-prompt-preprocess-history history)
      (generate-prompt-turn-start (string *^me*))))
    prompt
)) ; END generate-prompt-unconstrained



(defun generate-prompt-gist (examples utterance prior-gist-clause)
;``````````````````````````````````````````````````````````````````````
; Generates a GPT-3 prompt for rewriting an utterance (a string) as a gist
; clause, given a prior gist clause (a string) and a set of examples (a
; list of 3-tuples of strings).
;
  (let (prompt)
    (setq prompt "I want you to rewrite the utterance sentences I give you in a maximally context-independent and explicit way, given a context sentence. ")
    (setq prompt (concatenate 'string prompt "Only generate a single sentence, and try to keep it as short as possible, without redundant information."))
    (setq prompt (concatenate 'string prompt
      "[N][N]"
      ; Add examples to prompt
      (generate-prompt-preprocess-gist-examples examples)
      "[N][N]"
      ; Add prior-gist-clause and utterance to prompt
      "Context: \"" prior-gist-clause "\"[N]"
      "Utterance: \"" utterance "\"[N]"
      "Rewritten:"))
    prompt
)) ; END generate-prompt-gist



(defun trim-all-newlines (str)
;````````````````````````````````
; Trims all newline characters from the beginning of a string.
;
  (string-left-trim (string #\newline)
    (string-left-trim (string #\return)
      (string-left-trim (coerce '(#\return #\newline) 'string) str)))
) ; END trim-all-newlines



(defun get-gpt3-emotion (utterance history &key (emotions *emotions-list*))
;`````````````````````````````````````````````````````````````````````````````````````````````````````````
; Uses GPT-3 to classify the emotion of an utterance as one of the given emotions.
;
  (let (prompt stop-seq generated emotion)
    (setq emotions (mapcar (lambda (e)
        (format nil "~:(~a~)" (coerce (cdr (butlast (explode e))) 'string)))
      emotions))
    (setq prompt (generate-prompt-emotion utterance history emotions))
    ;; (format t "~%  gpt-3 prompt:~%-------------~%~a~%-------------~%" prompt) ; DEBUGGING
    (setq stop-seq (vector
      (generate-prompt-turn-start (format nil "~:(~a~)" *^you*))
      (generate-prompt-turn-start (format nil "~:(~a~)" *^me*))))
    ;; (format t "~%  gpt-3 stop-seq: ~s~%" stop-seq) ; DEBUGGING
    (setq generated (gpt3-generate (get-api-key "openai") prompt :stop-seq stop-seq))
    ;; (format t "~%  gpt-3 response:~%-------------~%~a~%-------------~%" generated) ; DEBUGGING
    (setq emotion (format nil "~:(~a~)" (string-trim " " (trim-all-newlines generated))))
    (if (member emotion emotions :test #'equal)
      (read-from-string (format nil "[~a]" emotion))
      '[NEUTRAL])
)) ; END get-gpt3-emotion



(defun get-gpt3-paraphrase (facts examples prev-utterance gist-clause)
;```````````````````````````````````````````````````````````````````````````
; Generates a GPT-3 paraphrase given a prompt containing facts, which is a list of
; strings; examples, which is a list of 3-tuples of strings representing example
; paraphrases; prev-utterance, which is a string, and gist-clause, which is a string.
; Returns a list of words.
;
  (let (prompt stop-seq generated)
    (setq prompt (generate-prompt-paraphrase facts examples prev-utterance gist-clause))
    ;; (format t "~%  gpt-3 prompt:~%-------------~%~a~%-------------~%" prompt) ; DEBUGGING
    (setq stop-seq (vector
      (generate-prompt-turn-start (format nil "~:(~a~)" *^you*))
      (generate-prompt-turn-start (format nil "~:(~a~)" *^me*))
      "Person A"
      "Person B"))
    ;; (format t "~%  gpt-3 stop-seq: ~s~%" stop-seq) ; DEBUGGING
    (setq generated (gpt3-generate (get-api-key "openai") prompt :stop-seq stop-seq))
    ;; (format t "~%  gpt-3 response:~%-------------~%~a~%-------------~%" generated) ; DEBUGGING
    (parse-chars (coerce (trim-all-newlines generated) 'list))
)) ; END get-gpt3-paraphrase



(defun get-gpt3-response (facts history)
;``````````````````````````````````````````
; Generates a GPT-3 response from facts, which is a list
; of strings, and history, which is a list of lists (agent turn)
; where agent and turn are both strings.
; Returns a list of words.
;
  (let (prompt stop-seq generated)
    (setq prompt (generate-prompt-unconstrained facts history))
    ;; (format t "~%  gpt-3 prompt:~%-------------~%~a~%-------------~%" prompt) ; DEBUGGING
    (setq stop-seq (vector
      (generate-prompt-turn-start (format nil "~:(~a~)" *^you*))
      (generate-prompt-turn-start (format nil "~:(~a~)" *^me*))))
    ;; (format t "~%  gpt-3 stop-seq: ~s~%" stop-seq) ; DEBUGGING
    (setq generated (gpt3-generate (get-api-key "openai") prompt :stop-seq stop-seq))
    ;; (format t "~%  gpt-3 response:~%-------------~%~a~%-------------~%" generated) ; DEBUGGING
    ; Hack to remove parentheticals that GPT-3 sometimes generates
    (setq generated
      (str-replace (str-replace (str-replace (str-replace generated "* " "] ") "*" "[") "(" "[") ")" "]"))
    (parse-chars (coerce (trim-all-newlines generated) 'list))
)) ; END get-gpt3-response



(defun get-gpt3-gist (examples utterance prior-gist-clause)
;``````````````````````````````````````````````````````````````
; Uses GPT-3 to rewrite an utterance as a gist clause using the context
; of the prior gist clause in the conversation, given a list of examples,
; which are 3-tuples of strings representing example gist clause interpretations.
; Returns a list of words, or nil if no gist clause was found.
;
; TODO: currently, the full generation is treated as a single gist clause, even
; if multiple sentences are generated. In the future, we may want to split these
; up based on punctuation (but we'd need to be certain that GPT-3 is reliable in
; having each generated sentence be fully explicit and context-independent).
;
  (let (prompt stop-seq generated)
    (setq prompt (generate-prompt-gist examples utterance prior-gist-clause))
    ;; (format t "~%  gpt-3 prompt:~%-------------~%~a~%-------------~%" prompt) ; DEBUGGING
    (setq stop-seq (vector
      "Context:"
      "Utterance:"
      "Rewritten:"))
    ;; (format t "~%  gpt-3 stop-seq: ~s~%" stop-seq) ; DEBUGGING
    (setq generated (gpt3-generate (get-api-key "openai") prompt :stop-seq stop-seq))
    (setq generated (string-trim '(#\" #\ ) (trim-all-newlines generated)))
    ;; (format t "~%  gpt-3 gist:~%-------------~%~a~%-------------~%" generated) ; DEBUGGING
    (if (member (string-downcase generated) '("none" "nil") :test #'equal)
      nil
      (list (parse-chars (coerce generated 'list))))
)) ; END get-gpt3-gist



(defun gpt3-generate (api-key prompt &key stop-seq)
;``````````````````````````````````````````````````````````
; A wrapper function for calling the gpt3-shell package.
; 
  (gpt3-shell:generate-safe 'gpt3-shell:generate-with-key
    (list (get-api-key "openai") prompt :stop-seq stop-seq))
) ; END gpt3-generate



;``````````````````````````````````````````````````````
;
; [*] ULF PARSER UTIL
;
;``````````````````````````````````````````````````````



(defun parse-str-to-ulf-bllip (str)
;``````````````````````````````````````````
; Parses a string into a ULF using the BLLIP-based ULF parser
; (i.e., the :lenulf package).
; 
; NOTE: due to the way that symbols are interned in the :lenulf package,
; this requires temporarily entering the :lenulf package to run the parser,
; and then re-entering :cl-user, which is the default package that Eta is assumed
; to be running within. Furthermore, the :cl-user package must be passed as an
; argument to standardize-ulf in order to intern the final symbols in the correct package.
; This may need to be changed in the future if it ends up becoming a problem.
;
  (let (ulf)
    (in-package :lenulf)
    (setq ulf (standardize-ulf:standardize-ulf
        (lenulf:remove-token-indices (lenulf:english-to-ulf str))
      :pkg :cl-user))
    (in-package :cl-user)
    ulf
)) ; END parse-str-to-ulf-bllip



;``````````````````````````````````````````````````````
;
; [*] INFORMATION RETRIEVAL UTIL
;
;``````````````````````````````````````````````````````



(defun precompute-knowledge-embeddings (knowledge)
;``````````````````````````````````````````````````````
; Precomputes embeddings for a given list of knowledge strings,
; and dumps the knowledge+embeddings into a CSV file for future use.
;
  (information-retrieval:embed-documents knowledge
    :filename (get-io-path "knowledge_embedded.csv"))
) ; END precompute-knowledge-embeddings



(defun retrieve-relevant-knowledge-from-kb (text &optional (n 5))
;``````````````````````````````````````````````````````````````````
; Retrieves the n most relevant facts to an input text from the
; knowledge base (assuming that the knowledge base has been
; previously embedded and stored in a CSV file).
;
  (coerce
    (information-retrieval:retrieve text
      :filename (get-io-path "knowledge_embedded.csv"))
    'list)
) ; END retrieve-relevant-knowledge-from-kb



;``````````````````````````````````````````````````````
;
; [*] ERROR UTIL
;
;``````````````````````````````````````````````````````



(defun error-check (&key caller)
;`````````````````````````````````
; Checks whether program has entered an infinite loop using a counter
;
  (cond
    ((> *error-check* 1000)
      (if caller
        (error-message (format nil "An error caused Eta to fall into an infinite loop in '~a'. Check if the plan is being updated correctly." caller))
        (error-message "An error caused Eta to fall into an infinite loop. Check if the plan is being updated correctly."))
      (error))
    (t (setq *error-check* (1+ *error-check*))))
) ; END error-check