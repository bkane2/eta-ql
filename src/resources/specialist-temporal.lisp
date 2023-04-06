;; March 20/2020
;; ================================================
;;
;; Functions to help Eta work with time, such as comparisons, relation-checking,
;; and maintaining/updating knowledge of time.
;; TODO: clean up preposition/adverb definitions by factoring out mod-a conditional into
;; function, and passing all functions as keyword arguments.
;; TODO: the numerical predicates in here should eventually be replaced with general versions
;; so that any number is recognized and processed correctly. I have to think more about how to do this.
;;

(in-package :eta)

; List of supported temporal prepositions (potentially aliased in the case of prepositions
; with multiple senses or ones that are essentially synonyms)
(defvar *temporal-prep-list*
  '(during.p (at.p during.p) (in.p during.p) (on.p during.p) (when.p during.p) (where.p during.p) (while.p during.p) (within.p during.p)
    before.p (prior_to.p before.p) (preceding.p before.p) (until.p before.p)
    after.p (following.p after.p) (since.p after.p) (from.p after.p)))

; List of supported temporal adjectives (potentially aliased in case of synonyms)
(defvar *temporal-adj-list*
  '(original.a (initial.a original.a) first.a second.a third.a last.a (final.a last.a) current.a (now.a current.a)
    recent.a previous.a (ago.a previous.a) (before.a previous.a) (preceding.a previous.a)
    next.a (after.a next.a) (following.a next.a) (future.a next.a) (later.a next.a) ever.a just.a))

; List of frequency adjectives (potentially aliased in case of synonyms)
; NOTE: never.a is here aliased to always.a because, once the negative context is extracted for relation checking in the
; historical questions, it is for all intents and purposes the same as always - e.g. "What block did I never move?" seems to
; be the same as asking "What block did I always not move?". If this turns out to not always hold, this can be changed in
; the future.
(defvar *frequency-adj-list*
  '(always.a (never.a always.a) one-time.a (once.a one-time.a) two-time.a (twice.a two-time.a) three-time.a (thrice.a three-time.a)
    four-time.a five-time.a))

; Possible temporal nouns of different categories
(defun temporal-unit-noun? (ulf)
  (member ulf '(second.n minute.n hour.n while.n)))
(defun temporal-turn-noun? (ulf)
  (member ulf '(turn.n time.n stage.n iteration.n period.n moment.n past.n)))
(defun temporal-question-noun? (ulf)
  (member ulf '(question.n utterance.n)))
(defun temporal-move-noun? (ulf)
  (member ulf '(move.n action.n step.n)))
(defun temporal-start-noun? (ulf)
  (member ulf '(start.n beginning.n)))
(defun temporal-now-noun? (ulf)
  (member ulf '(now.n present.n)))

; Possible temporal mod-a words of different categories
(defun temporal-recent-mod-a? (ulf)
  (member ulf '(recently.mod-a slightly.mod-a)))
(defun temporal-not-mod-a? (ulf)
  (member ulf '(not.mod-a)))
(defun temporal-just-mod-a? (ulf)
  (member ulf '(just.mod-a only.mod-a exactly.mod-a precisely.mod-a right.mod-a directly.mod-a immediately.mod-a)))
(defun temporal-most-mod-a? (ulf)
  (member ulf '(most.mod-a very.mod-a)))
(defun temporal-few-mod-a? (ulf)
  (member ulf '(few.mod-a couple.mod-a)))
(defun temporal-count-mod-a? (ulf)
  (numerical-mod-a? ulf))


; Plural noun without any sort of modifier resolves to 3 by default
(defvar *temporal-plur-value* 3)

; "Recently" is considered anything within a 2 minute threshold
(defvar *recently-threshold* 120)

; "n hour ago" is +- 30 mins, "n minute ago" is +- 60 secs, "n seconds ago" is +- 10 secs
(defvar *delta-seconds* 10)
(defvar *delta-minutes* 60)
(defvar *delta-hours* 1800)


(defun few-value! (mod-a)
; ``````````````````````````
; "Few" and "couple" resolve to 3 and 2, respectively.
;
  (case mod-a (few.mod-a 3) (couple.mod-a 2))
) ; END few-value!


(defun freq-np-to-adj (np)
; ``````````````````````````
; Turns an np like (three.d (plur turn.n)) into an adjective three-turn.a.
;
  (if (and (det-np? np) (numerical-det? (car np)))
    (intern (format nil "~a-TIME.A" (implode (butlast (explode (car np)) 2)))))
) ; END freq-np-to-adj


(defun update-time ()
; ``````````````````````
; Updates time to a "new period", i.e. creates a new constant denoting
; a new time period (and stores before/after relationships in context)
;
  (let ((time-old (ds-time *ds*)) time-new pred-before pred-after)
    (setq time-new (intern (format nil "NOW~a"
      (1+ (chars-to-int (cdddr (explode (ds-time *ds*))))))))
    (setf (ds-time *ds*) time-new)
    (store-time)
    (setq pred-before (list time-old 'before.p time-new))
    (setq pred-after  (list time-new 'after.p time-old))
    (store-in-context pred-before)
    (store-in-context pred-after))
) ; END update-time


(defun get-time ()
; ``````````````````
; Gets the system time and returns a record structure.
; 
  (multiple-value-bind (seconds minutes hours days months years) (get-decoded-time)
    `($ date+time :year ,years :month ,months :day ,days
                  :hour ,hours :minute ,minutes :sec ,seconds))
) ; END get-time


(defun to-universal-time (time-record)
; ``````````````````````````````````````
; Converts time record to universal time.
;
  (apply #'encode-universal-time (reverse
    (get-keyword-contents time-record '(:year :month :day :hour :minute :sec))))
) ; END to-universal-time


(defun store-time ()
; ````````````````````
; Gets and stores the date-time of the current time proposition.
;
  (let ((pred-time (list (ds-time *ds*) 'at-about.p (get-time))))
    (store-in-context pred-time))
) ; END store-time


(defun get-elapsed-time (time-record)
; ````````````````````````````````````
; Gets the elapsed time between time-record and the current time as a ULF noun phrase (up to hours).
;
  (let ((seconds (- (get-universal-time) (to-universal-time time-record))) minutes hours)
    (if (> seconds 60)
      (setq minutes (floor (/ seconds 60))))
    (if (> seconds (* 60 60))
      (setq hours (floor (/ seconds (* 60 60)))))
    (cond
      (hours (list (num-to-det hours) (if (> hours 1) '(plur hour.n) 'hour.n)))
      (minutes (list (num-to-det minutes) (if (> minutes 1) '(plur minute.n) 'minute.n)))
      (seconds (list (num-to-det seconds) (if (> seconds 1) '(plur second.n) 'second.n)))))
) ; END get-elapsed-time


(defun get-time-of-episode (ep-sym)
; ``````````````````````````````````
; Gets the time record corresponding to an episode symbol.
;
  (car (find-all-instances-context `(:l (?x) (,ep-sym at-about.p ?x))))
) ; END get-time-of-episode


(defun compare-time (Ti Tj)
; ``````````````````````````
; Returns -1 if Ti is before Tj, 0 if they're the same, 1 if Ti is after Tj
;
  (let ((i (to-universal-time (get-time-of-episode Ti))) (j (to-universal-time (get-time-of-episode Tj))))
    (if (equal Ti Tj) 0
      (if (> i j) 1 -1)))
) ; END compare-time


(defun diff-times (Ti Tj)
; `````````````````````````
; Returns the difference between time Ti and Tj
;
  (let ((i (chars-to-int (cdddr (explode Ti)))) (j (chars-to-int (cdddr (explode Tj)))))
    (abs (- i j)))
) ; END diff-times


(defun get-next-time (Ti)
; `````````````````````````
; Get a constant denoting the subsequent period (related by before.p/after.p propositions in context).
;
  (car (find-all-instances-context `(:l (?x) (,Ti before.p ?x))))
) ; END get-next-time


(defun get-prev-time (Ti)
; `````````````````````````
; Get a constant denoting the previous period (related by before.p/after.p propositions in context).
;
  (car (find-all-instances-context `(:l (?x) (,Ti after.p ?x))))
) ; END get-prev-time


(defun all-times (&optional type)
; `````````````````````````````````
; Gets a list of all times (excluding the current time, unless the current time is 'NOW0).
; NOTE: if type is given, restrict to turns of only that type.
;
  (let ((type-pred (case type (question #'ask-prop?) (move #'move-prop?) (otherwise nil))))
    (labels ((all-times-recur (time)
        (cond
          ; Base case
          ((null time) nil)
          ; If time contains a proposition of the given type, add time to list
          ((if type-pred (remove-if-not type-pred
                          (mapcar #'first (get-from-context `(nil @ ,time))))
                         t)
            (cons time (all-times-recur (get-prev-time time))))
          ; Otherwise, skip this time and continue recursively
          (t (all-times-recur (get-prev-time time))))))
      (all-times-recur (get-prev-time (ds-time *ds*)))))
) ; END all-times


(defun nth-time (times n)
; `````````````````````````
; Gets the nth time in times (in temporal order).
;
  ; If we're looking at times which have had the relevant relation evaluated (i.e.,
  ; at least one of the times has facts attached), only consider times with relevant facts.
  (if (some (lambda (time) (get time '@)) times)
    (setq times (remove-if-not (lambda (time) (get time '@)) times)))
  ; Sort in temporal order and get nth time
  (let ((sorted (sort (copy-seq times) (lambda (x y) (is-before x y)))))
    (nth n sorted))
) ; END nth-time


(defun latest-time (times &optional n)
; `````````````````````````````````````
; Gets the most recent time (or n most recent times, if n is given).
;
  ; If we're looking at times which have had the relevant relation evaluated (i.e.,
  ; at least one of the times has facts attached), only consider times with relevant facts.
  (if (some (lambda (time) (get time '@)) times)
    (setq times (remove-if-not (lambda (time) (get time '@)) times)))
  ; Sort in temporal order and get most recent time(s)
  (let ((sorted (sort (copy-seq times) (lambda (x y) (is-before x y)))))
    (if (null n) (car (last sorted))
      (reverse (last sorted n))))
) ; END latest-time


(defun earliest-time (times &optional n)
; ```````````````````````````````````````
; Gets the least recent time (or n least recent times, if n is given).
;
  ; Sort in temporal order and get earliest time(s)
  (let ((sorted (sort (copy-seq times) (lambda (x y) (is-before x y)))))
    (if (null n) (car sorted)
      (front sorted n)))
) ; END earliest-time


(defun time-np-to-num (np)
; ````````````````````````
; Converts an np such as "three turns" to the number 3.
;
  (let ((det (first np)) (noun (second np)) count type)
    (setq count (count-np np))
    (setq type (get-head-noun noun))
    count)
) ; END time-np-to-num


(defun time-np-to-seconds (np)
; `````````````````````````````
; Converts an np such as "three minutes" to some interval of seconds within some delta (in this case,
; (210 150), i.e. between 210 and 150 seconds)
;
  (let ((det (first np)) (noun (second np)) count type)
    (setq count (count-np np))
    (setq type (get-head-noun noun))
    (cond
      ((equal type 'second.n)
        (list (+ count *delta-seconds*) (- count *delta-seconds*)))
      ((equal type 'minute.n)
        (list (+ (* count 60) *delta-minutes*) (- (* count 60) *delta-minutes*)))
      ((equal type 'hour.n)
        (list (+ (* count 3600) *delta-hours*) (- (* count 3600) *delta-hours*)))
      ((equal type 'while.n) 
        (list 7200 1800); for now we treat "while" as being between 2 hours & 30 minutes ago, though this is arbitrary
      )))
) ; END time-np-to-seconds


(defun is-apart (time1 time2 interval)
; ```````````````````````````````````````
; Checks whether the time difference between time1 and time2 lies within some interval.
; 
  (let* ((utime1 (to-universal-time (get-time-of-episode time1)))
         (utime2 (to-universal-time (get-time-of-episode time2)))
         (diff (abs (- utime2 utime1))))
    (and (<= diff (first interval)) (>= diff (second interval))))
) ; END is-apart


(defun is-apart-now (time interval)
; ````````````````````````````````````
; Checks whether the time difference between time and the present time lies within some interval.
; 
  (let* ((utime1 (to-universal-time (get-time-of-episode time)))
         (utime2 (get-universal-time))
         (diff (abs (- utime2 utime1))))
    (and (<= diff (first interval)) (>= diff (second interval))))
) ; END is-apart-now


(defun is-apart-type (time1 time2 type n)
; ````````````````````````````````````````
; Checks whether time1 and time2 are apart by n instances of some prop.
;
  (let ((type-pred (case type (question #'ask-prop?) (move #'move-prop?) (otherwise nil))))
    (labels
      ; Past direction
      ((is-apart-type-prev (time-prev m)
        (cond
          ; If reach end
          ((null time-prev) nil)
          ; If all n hops have been taken, see if landed on time1
          ((<= m 0) (equal time1 time-prev))
          ; Otherwise, take a hop in the past direction
          (t
            (setq m (- m (if (or (not type-pred) (remove-if-not type-pred
                              (mapcar #'first (get-from-context `(nil @ ,time-prev)))))
                            1 0)))
            (if (<= m 0) (equal time1 time-prev)
              (is-apart-type-prev (get-prev-time time-prev) m)))))
       ; Future direction
       (is-apart-type-next (time-next m)
        (cond
          ; If reach end
          ((null time-next) nil)
          ; If all n hops have been taken, see if landed on time1
          ((<= m 0) (equal time1 time-next))
          ; Otherwise, take a hop in the future direction
          (t 
            (setq m (- m (if (or (not type-pred) (remove-if-not type-pred
                              (mapcar #'first (get-from-context `(nil @ ,time-next)))))
                            1 0)))
              (if (<= m 0) (equal time1 time-next)
                (is-apart-type-next (get-next-time time-next) m))))))
      (or (is-apart-type-prev time2 n) (is-apart-type-next time2 n))))
) ; END is-apart-type


(defun is-recent (time1 time2)
; `````````````````````````````
; Checks if time1 is recent to time2, i.e. if the difference between
; the two is within the recency threshold.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time1)))
        (utime2 (to-universal-time (get-time-of-episode time2))))
    (< (abs (- utime2 utime1)) *recently-threshold*))
) ; END is-recent


(defun is-recent-now (time)
; ```````````````````````````
; Checks if time is recent to the current time.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time)))
        (utime2 (get-universal-time)))
    (< (abs (- utime2 utime1)) *recently-threshold*))
) ; END is-recent-now


(defun is-prev (time1 time2)
; `````````````````````````````
; Checks if time1 is the previous time to time2.
;
  (equal time1 (get-prev-time time2))
) ; END is-prev


(defun is-before (time1 time2)
; `````````````````````````````
; Checks if time1 is before time2.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time1)))
        (utime2 (to-universal-time (get-time-of-episode time2))))
    (or (< utime1 utime2) (is-prev time1 time2) (is-prev time1 (get-prev-time time2))))
) ; END is-before


(defun is-before-now (time)
; `````````````````````````````````
; Checks if time is before the present time.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time)))
        (utime2 (get-universal-time)))
    (or (< utime1 utime2)))
) ; END is-before-now


(defun is-next (time1 time2)
; ```````````````````````````
; Checks if time1 is the next time to time2.
;
  (equal time1 (get-next-time time2))
) ; END is-next


(defun is-after (time1 time2)
; `````````````````````````````
; Checks if time1 is after time2.
;
  (let ((utime1 (to-universal-time (get-time-of-episode time1)))
        (utime2 (to-universal-time (get-time-of-episode time2))))
    (or (> utime1 utime2) (is-next time1 time2) (is-next time1 (get-next-time time2))))
) ; END is-after


(defun limit-props-by-freq (times n)
; ```````````````````````````````````````
; Given a list of times, each one having a list of propositions associated, and some
; frequency n, go through list of times and filter out any propositions which don't
; occur n times. Return only the times which still have propositions associated.
; NOTE: scalar implicatures are observed here - e.g. "what block did I move one time"
; implies no more than one time.
;
  (let ((props-table (make-hash-table :test #'equal)))
    ; Loop through times and propositions for each time
    (dolist (time times)
      (dolist (prop (get time '@))
        (cond
          ; If prop found, increment count
          ((gethash prop props-table)
            (setf (gethash prop props-table) (1+ (gethash prop props-table))))
          ; If new prop, set count to 1
          (t (setf (gethash prop props-table) 1)))))
    
    ; Now that we have counts of all propositions, loop through
    ; times again and filter all propositions that don't occur n times
    (dolist (time times)
      ;; (format t "time: ~a~%" time)
      ;; (lambda (prop) (format t " > prop ~a occurs with freq ~a~%" prop (gethash prop props-table)) (get time '@)) ; DEBUGGING
      (setf (get time '@)
        (remove-if-not (lambda (prop) (= n (gethash prop props-table))) (get time '@))))

    ; Filter only the times which have props left, and return the last one
    ; (there might be multiple prop combinations which happened n times, )
    (remove-if-not (lambda (time) (get time '@)) times))
) ; END limit-props-by-freq


(defun eval-temporal-noun (noun)
; ```````````````````````````````
; Evaluates a noun to a list of times it can denote.
; TODO: need to figure out what to do with the temporal-unit-noun? case.
; If someone asks something like "within the last five minutes", "minutes"
; shouldn't simply resolve to a list of all times (in this case, "last five"
; will modify the noun to select the last five turns, which isn't correct...)
;
  (cond
    ((temporal-now-noun? noun)
      (list (get-prev-time (ds-time *ds*))))
    ((temporal-start-noun? noun)
      (list 'NOW0))
    ((temporal-move-noun? noun)
      (all-times 'move))
    ((temporal-question-noun? noun)
      (all-times 'question))
    ((temporal-unit-noun? noun)
      (all-times))
    (t (all-times)))
) ; END eval-temporal-noun


(defun eval-temporal-relation (rel time1 time2 &optional mod-a)
; ```````````````````````````````````````````````````````````````````
; Returns a boolean value for the relation rel between time1 and time2.
; time2 is a symbol representing a time individual, e.g. NOW0, or a
; set of times, e.g. (set-of NOW0 NOW1). time1 is a time individual.
; If mod-a is given, this is passed to the predicate.
; NOTE: if relation is not found, return true by default.
;
  (let* ((prep-lookup (find-car rel *temporal-prep-list*))
         (prep (if (atom prep-lookup) prep-lookup (second prep-lookup))))
    (cond
      ((or (null time1) (null time2)) nil)
      ((and (symbolp prep) (fboundp prep))
        (funcall prep time1 time2 mod-a))
      (t t)))
) ; END eval-temporal-relation


(defun during.p (time1 time2 mod-a)
; ```````````````````````````````````
; Evaluates if time1 is during time2. If time2 is an atom, simply check if
; they're the same time, otherwise time1 is an element of the times in time2.
;
  (if (listp time2) (find time1 time2) (equal time1 time2))
) ; END during.p


(defun before.p (time1 time2 mod-a)
; ``````````````````````````````````
; Check if time1 is before time2, applying any mod-a as appropriate.
;
  (if (listp time2) (before.p time1 (earliest-time (cdr time2)) mod-a)
    (cond
      ; recently before
      ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
        (and (is-recent time1 time2) (is-before time1 time2)))
      ; not before
      ((ttt:match-expr 'temporal-not-mod-a? mod-a)
        (not (is-before time1 time2)))
      ; just before
      ((ttt:match-expr 'temporal-just-mod-a? mod-a)
        (is-prev time1 time2))
      ; two seconds/minutes/hours before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-unit-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart time1 time2 (time-np-to-seconds (cadadr mod-a)))))
      ; two turns before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-turn-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart-type time1 time2 'turn (time-np-to-num (cadadr mod-a)))))
      ; two questions before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-question-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart-type time1 time2 'question (time-np-to-num (cadadr mod-a)))))
      ; two moves before
      ((ttt:match-expr '(mod-a (by.p (^* temporal-move-noun?))) mod-a)
        (and (is-before time1 time2) (is-apart-type time1 time2 'move (time-np-to-num (cadadr mod-a)))))
      ; no/unknown mod-a
      (t (is-before time1 time2))
    ))
) ; END before.p


(defun after.p (time1 time2 mod-a)
; ``````````````````````````````````
; Check if time1 is after time2, applying any mod-a as appropriate.
;
  (if (listp time2) (after.p time1 (latest-time (cdr time2)) mod-a)
    (cond
      ; recently after
      ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
        (and (is-recent time1 time2) (is-after time1 time2)))
      ; not after
      ((ttt:match-expr 'temporal-not-mod-a? mod-a)
        (not (is-after time1 time2)))
      ; just after
      ((ttt:match-expr 'temporal-just-mod-a? mod-a)
        (is-next time1 time2))
      ; two seconds/minutes/hours after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-unit-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart time1 time2 (time-np-to-seconds (cadadr mod-a)))))
      ; two turns after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-turn-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart-type time1 time2 'turn (time-np-to-num (cadadr mod-a)))))
      ; two questions after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-question-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart-type time1 time2 'question (time-np-to-num (cadadr mod-a)))))
      ; two moves after
      ((ttt:match-expr '(mod-a (by.p (^* temporal-move-noun?))) mod-a)
        (and (is-after time1 time2) (is-apart-type time1 time2 'move (time-np-to-num (cadadr mod-a)))))
      ; no/unknown mod-a
      (t (is-after time1 time2))
    ))
) ; END after.p


(defun eval-temporal-modifier (adj times &optional mod-a)
; `````````````````````````````````````````````````````````
; Applies an adjectival modifier to times to retrieve a subset of times.
; If mod-a is given, this is passed to the predicate.
; NOTE: if relation is not found, return times by default.
;
  (let* ((mod-lookup (find-car adj *temporal-adj-list*))
         (mod (if (atom mod-lookup) mod-lookup (second mod-lookup))))
    (cond
      ((or (null times) (not (listp times))) nil)
      ((and (symbolp mod) (fboundp mod))
        (funcall mod times mod-a))
      (t times)))
) ; END eval-temporal-modifier


(defun original.a (times mod-a)
; ```````````````````````````
; Select the original time(s) in times, applying any mod-a as appropriate.
; Synonyms: initial.a
;
  (cond
    ; not original
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (set-difference times (earliest-time times 1)))
    ; just original
    ((ttt:match-expr 'temporal-just-mod-a? mod-a)
      (earliest-time times 1))
    ; most original
    ((ttt:match-expr 'temporal-most-mod-a? mod-a)
      (earliest-time times 1))
    ; few original
    ((ttt:match-expr 'temporal-few-mod-a? mod-a)
      (earliest-time times (few-value! mod-a)))
    ; two original
    ((ttt:match-expr 'temporal-count-mod-a? mod-a)
      (earliest-time times (numerical-mod-a! mod-a)))
    ; original + plural noun
    ((ttt:match-expr 'plur.mod-a mod-a)
      (earliest-time times *temporal-plur-value*))
    ; no/unknown mod-a
    (t (earliest-time times 1)))
) ; END original.a


(defun first.a (times mod-a)
; ```````````````````````````
; Select the first time(s) in times, applying any mod-a as appropriate.
;
  (cond
    ; not first
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (set-difference times (earliest-time times 1)))
    ; just first
    ((ttt:match-expr 'temporal-just-mod-a? mod-a)
      (earliest-time times 1))
    ; most first
    ((ttt:match-expr 'temporal-most-mod-a? mod-a)
      (earliest-time times 1))
    ; few first
    ((ttt:match-expr 'temporal-few-mod-a? mod-a)
      (earliest-time times (few-value! mod-a)))
    ; two first
    ((ttt:match-expr 'temporal-count-mod-a? mod-a)
      (earliest-time times (numerical-mod-a! mod-a)))
    ; first + plural noun
    ((ttt:match-expr 'plur.mod-a mod-a)
      (earliest-time times *temporal-plur-value*))
    ; no/unknown mod-a
    (t (list (nth-time times 0))))
) ; END first.a


(defun second.a (times mod-a)
; ````````````````````````````
; Select the first time(s) in times, applying any mod-a as appropriate.
;
  (cond
    ; not adj
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (set-difference times (list (nth-time times 1))))
    ; no/unknown mod-a
    (t (list (nth-time times 1))))
) ; END second.a


(defun third.a (times mod-a)
; ````````````````````````````
; Select the first time(s) in times, applying any mod-a as appropriate.
;
  (cond
    ; not adj
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (set-difference times (list (nth-time times 2))))
    ; no/unknown mod-a
    (t (list (nth-time times 2))))
) ; END third.a


(defun last.a (times mod-a)
; ```````````````````````````````
; Select the last time(s) in times, applying any mod-a as appropriate.
; Synonyms: final.a
;
  (cond
    ; not last
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (set-difference times (latest-time times 1)))
    ; just last
    ((ttt:match-expr 'temporal-just-mod-a? mod-a)
      (latest-time times 1))
    ; most last
    ((ttt:match-expr 'temporal-most-mod-a? mod-a)
      (latest-time times 1))
    ; few last
    ((ttt:match-expr 'temporal-few-mod-a? mod-a)
      (latest-time times (few-value! mod-a)))
    ; two last
    ((ttt:match-expr 'temporal-count-mod-a? mod-a)
      (latest-time times (numerical-mod-a! mod-a)))
    ; last + plural noun
    ((ttt:match-expr 'plur.mod-a mod-a)
      (latest-time times *temporal-plur-value*))
    ; no/unknown mod-a
    (t (latest-time times 1)))
) ; END last.a


(defun current.a (times mod-a)
; ```````````````````````````````
; Select the time corresponding to the current time, applying any mod-a as appropriate.
; NOTE: at the time of utterance being evaluated, the time of "now" is one step earlier than (ds-time *ds*)
; NOTE: the only mod-a that matters here is 'not', e.g. "not now"
; Synonyms: now.a
;
  (cond
    ; not current
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (set-difference times (list (get-prev-time (ds-time *ds*)))))
    ; no/unknown mod-a
    (t (list (get-prev-time (ds-time *ds*)))))
) ; END current.a


(defun recent.a (times mod-a)
; ```````````````````````````````
; Select the recent time(s) in times, applying any mod-a as appropriate.
;
  (cond
    ; not recent
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (set-difference times (remove-if-not #'is-recent-now times)))
    ; just recent
    ((ttt:match-expr 'temporal-just-mod-a? mod-a)
      (remove-if-not #'is-recent-now times))
    ; most recent
    ((ttt:match-expr 'temporal-most-mod-a? mod-a)
      (latest-time times 1))
    ; few recent
    ((ttt:match-expr 'temporal-few-mod-a? mod-a)
      (latest-time times (few-value! mod-a)))
    ; two recent
    ((ttt:match-expr 'temporal-count-mod-a? mod-a)
      (latest-time times (numerical-mod-a! mod-a)))
    ; recent + plural noun
    ((ttt:match-expr 'plur.mod-a mod-a)
      (remove-if-not #'is-recent-now times))
    ; no/unknown mod-a
    (t (remove-if-not #'is-recent-now times)))
) ; END recent.a


(defun previous.a (times mod-a)
; ```````````````````````````````
; Select the previous time(s) in times, applying any mod-a as appropriate.
; NOTE: if mod-a is some measure, e.g. in "two turns ago", this selects any time
; in times which is two turns previously from the present time, or nil if none exists.
; Synonyms: ago.a, before.a, preceding.a
;
  (cond
    ; recently previous
    ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
      (remove-if-not #'is-recent-now times))
    ; not previous
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      (remove-if-not #'is-recent-now times))
    ; just previous
    ((ttt:match-expr 'temporal-just-mod-a? mod-a)
      (latest-time times 1))
    ; most previous
    ((ttt:match-expr 'temporal-most-mod-a? mod-a)
      (earliest-time times 1))
    ; few previous
    ((ttt:match-expr 'temporal-few-mod-a? mod-a)
      (latest-time times (few-value! mod-a)))
    ; two previous
    ((ttt:match-expr 'temporal-count-mod-a? mod-a)
      (latest-time times (numerical-mod-a! mod-a)))
    ; two seconds/minutes/hours previous
    ((ttt:match-expr '(mod-a (by.p (^* temporal-unit-noun?))) mod-a)
      (remove-if-not (lambda (time)
        (is-apart-now time (time-np-to-seconds (cadadr mod-a)))) times))
    ; two turns previous
    ((ttt:match-expr '(mod-a (by.p (^* temporal-turn-noun?))) mod-a)
      (remove-if-not (lambda (time)
        (is-apart-type time (ds-time *ds*) 'turn (time-np-to-num (cadadr mod-a)))) times))
    ; two questions previous
    ((ttt:match-expr '(mod-a (by.p (^* temporal-question-noun?))) mod-a)
      (remove-if-not (lambda (time)
        (is-apart-type time (ds-time *ds*) 'question (time-np-to-num (cadadr mod-a)))) times))
    ; two moves previous
    ((ttt:match-expr '(mod-a (by.p (^* temporal-move-noun?))) mod-a)
      (remove-if-not (lambda (time)
        (is-apart-type time (ds-time *ds*) 'move (time-np-to-num (cadadr mod-a)))) times))
    ; previous + plural noun
    ((ttt:match-expr 'plur.mod-a mod-a)
      (latest-time times *temporal-plur-value*))
    ; no/unknown mod-a
    (t (latest-time times 3)))
) ; END previous.a


(defun next.a (times mod-a)
; ```````````````````````````````
; Select the next time(s) in times, applying any mod-a as appropriate.
; NOTE: if mod-a is some measure, e.g. in "two turns ago", this selects any time
; in times which is two turns next from the present time, or nil if none exists.
; Synonyms: after.a, later.a, following.a, future.a
; TODO: the meaning of "two turns later" isn't yet captured properly here, since
; this relies on an aspectual shift to some reference point in the past.
;
  (cond
    ; just next
    ((ttt:match-expr 'temporal-just-mod-a? mod-a)
      (earliest-time times 1))
    ; most next
    ((ttt:match-expr 'temporal-most-mod-a? mod-a)
      (latest-time times 1))
    ; few next
    ((ttt:match-expr 'temporal-few-mod-a? mod-a)
      (earliest-time times (few-value! mod-a)))
    ; two next
    ((ttt:match-expr 'temporal-count-mod-a? mod-a)
      (earliest-time times (numerical-mod-a! mod-a)))
    ;; ; two seconds/minutes/hours next
    ;; ((ttt:match-expr '(mod-a (by.p (^* temporal-unit-noun?))) mod-a)
    ;;   (remove-if-not (lambda (time)
    ;;     (is-apart-now time (time-np-to-seconds (cadadr mod-a)))) times))
    ;; ; two turns next
    ;; ((ttt:match-expr '(mod-a (by.p (^* temporal-turn-noun?))) mod-a)
    ;;   (remove-if-not (lambda (time)
    ;;     (is-apart-type time (ds-time *ds*) 'turn (time-np-to-num (cadadr mod-a)))) times))
    ;; ; two questions next
    ;; ((ttt:match-expr '(mod-a (by.p (^* temporal-question-noun?))) mod-a)
    ;;   (remove-if-not (lambda (time)
    ;;     (is-apart-now time (ds-time *ds*) 'question (time-np-to-num (cadadr mod-a)))) times))
    ;; ; two moves next
    ;; ((ttt:match-expr '(mod-a (by.p (^* temporal-move-noun?))) mod-a)
    ;;   (remove-if-not (lambda (time)
    ;;     (is-apart-now time (ds-time *ds*) 'move (time-np-to-num (cadadr mod-a)))) times))
    ; next + plural noun
    ((ttt:match-expr 'plur.mod-a mod-a)
      (earliest-time times *temporal-plur-value*))
    ; no/unknown mod-a
    (t (earliest-time times 3)))
) ; END next.a


(defun ever.a (times mod-a)
; ```````````````````````````````
; Select all time(s) in times, applying any mod-a as appropriate.
;
  (cond
    ; recently ever
    ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
      (remove-if-not #'is-recent-now times))
    ; no/unknown mod-a
    (t times))
) ; END ever.a


(defun just.a (times mod-a)
; ```````````````````````````````
; Select the time in times that happens to be the previous turn, or nil of none exist,
; applying any mod-a as appropriate.
;
  (cond
    ; no/unknown mod-a
    (t (remove-if-not (lambda (time) (is-prev time (get-prev-time (ds-time *ds*)))) times)))
) ; END just.a


(defun template.a (times mod-a)
; ```````````````````````````````
; Select the [] time(s) in times, applying any mod-a as appropriate.
; Synonyms: []
;
  (cond
    ; recently template
    ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
      nil)
    ; not template
    ((ttt:match-expr 'temporal-not-mod-a? mod-a)
      nil)
    ; just template
    ((ttt:match-expr 'temporal-just-mod-a? mod-a)
      nil)
    ; most template
    ((ttt:match-expr 'temporal-most-mod-a? mod-a)
      nil)
    ; few template
    ((ttt:match-expr 'temporal-few-mod-a? mod-a)
      nil)
    ; two template
    ((ttt:match-expr 'temporal-count-mod-a? mod-a)
      nil)
    ; two seconds/minutes/hours template
    ((ttt:match-expr '(mod-a (by.p (^* temporal-unit-noun?))) mod-a)
      nil)
    ; two turns template
    ((ttt:match-expr '(mod-a (by.p (^* temporal-turn-noun?))) mod-a)
      nil)
    ; two questions template
    ((ttt:match-expr '(mod-a (by.p (^* temporal-question-noun?))) mod-a)
      nil)
    ; two moves template
    ((ttt:match-expr '(mod-a (by.p (^* temporal-move-noun?))) mod-a)
      nil)
    ; template + plural noun
    ((ttt:match-expr 'plur.mod-a mod-a)
      nil)
    ; no/unknown mod-a
    (t nil))
) ; END template.a


(defun eval-frequency-modifier (adj times &optional mod-a)
; ``````````````````````````````````````````````````````````
; Applies an adjectival frequency modifier to times (with props attached) to
; retrieve a subset of times which have props satisfying the given frequency,
; and the props limited to only those props.
;
  (let* ((mod-lookup (find-car adj *frequency-adj-list*))
         (mod (if (atom mod-lookup) mod-lookup (second mod-lookup))))
    (cond
      ((or (null times) (not (listp times))) nil)
      ((and (symbolp mod) (fboundp mod))
        (funcall mod times mod-a))
      (t times)))
) ; END eval-frequency-modifier


(defun always.a (times mod-a)
; ``````````````````````````````
; Select the times & props such that each prop occurs at every time. This is the same
; as ensuring that each prop occurs n times, where n is the length of times.
;
  (cond
    ; recently always
    ((ttt:match-expr 'temporal-recent-mod-a? mod-a)
      (let ((recent-times (remove-if-not #'is-recent-now times)))
        (limit-props-by-freq recent-times (length recent-times))))
    ; no/unknown mod-a
    (t (limit-props-by-freq times (length times))))
) ; END always.a


(defun one-time.a (times mod-a)
; ``````````````````````````````
; Select the times & props that occur one time (and no more than once).
;
  (limit-props-by-freq times 1)
) ; END one-time.a


(defun two-time.a (times mod-a)
; ``````````````````````````````
; Select the times & props that occur two times (and no more).
;
  (limit-props-by-freq times 2)
) ; END two-time.a


(defun three-time.a (times mod-a)
; ````````````````````````````````
; Select the times & props that occur three times (and no more).
;
  (limit-props-by-freq times 3)
) ; END three-time.a


(defun four-time.a (times mod-a)
; `````````````````````````````````
; Select the times & props that occur four times (and no more).
;
  (limit-props-by-freq times 4)
) ; END four-time.a


(defun five-time.a (times mod-a)
; `````````````````````````````````
; Select the times & props that occur five times (and no more).
;
  (limit-props-by-freq times 5)
) ; END five-time.a