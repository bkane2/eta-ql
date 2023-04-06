;; Dec 9/19
;; ================================================
;;
;; This code is used to compute answer propositions, given a historical query ULF,
;; a list of currently observed block locations, and a record of actions stored in
;; Eta's memory.
;;
;; The initial function 'recall-answer' serves to process the block coordinates into
;; a simpler format, as well as categorize the query as a 'when' question or a historical
;; spatial question. In the case of a 'when' question, the relevant time(s) are returned
;; as answers, otherwise the relevant spatial relations are returned.
;;
;; The 'find-answer-times' function extracts any adv-e or adv-f constituents as unary &
;; binary constraints, resolving the arguments into specific times in the case of binary
;; constraints. The function also extracts information about the type of question, subject,
;; object, pos/neg context, action predicate (if any), and spatial predicate (if any), and
;; transduces this information into a relevant function call to be used by the temporal
;; backtracking algorithm.
;;
;; The 'find+constrain-times' function implements the temporal backtracking algorithm.
;; Starting from the current time, given the currently observed block coordinates, the
;; algorithm reconstructs the previous scene using the block move actions stored in Eta's
;; memory. For any time not excluded by binary temporal constraints, the relevant function
;; is called to compute propositions, which are attached to that time. At the end, frequency
;; constraints (if any) are applied to select a subset of the resulting times, any times with
;; no remaining facts attached are pruned, and finally unary constraints (if any) are applied
;; to select a final subset of times.
;;

(in-package :eta)


(defun recall-answer (object-locations ulf)
; ````````````````````````````````````````````
; Given current observed block locations and the ULF of the query, recall the answer by consulting
; historical record of block moves stored in context.
;
  (format t "object locations: ~a~%" object-locations) ; DEBUGGING
  (let ((coords (extract-coords object-locations))
        (when-question (extract-when-question ulf)) answer)

    (format t "when question: ~a~%" when-question) ; DEBUGGING

    ; If when question, remove when adv-e from ulf
    (setq ulf (remove-when-adv-e ulf))

    ; Get answer times which satisfy ULF and current coords
    (setq answer (find-answer-times coords ulf :when-question when-question))

    ; Return either times or corresponding props depending on if when-question or not.
    ; TODO: check if next time, if so return BEFORE.P <prop> as answer relation, resolved into S by response generator
    (remove-duplicates
      (if when-question
        ; If when question, get either time(s) or temporal relation
        (mapcar (lambda (prop) (add-certainty prop nil))
          (find-relation-to-next-time answer coords))
        ; Otherwise, get spatial relations
        (apply #'append (mapcar (lambda (time)
          (mapcar (lambda (prop) (add-certainty prop time)) (get time '@))) answer)))
      :test (lambda (x y) (equal (car x) (car y)))))
) ; END recall-answer


(defun find-relation-to-next-time (times scene)
; ```````````````````````````````````````````````
; Given a time (or list of times), check if the time directly after contains a move event, and if so, return
; a relation to the (reified) event. Otherwise, simply return the time/list of times.
; TODO: this function could use some cleaning.
;
  (let* ((latest (latest-time times))
         (move-before-answers (find+constrain-times '(compute-move (what.d block.n) nil)
                                               nil `((just.mod-a (after.p ,latest))) nil scene))
         (moved-before-block (if (and move-before-answers (listp move-before-answers) (get (car move-before-answers) '@))
            (caar (get (car move-before-answers) '@))))
         (move-after-answers (find+constrain-times '(compute-move (what.d block.n) nil)
                                               nil `((just.mod-a (before.p ,latest))) nil scene))
         (moved-after-block (if (and move-after-answers (listp move-after-answers) (get (car move-after-answers) '@))
            (caar (get (car move-after-answers) '@)))))
    ; If next turn has some move action, return a relation to that move, otherwise just return the original time(s).
    (cond
      (moved-before-block
        (list `(before.ps (you.pro ((past move.v) ,moved-before-block)))))
      (moved-after-block
        (list `(after.ps (you.pro ((past move.v) ,moved-after-block)))))
      (t times)))
) ; END find-relation-to-next-time


(defun find-answer-times (coords ulf &key when-question embedded)
; `````````````````````````````````````````````````````````````````
; Given coordinates and a query ulf, return a list of time/event symbols corresponding to the ULF.
;
  (let* ((ulf-base (uninvert-question (remove-not (remove-adv-e (remove-adv-f ulf)))))
         (where-question (extract-where-question ulf))
         (neg (extract-neg ulf))
         (deg-adv (extract-deg-adv ulf))
         (adv-e (extract-adv-e ulf))
         (adv-f (extract-adv-f ulf))
         constraints-unary constraints-binary constraints-freq
         (subj (extract-subj ulf-base))
         (obj (extract-obj ulf-base))
         (relation (extract-relation ulf-base))
         (action (extract-action ulf-base))
         func ans-times)

    ; Generate any pragmatic inferences for underspecified queries
    (let ((adv-inferred (infer-temporal-adverbials adv-e adv-f neg when-question where-question subj obj relation action embedded)))
      (setq adv-e (list
        (remove-duplicates (append (first adv-inferred) (first adv-e)) :test #'equal)
        (remove-duplicates (append (second adv-inferred) (second adv-e)) :test #'equal)))
      (setq adv-f
        (remove-duplicates (append (third adv-inferred) adv-f) :test #'equal)))

    ; Resolve constraints
    (setq constraints-unary (resolve-unary-constraint (first adv-e)))
    (setq constraints-binary (resolve-binary-constraint coords (second adv-e)))
    (setq constraints-freq (resolve-frequency-constraint adv-f))

    ; Select appropriate function depending on whether it's a where-question, asking about a relation,
    ; asking about an action, or both (e.g. "when did I put the Twitter block on the Starbucks block")
    (cond
      ((and where-question action)
        (setq func `(compute-move-relations ,obj)))
      (where-question
        (setq func `(compute-relations ,subj)))
      ((and relation (equal (second relation) '=))
        (setq func `(compute-predicative ,subj ,obj)))
      ((and relation action)
        (setq func `(compute-move+relation ,relation ,obj ,neg ,deg-adv)))
      ((and relation (>= (length relation) 3))
        (setq func `(compute-relation ,relation ,neg ,deg-adv)))
      (relation
        (setq func `(compute-modifier ,relation ,neg)))
      (action
        (setq func `(compute-move ,obj ,neg))))

    ; Print info for debugging
    (when (not embedded)
      (format t "where question: ~a~%" where-question)
      (format t "neg: ~a~%" neg)
      (format t "deg-adv: ~a~%" deg-adv)
      (format t "adv-e: ~a~%" adv-e)
      (format t "constraint-unary: ~a~%" constraints-unary)
      (format t "constraint-binary: ~a~%" constraints-binary)
      (format t "constraint-freq: ~a~%" constraints-freq)
      (format t "base ulf: ~a~%" ulf-base)
      (format t "subj: ~a~%" subj)
      (format t "obj: ~a~%" obj)
      (format t "extracted relation: ~a~%" relation)
      (format t "extracted action-verb: ~a~%" action)
      (format t "using function: ~a~%" func)
      (format t "blocks at coordinates: ~a~%" coords)) ; DEBUGGING

    ; Find the times where the function holds (determining the relevant answer propositions),
    ; and constrain by any given temporal adverbials
    (setq ans-times (find+constrain-times func constraints-unary constraints-binary constraints-freq coords))

    (when (not embedded)
      (format t "ans-times: ~a~%" ans-times)) ; DEBUGGING
    
  ans-times)
) ; END find-answer-times


(defun find+constrain-times (func constraints-unary constraints-binary constraints-freq coords)
; ```````````````````````````````````````````````````````````````````````````````````````````````
; Finds all times, constrained by the given adverbials, at which func holds. Return all
; satisfying times conjoined with the relevant propositions given by func.
; 
  (let ((f (car func)) (args (cdr func)) (time (ds-time *ds*)) (scene coords) moves scene1 props times)
    ; Backtrack through times until the initial time is reached
    (loop while time do

      ; Get moves and 'backup' current scene (so we can use the scene of the consecutive turn)
      (setq moves (extract-moves (mapcar #'first (get-from-context `(nil @ ,time)))))
      (setq scene1 scene)
      ; Undo all moves that happened at current time to get new scene
      (mapcar (lambda (move)
        (setq scene (subst-move-scene move scene))) moves)

      ; Only consider this time if it satisfies all binary relations given by the binary adv-e phrases
      (when (every (lambda (constraint) (apply-binary-constraint constraint time)) constraints-binary)
        ; Call func using current scene, any moves, result scene, and any special args
        (setq props (if f (apply f (append (list scene moves scene1) args))))

        (format t " > time ~a props: ~a~%" time props) ; DEBUGGING

        ; Attach propositions to time and cons to result list
        (setf (get time '@) props)
        (setq times (cons time times)))

      ; Go to previous time
      (setq time (get-prev-time time)))

    (format t "Times before applying frequency constraints: ~a~%" times) ; DEBUGGING

    ; Apply all frequency constraints phrases to select times which satisfy frequency
    (mapcar (lambda (constraint) (setq times (apply-frequency-constraint constraint times))) constraints-freq)

    (format t "Times before applying unary constraints: ~a~%" times) ; DEBUGGING

    ; Apply all unary constraints to further constrain times
    (mapcar (lambda (constraint) (setq times (apply-unary-constraint constraint times))) constraints-unary)

    (format t "Times before removing times with no props: ~a~%" times) ; DEBUGGING

    ; Remove any times with no props
    (setq times (remove-if-not (lambda (time) (get time '@)) times))

    (format t "Returning times: ~a~%~%" times) ; DEBUGGING

  times)
) ; END find+constrain-times


(defun infer-temporal-adverbials (adv-e adv-f neg when-question where-question subj obj relation action embedded)
; ````````````````````````````````````````````````````````````````````````````````````````````````````````````````
; Given an underspecified query (e.g. "what blocks did I move"), we want to generate temporal adverbials
; corresponding to the likely intended scope of the question (e.g. in this case, the speaker likely means
; something like "recently"). This uses the various ULF features extracted by the calling function. Currently
; the inferences are simple (either adding 'recently' or 'most recently'), but they can be improved as we encounter
; more pragmatic issues.
; TODO: clean function.
;
  (let ((unary-constraints (append (first adv-e) adv-f)) (binary-constraints (second adv-e))
        adv-e-unary-inferred adv-e-binary-inferred adv-f-inferred)

    (when (and neg action)
      (format t "action question in a negative context: adding 'never'~%")
      (setq adv-f-inferred (list 'never.a)))

    (cond
      (embedded nil)
      (when-question nil)
      ((and where-question action (null unary-constraints) (null binary-constraints))
        (format t "'where did I move' question with underspecified unary constraints: adding 'most recently'~%")
        (setq adv-e-unary-inferred (cons '(most.mod-a recent.a) adv-e-unary-inferred)))
      ((and where-question (null unary-constraints) (null binary-constraints))
        (format t "where question with underspecified unary & binary constraints: adding 'most recently' and 'before the last move'~%")
        (setq adv-e-unary-inferred (cons '(most.mod-a recent.a) adv-e-unary-inferred))
        (setq adv-e-binary-inferred (cons '(before.p (the.d (last.a move.n))) adv-e-binary-inferred)))
      ((and where-question (null unary-constraints))
        (format t "where question with underspecified unary constraints: adding 'most recently'~%")
        (setq adv-e-unary-inferred (cons '(most.mod-a recent.a) adv-e-unary-inferred)))
      ((and (null action) (null unary-constraints) (null binary-constraints))
        (format t "relation question with underspecified unary & binary constraints: adding 'recently' and 'before the last move'~%")
        (setq adv-e-unary-inferred (cons 'recent.a adv-e-unary-inferred))
        (setq adv-e-binary-inferred (cons '(before.p (the.d (last.a move.n))) adv-e-binary-inferred)))
      ((and (null unary-constraints) binary-constraints)
        (cond
          ((some (lambda (binary-constraint)
                (let* ((prep-lookup (find-car (car binary-constraint) *temporal-prep-list*))
                       (prep (if (atom prep-lookup) prep-lookup (second prep-lookup))))
                  (equal prep 'after.p)))
              binary-constraints)
            (format t "historical question with underspecified unary constraints involving 'after' predicate: adding 'ever'~%")
            (setq adv-e-unary-inferred (cons 'ever.a adv-e-unary-inferred)))
          ((some (lambda (binary-constraint)
                (let* ((prep-lookup (find-car (car binary-constraint) *temporal-prep-list*))
                       (prep (if (atom prep-lookup) prep-lookup (second prep-lookup))))
                  (equal prep 'before.p)))
              binary-constraints)
            (format t "historical question with underspecified unary constraints involving 'before' predicate: adding 'recently'~%")
            (setq adv-e-unary-inferred (cons 'recent.a adv-e-unary-inferred))))))
            
    (list adv-e-unary-inferred adv-e-binary-inferred adv-f-inferred))
) ; END infer-temporal-adverbials


(defun extract-where-question (ulf)
; ```````````````````````````````````
; Returns t if ULF contains an 'at what place' phrase, nil otherwise.
;
  (if (ttt:match-expr '(^* (at.p (what.d place.n))) ulf) t)
) ; END extract-where-question


(defun extract-when-question (ulf)
; ```````````````````````````````````
; Returns t if ULF contains a phrase like 'at what time' (and isn't a where question), nil otherwise.
;
  (if (and (ttt:match-expr '(^* ((! at.p in.p on.p during.p ago.p) wh-np?)) ulf)
           (not (extract-where-question ulf))) t)
) ; END extract-when-question


(defun remove-when-adv-e (ulf)
; ``````````````````````````````
; Removes an adv-e such as "(at.p (what.d time.n))" from a ULF.
;
  (ttt:apply-rules '(
    (/ (sub (prep? (wh-det? not-place.n?)) _!) _!)
    (/ (_! (adv-e (! hole? (prep? (wh-det? not-place.n?))))) _!)
    (/ (_*1 (adv-e (! hole? (prep? (wh-det? not-place.n?)))) _*2) (_*1 _*2))
  ) ulf)
) ; END remove-when-adv-e


(defun extract-neg (ulf)
; ````````````````````````
; Returns t if ULF is in a negative environment.
;
  (if (or
    (ttt:match-expr '(^* not) ulf)
    (ttt:match-expr '(^* (adv-f never.a)) ulf)) t)
) ; END extract-neg


(defun extract-deg-adv (ulf)
; ````````````````````````````
; Extracts any deg-adv modifying a spatial question.
;
  (if 
    (ttt:match-expr '(^* spatial-deg-adv?) ulf) t)
) ; END extract-deg-adv


(defun extract-action (ulf)
; ``````````````````````````
; Extracts a spatial action verb, e.g. put.v.
;
  (let ((action
      (ttt:apply-rules '(
        (/ (^* action-verb?) action-verb?))
      ulf :shallow t)))
    (if (action-verb? action) action nil))
) ; END extract-action


(defun extract-relation (ulf)
; `````````````````````````````
; Extracts a relation (e.g. ((what.d block.n) on.p (the.d (|Twitter| block.n)))) from a ULF.
;
  (let ((relation
      (ttt:apply-rules '(
        ; Straightforward preposition cases
        (/ (_!1 ((tense? be.v) _* (^* (between.p (set-of _!2 _!3)))))
           (_!1 between.p _!2 _!3))
        (/ (_!1 ((tense? be.v) _* (^* (prep? _!2))))
           (_!1 prep? _!2))
        (/ (_!1 ((tense? aspect?) (be.v _* (^* (between.p (set-of _!2 _!3))))))
           (_!1 between.p _!2 _!3))
        (/ (_!1 ((tense? aspect?) (be.v _* (^* (prep? _!2)))))
           (_!1 prep? _!2))
        ; Equality relation, e.g. "what is the leftmost block?"
        (/ (_!1 ((tense? be.v) (= _!2)))
           (_!1 = _!2))
        (/ (_!1 ((tense? aspect?) (= _!2)))
           (_!1 = _!2))
        ; "what block did I put on the Twitter block?"
        (/ (_! ((tense? verb-untensed?) _!1 (^* (between.p (set-of _!2 _!3)))))
           (_!1 between.p _!2 _!3))
        (/ (_! ((tense? verb-untensed?) _!1 (^* (prep? _!2))))
           (_!1 prep? _!2))
        (/ (_! ((tense? aspect?) (verb-untensed? _!1 (^* (between.p (set-of _!2 _!3))))))
           (_!1 between.p _!2 _!3))
        (/ (_! ((tense? aspect?) (verb-untensed? _!1 (^* (prep? _!2)))))
           (_!1 prep? _!2))
        (/ (_!1 (((! tense? (tense? aspect?)) (pasv verb-untensed?)) (^* (between.p (set-of _!2 _!3)))))
           (_!1 between.p _!2 _!3))
        (/ (_!1 (((! tense? (tense? aspect?)) (pasv verb-untensed?)) (^* (prep? _!2))))
           (_!1 prep? _!2))
        ; "what block touches the Twitter block?" (TODO: needs to be generalized)
        (/ (_!1 ((tense? spatial-verb?) _!2))
           (_!1 (spatial-verb-to-prep! spatial-verb?) _!2))
        (/ (_!1 ((tense? aspect?) (spatial-verb? _!2)))
           (_!1 (spatial-verb-to-prep! spatial-verb?) _!2))
        ; "what blocks were clear?"
        (/ (_!1 ((tense? be.v) _* adj?))
           (_!1 adj?))
        (/ (_!1 ((tense? aspect?) _* adj?))
           (_!1 adj?)))
      ulf)))
    (if (relation-prop? relation) relation nil))
) ; END extract-relation


(defun extract-subj (ulf)
; ````````````````````````
; Extracts a subject (e.g. (what.d block.n)) of a ULF.
;
  (let ((subj
      (ttt:apply-rules '(
        (/ (_!1 (! (^* (tense? verb-untensed?)))) _!1)
        (/ (_!1 (! (^* ((tense? aspect?) (verb-untensed? _*))))) _!1)
        (/ (_!1 (! (^* (pasv verb-untensed?)))) _!1))
      ulf))) 
    (if (or (np? subj) (nnp? subj) (variable? subj) (restricted-variable? subj)) subj nil))
) ; END extract-subj


(defun extract-obj (ulf)
; ````````````````````````
; Extracts an object (e.g. (the.d (|Twitter| block.n))) of a ULF.
;
  (let ((obj
    (ttt:apply-rules '(
      (/ (_! (^* ((tense? verb-untensed?) (^*2 (! wh-pron? (det? _!1) ((nquan _!2) _!3))) _*))) !)
      (/ (_! (^* ((tense? aspect?) (verb-untensed? (^*2 (! wh-pron? (det? _!1) ((nquan _!2) _!3))) _*)))) !)
      (/ (_! ((! tense? (tense? aspect?)) (pasv verb-untensed?))) _!))
    ulf)))
  (if (or (np? obj) (nnp? obj) (variable? obj) (restricted-variable? obj)) obj nil))
) ; END extract-obj


(defun extract-coords (prop-list)
; `````````````````````````````````
; Extracts all coordinates from a list of propositions, returned in the
; form: (|Name| ?x ?y ?z). NOTE: assuming that all at-loc.p propositions
; use explicit location record, i.e. ($ loc :x ?x :y ?y :z ?z)
;
  (mapcar (lambda (prop) (append (list (first prop))
                                       (get-keyword-contents (third prop) '(:x :y :z))))
    (remove-if-not #'at-loc-prop? prop-list))
) ; END extract-coords


(defun extract-questions (prop-list)
; ```````````````````````````````````
; Extracts all questions from a list of propositions, returned in the form:
; (you 'ulf)
;
  (mapcar (lambda (prop)
      (list (first prop) (second (second prop))))
    (remove-if-not #'ask-prop? prop-list))
) ; END extract-questions


(defun extract-moves (prop-list)
; ```````````````````````````````
; Extracts all moves from a list of propositions, returned in the form:
; (|Name| (?x1 ?y1 ?z1) (?x2 ?y2 ?z2))
; (|Twitter| ((pres move.v) (from.p-arg ($ loc ...)) (to.p-arg ($ loc ...)) ))
; NOTE: assuming that all move.v propositions use explicit
; location record, i.e. ($ loc :x ?x1 :y ?y1 :z ?z1)
;
  (mapcar (lambda (prop)
      (list (first prop)
            (get-keyword-contents (second (second (second prop))) '(:x :y :z))
            (get-keyword-contents (second (third  (second prop))) '(:x :y :z))))
    (remove-if-not #'move-prop? prop-list))
) ; END extract-moves


(defun extract-adv-e (ulf)
; ``````````````````````````
; Extracts all adv-e phrases from ULF (dividing into two sublists, one for unary preds,
; e.g. recent.a, and one for binary preds, e.g. before.p).
;
  (setq ulf (apply-sub-macro ulf))
  (let (adv-e-unary adv-e-binary)
    (labels ((extract-adv-e-recur (ulf-part)
        (cond
          ; If ULF is an adv-e with a unary pred, cons to adv-e-unary
          ((ttt:match-expr '(! (adv-e adj?) (adv-e (mod-a? adj?))) ulf-part)
            (setq adv-e-unary (cons (second ulf-part) adv-e-unary)))
          ; If ULF is an adv-e with a binary pred, cons to adv-e-binary
          ((ttt:match-expr '(! (adv-e prep-phrase?) (adv-e (mod-a? prep-phrase?))) ulf-part)
            (setq adv-e-binary (cons (second ulf-part) adv-e-binary)))
          ((ttt:match-expr '(! (sent-prep? _!) (mod-a? (sent-prep? _!))) ulf-part)
            (setq adv-e-binary (cons ulf-part adv-e-binary)))
          ; Otherwise, if ULF is atom, do nothing
          ((atom ulf-part) nil)
          ; Otherwise, if ULF is list, recur on each sub-part
          (t (mapcar #'extract-adv-e-recur ulf-part)))))
      (extract-adv-e-recur ulf))
    (list adv-e-unary adv-e-binary))
) ; END extract-adv-e


(defun extract-adv-f (ulf)
; ``````````````````````````
; Extracts all adv-f phrases from ULF.
;
  (setq ulf (apply-sub-macro ulf))
  (let (adv-f)
    (labels ((extract-adv-f-recur (ulf-part)
        (cond
          ; If ULF is an adv-f, cons to adv-f
          ((ttt:match-expr '(adv-f _!) ulf-part)
            (setq adv-f (cons (second ulf-part) adv-f)))
          ; Otherwise, if ULF is atom, do nothing
          ((atom ulf-part) nil)
          ; Otherwise, if ULF is list, recur on each sub-part
          (t (mapcar #'extract-adv-f-recur ulf-part)))))
      (extract-adv-f-recur ulf))
    adv-f)
) ; END extract-adv-f


(defun resolve-unary-constraint (constraints-unary)
; ``````````````````````````````````````````````````
; Resolves each unary constraint (currently does nothing).
;
  constraints-unary
) ; END resolve-unary-constraint


(defun resolve-binary-constraint (coords constraints-binary)
; ````````````````````````````````````````````````````````````
; Resolves each constraint in constraints-binary to a simple binary predicate involving a
; time individual or set of time individuals.
; e.g. (before.p (the.d (last.a turn.n))) => (before.p Now3)
;
  (mapcar (lambda (constraint)
    (ttt:apply-rules `(
      (/ (prep? np?) (prep? (resolve-time-np! ,coords np?)))
      (/ (sent-prep? _!) ((ps-to-p! sent-prep?) (resolve-time-s! ,coords _!)))
    ) constraint))
  constraints-binary)
) ; END resolve-binary-constraint


(defun resolve-frequency-constraint (constraints-freq)
; ``````````````````````````````````````````````````````
; Resolves each constraint in constraints-freq to a simple adjective, combining prepositions
; like (at.p (two.d (plur time.n))) to an adjective predicate like two-time.a.
;
  (mapcar (lambda (constraint) (if (prep-phrase? constraint)
    (freq-np-to-adj (second constraint)) constraint)) constraints-freq)
) ; END resolve-frequency-constraint


(defun plur-as-mod-a (np)
; `````````````````````````
; This removes any plur in a np and adds it as a mod-a to any temporal adjectives, unless it
; is redundant to do so, e.g. "the last few turns".
; TODO: this is somewhat flawed currently, because sentences like "the most recent turns" is
; intelligible (although a bit awkward), and appears to indicate multiple (most) recent turns.
; However, the most.mod-a is redundant, so this will simply drop the plur, giving "the most recent
; turn", which does NOT capture the same meaning. However, we leave these sorts of low-frequency
; expressions untouched for now.
;
  (ttt:apply-rules '(
    (/ (adj? (plur noun?)) ((plur.mod-a adj?) noun?))
    (/ (plur noun?) noun?)
  ) np)
) ; END plur-as-mod-a


(defun resolve-rel-np! (ulf scene)
; ``````````````````````````````````
; Turns a np ULF into a list of proper names to which that noun phrase can refer.
; TODO: deal with multiple postmods, deal with sets/conjunctions.
; TODO: deal with np's like "first block that I put on the Twitter block".
; TODO: this function is rather messy... I'm a bit unsure how to reorganize it, due to the complexity
; of some adjectives interacting with temporal postmodifiers, e.g. "the first block I moved". Some things
; should just be refactored though.
; TODO: this function assumes that np's are blocks, which needs to change for generality.
;
  (let* ((np-parts (split-np-modifiers ulf)) (premods (first np-parts))
         (postmods (second np-parts)) (noun (third np-parts)) coords-list times)  

    ; If we have a definite reference of a specific block, return the name of that block
    (when (nnp? ulf)
      (return-from resolve-rel-np! (list `(the.d (,ulf block.n)))))
    (when (ttt:match-expr '(det? (nnp? noun?)) ulf)
      (return-from resolve-rel-np! (list ulf)))

    ; Select initial names based on postmodifier. If temporal postmodifier, also find list of relevant
    ; times with attached propositions.
    (cond
      ; "the block on the Twitter block"
      ((ttt:match-expr '(prep? _!) (car postmods))
        (setq coords-list (filter-names scene (mapcar #'first
          (compute-relation scene nil nil (cons '(what.d block.n) (car postmods)) nil nil)))))
      ; "the block that I moved"
      ((ttt:match-expr '(^* (tense? action-verb?)) (car postmods))
        (setq times (find+constrain-times '(compute-move (what.d block.n) nil) '(recent.a) nil nil scene))
        (setq coords-list (filter-names scene (mapcan (lambda (time) (mapcar #'first (get time '@))) times))))
      ; unrecognized or no postmods
      (t (setq coords-list scene)))
    
    ; Filter by type
    (when noun
      (setq coords-list (remove-if-not (lambda (c) (equal (get-head-noun (car c)) noun)) coords-list)))
    
    ; Apply any premods
    (mapcar (lambda (premod)
        (cond
          ; Temporal premod
          ((find-car premod *temporal-adj-list*)
            ; Prune times which don't involve any of the objects in coords-list
            (mapcar (lambda (time) (setf (get time '@)
              (remove-if-not (lambda (prop) (member (car prop) (mapcar #'first coords-list))) (get time '@)))) times)
            (setq times (remove-if (lambda (time) (null (get time '@))) times))
            ; Apply temporal constraint and clear props on excluded times
            (let ((new-times (apply-unary-constraint premod times)))
              (mapcar (lambda (time) (setf (get time '@) nil)) (set-difference times new-times))
              (setq times new-times))
            ; Restrict coords-list
            (setq coords-list (filter-names coords-list (mapcan (lambda (time) (mapcar #'first (get time '@))) times))))
          ; Premod with most-n or some mod-a
          ((listp premod)
            (setq coords-list (eval-spatial-modifier (second premod) coords-list (first premod))))
          ; Simple adjective premod
          (t (setq coords-list (eval-spatial-modifier premod coords-list)))))
      premods)

    ; Return list of names of each item in coords-list
    (mapcar #'first coords-list))
) ; END resolve-rel-np!


(defun resolve-rel-nps! (ulf scene)
; ````````````````````````````````````
; Resolves all noun phrases in a relation.
;
  (mapcar (lambda (x) (if (np? x) (resolve-rel-np! x scene) x)) ulf)
) ; END resolve-rel-nps!


(defun resolve-time-np! (coords np)
; ```````````````````````````````````
; Resolves a temporal noun phrase to a corresponding time individual, or set
; of time individuals.
; NOTE: currently assumes that a np will have at most one unary constraint (i.e. adjective,
; possibly with modifier such as "last few"). If a temporal np can have multiple, this
; will need changing.
; TODO: this function is a bit messy and could use some refactoring.
;
  (let ((np1 (plur-as-mod-a np)); transform plur to mod-a, remove determiner
        det noun constraints-binary constraints-unary times)
    ; Save and remove det
    (setq det (first np1))
    (setq np1 (second np1))
    ; Map indexical det (i.e. 'this.d') to now.n
    (if (equal det 'this.d) (setq np1 'now.n))
    ; If n+preds, extract preds as binary constraints and resolve
    (when (n+preds? np1)
      (setq constraints-binary (resolve-binary-constraint coords (cddr np1)))
      (setq np1 (second np1)))
    ; Get head noun
    (setq noun (get-head-noun np1))
    ; Extract any unary constraints
    (when (ttt:match-expr '((! adj? (mod-a? adj?)) noun?) np1)
      (setq constraints-unary (list (resolve-unary-constraint (first np1)))))

    ; Get times corresponding to head noun
    (setq times (eval-temporal-noun noun))
    ; Constrain times using binary constraints, if any
    (mapcar (lambda (constraint)
      (setq times (remove-if-not (lambda (time)
        (apply-binary-constraint constraint time)) times))) constraints-binary)
    ; Apply unary constraints to get subset of times
    (mapcar (lambda (constraint)
      (setq times (apply-unary-constraint constraint times))) constraints-unary)
    ; Return set of times (or individual time if only one)
    (make-set times))
) ; END resolve-time-np!


(defun resolve-time-s! (coords s)
; `````````````````````````````````
; Resolves a sentence to the time at which the sentence was true, or set of times.
; NOTE: in theory this should support sentential prepositions which themselves have
; adverbials (e.g. "what block did I move before I last moved the Twitter block"),
; though this isn't supported on the parsing side yet.
;
  (make-set (mapcar (lambda (time) (setf (get time '@) nil) time)
    (find-answer-times coords s :embedded t)))
) ; END resolve-time-s!


(defun eval-prep-with-certainty (prep coords1 coords2 coords3 neg deg-adv)
; ``````````````````````````````````````````````````````````````````````````
; Evaluates some preposition with coords1 as the subject and coords2 (and coords3
; if between.p) as the object. Determine certainty, and return relations for which
; the certainty is above the threshold (or zero in the case of neg).
;
  (let ((certainty (eval-spatial-relation prep coords1 coords2 coords3 deg-adv)))
    ;; (format t "::(~a ~a ~a) -> ~a~%" prep coords1 coords2 certainty) ; DEBUGGING
    (if neg
      ; If neg, add negated tuple + certainty for all with zero certainty
      (if (and (numberp certainty) (<= certainty 0))
        (if coords3
          `((,(car coords1) not ,prep (set-of ,(car coords2) ,(car coords3))) ,(- 1.0 certainty))
          `((,(car coords1) not ,prep ,(car coords2)) ,(- 1.0 certainty))))
      ; Otherwise, if certainty is greater than threshold, add tuple + certainty to pred-list
      (if (and (numberp certainty) (> certainty *certainty-threshold*))
        (if coords3
          `((,(car coords1) ,prep (set-of ,(car coords2) ,(car coords3))) ,certainty)
          `((,(car coords1) ,prep ,(car coords2)) ,certainty)))))
) ; END eval-prep-with-certainty


(defun form-pred-list (coords-list1 prep-list coords-list2 coords-list3 &key neg deg-adv)
; `````````````````````````````````````````````````````````````````````````````````````````
; Form predicates from all relations that are satisfied having things from coords-list1
; as the subject, a preposition from prep-list, and coords-list2 as the object.
; coords-list3 is only used in the case of a between.p predicate, nil otherwise.
; If neg is given as t, return negated predicates.
; NOTE: preds are returned in decreasing order of certainty.
; TODO: this is kind of messy, could use some cleaning.
;
  (let ((pred-list (remove nil
        ; Check all combination of blocks between coords-list1 and coords-list2
        (mapcan (lambda (coords1) (mapcan (lambda (coords2)
          (if (not (equal (car coords1) (car coords2)))
            ; Evaluate all prepositions in prep-list
            (mapcan (lambda (prep)
              ; If ternary pred, e.g. BETWEEN.P, also need to check all possible third blocks
              (if (ternary-spatial-prep-p prep)
                (mapcar (lambda (coords3)
                    (eval-prep-with-certainty prep coords1 coords2 coords3 neg deg-adv))
                  coords-list3)
                (list (eval-prep-with-certainty prep coords1 coords2 nil neg deg-adv))))
            prep-list)))
          coords-list2)) coords-list1))))
    ; Sort by certainty and remove certainties
    (mapcar #'first (sort (copy-seq pred-list) #'> :key #'second)))
) ; END form-pred-list


(defun compute-relations (scene moves scene1 subj)
; ``````````````````````````````````````````````````
; Computes all spatial relations that hold at a particular scene, for a particular subject.
; NOTE: we assume uniqueness of coords in the scene, or else this will break.
; NOTE: we ensure that the top-2 relations returned do not have duplicate objects.
; TODO: "where wasn't the Twitter block?"
;
  ; Find all possible pairs of subject + object in the scene, and check if relation holds
  (let ((relations (form-pred-list scene *spatial-prep-list* scene scene)))
    ; Resolve subject to specific names
    (setq subj (resolve-rel-np! subj scene))
    ; Filter relations and return the top-2 relations with unique objects
    (last (remove-duplicates (reverse (find-cars-list subj relations)) :key #'third) 2))
) ; END compute-relations


(defun compute-move-relations (scene moves scene1 obj)
; ```````````````````````````````````````````````````````
; Computes all spatial relations that hold at a particular scene immediately following a move of
; a particular object, if such a move did occur.
; TODO: "where did I not move the Twitter block?"
;
  ; Resolve object to specific names
  (setq obj (resolve-rel-np! obj scene))
  ; Only find relations in the case where the object(s) were moved
  (when (find-cars-list obj moves)
    ; Find relations in the scene following the move with the block(s) in question as subject(s),
    ; and return the top-2 relations with unique objects
    (let ((relations (form-pred-list scene1 *spatial-prep-list* scene1 scene1)))
      (last (remove-duplicates (reverse (find-cars-list obj relations)) :key #'third) 2)))
) ; END compute-move-relations


(defun compute-move+relation (scene moves scene1 rel obj neg deg-adv)
; ````````````````````````````````````````````````````````````````````
; Computes all moves of a block into a particular relation, i.e. whether the
; given object was moved, and that the relation holds in the resulting scene.
; If neg is given, either the object was not moved, or the relation didn't hold in the scene.
;
  (let ((relations (compute-relation scene moves scene1 rel nil deg-adv)))
    ; Resolve object to specific names
    (setq obj (resolve-rel-np! obj scene))
    ; Get rid of any moves for which the relation does not hold in scene1
    (setq moves (remove-if-not (lambda (move) (find move relations
                 :test (lambda (x y) (equal (car x) (car y))))) moves))
    ; If negation, find complement of moves
    (if neg (setq moves (negate-moves moves)))
    ; Simplify form of relations and filter based on obj
    (filter+process-moves moves obj))
) ; END compute-move+relation


(defun compute-predicative (scene moves scene1 subj obj)
; ````````````````````````````````````````````````````````
; Computes a predicative relation at a particular scene, i.e. gives a list of all names
; that obj can refer to which are also in the list of names that subj can refer to.
;
  ; Resolve subject and object to specific names
  (setq subj (resolve-rel-np! subj scene))
  (setq obj (resolve-rel-np! obj scene))
  (mapcar #'list (remove-if-not (lambda (name) (member name subj)) obj))
) ; END compute-predicative


(defun compute-modifier (scene moves scene1 rel neg)
; ````````````````````````````````````````````````````
; Computes a unary relation/modifier at a particular scene.
;
  (let* ((subj (resolve-rel-np! (first rel) scene)) (adj (second rel))
         (coords-list (find-cars-list subj scene)))
    ; Reduce coords-list to the ones for which the modifier holds (or the opposite if neg)
    (if neg
      (setq coords-list (set-difference coords-list (eval-spatial-modifier adj coords-list) :test #'equal))
      (setq coords-list (eval-spatial-modifier adj coords-list)))
    ; Turn each coord into a unary relation
    (mapcar (lambda (coord) (list (car coord) adj)) coords-list))
) ; END compute-modifier


(defun compute-relation (scene moves scene1 rel neg deg-adv)
; ````````````````````````````````````````````````````````````
; Computes a relation at a particular scene.
;
  (let* ((subj (resolve-rel-np! (first rel) scene)) (prep (second rel))
         (obj (resolve-rel-np! (third rel) scene)) (obj2 (resolve-rel-np! (fourth rel) scene))
         (coords-list1 (find-cars-list subj scene)) (coords-list2 (find-cars-list obj scene))
         (coords-list3 (find-cars-list obj2 scene)) relation-list)
    ; Find all relations that hold (removing duplicates if counting)
    (setq relation-list (form-pred-list
      coords-list1 (list prep) coords-list2 coords-list3 :neg neg :deg-adv deg-adv))
    (if (quan? (first rel))
      (setq relation-list (remove-duplicates relation-list
        :test (lambda (r1 r2) (equal (first r1) (first r2))))))
    (if (quan? (third rel))
      (setq relation-list (remove-duplicates relation-list
        :test (lambda (r1 r2) (equal (car (last r1)) (car (last r2)))))))
  relation-list)
) ; END compute-relation


(defun compute-move (scene moves scene1 obj neg)
; ````````````````````````````````````````````````
; Computes all moves at a particular time with the given object.
;
  ; Resolve subject to specific names
  (setq obj (resolve-rel-np! obj scene))
  ; If negation, we need to access context to see all blocks (or more generally, 'movable entities'),
  ; and remove all of the blocks which moved during Ti.
  (if neg (setq moves (negate-moves moves)))
  ; Simplify form of relations and filter based on obj
  (filter+process-moves moves obj)
) ; END compute-move


(defun filter+process-moves (moves obj)
; ``````````````````````````````````````
; Filters all moves to ones involving the given object, and processes to form ((the.d (|Texaco| block.n)) (past move.v))
;
  (mapcar (lambda (move) `(,(car move) (past move.v))) (find-cars-list obj moves))
) ; END filter+process-moves


(defun negate-moves (moves)
; ``````````````````````````
; Given a list of block moves, return the complement of that list by accessing context.
; TODO: this is a bad approach because (a) it's not general, and (b) it relies on "internal"
; manipulation of context outside of the schemas. I'm not immediately sure how to generalize
; it, though.
;
  (set-difference (get-from-context 'block.n) moves
    :test (lambda (x y) (equal (car x) (car y))))
) ; END negate-moves


(defun apply-unary-constraint (constraint times)
; `````````````````````````````````````````````````
; Applies a unary constraint (e.g. RECENT.A) to list of times.
;
  (let ((constraint-eval (ttt:apply-rule `(/ (! adj? (mod-a? adj?))
          (eval-temporal-modifier 'adj? ',times '(ensure-bound! mod-a?))) constraint :shallow t)))
    (eval constraint-eval))
) ; END apply-unary-constraint


(defun apply-binary-constraint (constraint time)
; ```````````````````````````````````````````````
; Applies a binary constraint (e.g. (BEFORE.P NOW2)) to time and returns t or nil.
;
  (let ((constraint-eval (ttt:apply-rule `(/ (! (prep? _!) (mod-a? (prep? _!)))
          (eval-temporal-relation 'prep? ',time '_! '(ensure-bound! mod-a?))) constraint :shallow t)))
    (eval constraint-eval))
) ; END apply-binary-constraint


(defun apply-frequency-constraint (constraint times)
; ````````````````````````````````````````````````````
; Applies a frequency constraint (e.g. ALWAYS.A) to a list of times.
;
  (let ((constraint-eval (ttt:apply-rule `(/ (! adj? (mod-a? adj?))
          (eval-frequency-modifier 'adj? ',times '(ensure-bound! mod-a?))) constraint :shallow t)))
    (eval constraint-eval))
) ; END apply-frequency-constraint


(defun subst-move-scene (move scene)
; ````````````````````````````````````
; Substitutes the from.p coordinates into the to.p coordinates of a move in a given scene,
; effectively undoing the move.
;
  (subst (cons (first move) (second move)) (cons (first move) (third move)) scene :test #'~equal)
) ; END subst-move-scene


(defun add-certainty (rel time)
; ```````````````````````````````
; Adds certainty to a relation based on time.
; NOTE: currently always set to 1.0 - see note on 'apply-to-times' func.
;
  `((that ,rel) certain-to-degree 1.0)
) ; END add-certainty


(defun add-certainty-list (rels time)
; ````````````````````````````````````
; Adds certainties to a list of relations.
; NOTE: currently always set to 1.0 - see note on 'apply-to-times' func.
;
  (mapcar (lambda (rel) (add-certainty rel time)) rels)
) ; END add-certainty-list


(defun not-place.n? (x)
  (not (equal x 'place.n)))