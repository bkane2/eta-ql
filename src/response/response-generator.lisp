;; Feb 4/2020
;; ================================================
;;
;; This code is used to generate a response to a spatial query,
;; given a list of propositional answer relations with certainties.
;;
;; The general process is to categorize each query into one of a small
;; set of general question categories, such as "confirm" (yes/no question),
;; "ident" (what question), "descr" (where question), etc. This category is
;; used to form an appropriate answer ULF fragment (such as simple yes/no for
;; "confirm", a noun phrase for "ident", a set of prepositional phrases for "descr",
;; etc.). This answer ULF is then substituted in for the appropriate constituent in
;; the query ULF, such as what.pro/(what.d block.n) in the case of "ident".
;;
;; After this substitution, the modified query ULF is uninverted by applying all
;; sub operators and removing redundant auxiliaries to form the response ULF.
;; Certain pragmatic modifications are made at this point, chiefly checking for
;; and responding to presupposition failure using the ulf-pragmatics library.
;;

(in-package :eta)


(defun generate-response (query-ulf relations)
; `````````````````````````````````````````````
; Generates a natural language response, given a query ULF and a list of satisfying relations with
; certainties (of form ((that ?rel) certain-to-degree ?cert)).
; In the case where relations is nil, we rely on presupposition handling to generate the appropriate response.
;
  (let ((query-type (get-query-type query-ulf)) ans-tuple ans-ulf output-ulf uncertain-flag poss-ans response)

    ; Simplify format of relations to (?rel ?cert) pairs
    (setq relations (mapcar (lambda (relation) (list (second (first relation)) (third relation))) relations))

    ; Check if poss-ques, if so set flag to true (to add a hedge later on) and set ulf to remainder
    (when (poss-ques? query-ulf)
      (setq poss-ans t)
      (setq query-ulf (second query-ulf)))

    ; ans-tuple consists of a list (ans-ulf uncertain-flag), where ans-ulf is a ULF corresponding
    ; to the relevant answer, and uncertain-flag indicates whether the answer is above or below the
    ; certainty threshold.
    (setq ans-tuple (form-ans query-type relations))
    (setq ans-ulf (first ans-tuple))
    (setq uncertain-flag (second ans-tuple))

    ;; (format t "Query: ~a~%" query-ulf)
    (format t "Query type: ~a~%" query-type)
    ;; (format t "Relations: ~a~%" relations)
    (format t "Answer: ~a~%" ans-ulf) ; DEBUGGING

    ; Make appropriate substitutions of answer ULF into query ULF,
    ; and uninvert form of question to get output ULF.
    (setq output-ulf (uninvert-question (cond
      ; Query is CONFIRM type
      ((equal query-type 'CONFIRM)
        ans-ulf)
      ; Query is ATTR-COLOR type
      ((equal query-type 'ATTR-COLOR)
        (ttt:apply-rule `(/ color-flag? ,ans-ulf) query-ulf))
      ; Query is COLOR-OBJECT type
      ((equal query-type 'COLOR-OBJECT)
        (ttt:apply-rule `(/ color-object-flag? ,ans-ulf) query-ulf))
      ; Query is COUNT type
      ((equal query-type 'COUNT)
        (ttt:apply-rule `(/ count-flag? ,ans-ulf) query-ulf))
      ; Query is EXIST type
      ((equal query-type 'EXIST)
        (if ans-ulf
          (ttt:apply-rule `(/ exist-flag? ,ans-ulf) query-ulf)
          '(NO.YN)))
      ; Query is IDENT-PREP type
      ((equal query-type 'IDENT-PREP)
        ; although ident-prep was used to select the appropriate argument,
        ; we can still use the normal ident flag for answer substitution
        (ttt:apply-rule `(/ ident-flag? ,ans-ulf) query-ulf))
      ; Query is IDENT type
      ((equal query-type 'IDENT)
        (ttt:apply-rule `(/ ident-flag? ,ans-ulf) query-ulf))
      ; Query is DESCR type
      ((equal query-type 'DESCR)
        (ttt:apply-rule `(/ descr-flag? ,ans-ulf) query-ulf))
      ; Query is TIME type
      ((equal query-type 'TIME)
        (ttt:apply-rule `(/ time-flag? ,ans-ulf) query-ulf))
      ; Query is EXPLANATION type
      ((equal query-type 'EXPLANATION)
        (ttt:apply-rule `(/ explanation-flag? ,ans-ulf) query-ulf)))))

    ; When answer is uncertain, append an adverb indicating uncertainty to answer
    (when uncertain-flag
      (setq output-ulf (list 'possibly.adv-s output-ulf)))

    ; When no answer, check if a presupposition failure has occurred, if so, output the
    ; negation of that presupposition
    (if (null relations)
      (let ((presupposition-failure (respond-to-presupposition query-ulf)))
        (format t "presupposition failure response: ~a~%" presupposition-failure) ; DEBUGGING
        (if presupposition-failure (setq output-ulf presupposition-failure))))

    ; Unless a TIME type question, remove all adv-e from output
    (when (not (equal query-type 'TIME))
      (setq output-ulf (remove-adv-e output-ulf)))
    
    (format t "output ULF: ~a~%" output-ulf) ; DEBUGGING

    ; Convert output ULF to an english string and output (or output an error if the output ULF is nil)
    (setq response (if output-ulf
      (append (if poss-ans '(I\'m not sure that I understood the question correctly \, but))
        (ulf-to-english (normalize-output output-ulf)))
      '(Sorry \, I was unable to find an object that satisfies given constraints \, please rephrase in a simpler way \.)))

    (list response output-ulf))
) ; END generate-response


(defun generate-proposal (ka)
; ```````````````````````````````
; Generates a gist clause for an action proposal (possibly further transduced into a surface expression),
; given a reified action (e.g., (ka (put.v |B1| (on.p |B2|))), or (ka (move.v |B1| (back.mod-a (on.p |B2|))))).
;
  (let (ulf output-ulf)
    (setq ulf '((the.d (next.a step.n)) ((pres be.v) (= what.pro))))
    (setq output-ulf (ttt:apply-rule `(/ what.pro ,ka) ulf))
    (setq output-ulf (ttt:apply-rule '(/ BW-concept? (BW-concept-to-common-name! BW-concept?)) output-ulf))
    (ulf-to-english output-ulf)
)) ; END generate-proposal


(defun get-query-type (ulf)
; ```````````````````````````
; Classify a ULF query as one of a few types by checking corresponding TTT flags.
; NOTE By default, we might want to treat queries as CONFIRM - the above flags have difficulties matching to
; queries which are given in declarative form, or parsed in such a way (for example, the parse of
; "has the Twitter block ever touched the Mercedez block?" removes the "has" and adds a perfect aspect
; to touch.v, so the output ULF resembles a question in declarative form).
;
  (setq ulf (apply-sub-macro ulf))
  (cond
    ((ttt:match-expr '(^* color-flag?) ulf) 'ATTR-COLOR)
    ((ttt:match-expr '(^* color-object-flag?) ulf) 'COLOR-OBJECT)
    ((ttt:match-expr '(^* descr-flag?) ulf) 'DESCR)
    ((ttt:match-expr '(^* time-flag?)  ulf) 'TIME)
    ((ttt:match-expr '(^* ident-prep-flag?) ulf) 'IDENT-PREP)
    ((ttt:match-expr '(^* ident-flag?) ulf) 'IDENT)
    ((ttt:match-expr '(^* count-flag?) ulf) 'COUNT)
    ((ttt:match-expr '(^* exist-flag?) ulf) 'EXIST)
    ((ttt:match-expr '(^* explanation-flag?) ulf) 'EXPLANATION)
    (t 'CONFIRM))
) ; END get-query-type


(defun form-ans (query-type relations)
; `````````````````````````````````````````
; Get an answer (i.e. a ULF object reflecting the answer) depending on the broad type of query
; and the list of relations given.
;
  (let ((certain (certain-rels relations)) (uncertain (uncertain-rels relations))
        ans-set uncertain-flag ans)

    ; If any certain relations are known, discard the uncertain ones and construct the answer ULF from those.
    ; Otherwise, construct the answer ULF from the uncertain relations and add an uncertainty indicator at the end.
    (setq ans-set certain)
    (when (null ans-set)
      (setq ans-set uncertain)
      (if ans-set (setq uncertain-flag t)))

    ; Refine answer depending on what general category of question was asked.
    (setq ans (cond
      ; Query is CONFIRM type
      ((equal query-type 'CONFIRM)
        (if ans-set '(YES.YN) '(NO.YN)))
      ; Query is ATTR-COLOR type
      ((equal query-type 'ATTR-COLOR)
        (form-ans-color ans-set))
      ; Query is COLOR-OBJECT type
      ((equal query-type 'COLOR-OBJECT)
        (form-ans-color-object ans-set))
      ; Query is COUNT type
      ((equal query-type 'COUNT)
        (form-ans-count ans-set))
      ; Query is EXIST type
      ((equal query-type 'EXIST)
        (form-ans-subj ans-set))
      ; Query is IDENT-PREP type
      ((equal query-type 'IDENT-PREP)
        (form-ans-obj ans-set))
      ; Query is IDENT type
      ((equal query-type 'IDENT)
        (form-ans-subj ans-set))
      ; Query is DESCR type
      ((equal query-type 'DESCR)
        (form-ans-descr ans-set))
      ; Query is TIME type
      ((equal query-type 'TIME)
        (form-ans-time ans-set))
      ; Query is EXPLANATION type
      ((equal query-type 'EXPLANATION)
        (form-ans-explanation ans-set))
      ; Other
      (t
        '(Sorry \, I was unable to find an object that satisfies given constraints \, please rephrase in a simpler way \.))))

    (list ans uncertain-flag))
) ; END form-ans


(defun ulf-to-english (ulf)
; ``````````````````````````
; For converting a ULF response to a surface form response via the ulf2english library.
;
  ;; (format t "converting to english: ~a~%" ulf) ; DEBUGGING
  (str-to-output (ulf2english:ulf2english ulf :add-commas t))
) ; END ulf-to-english


(defun respond-to-presupposition (ulf)
; ``````````````````````````````````````
; Generates presupposition for query ULF (if any), and creates a response to that
; presupposition by negating it.
;
  (let ((presupposition nil))
    (setq presupposition (ulf-pragmatics:get-wh-question-presupposition ulf :calling-package *package*))
    (format t "presupposition: ~a~%" presupposition) ; DEBUGGING
    (negate-wh-question-presupposition (normalize-wh-question-presupposition (remove-adv-e presupposition))))
) ; END respond-to-presupposition


(defun normalize-output (ulf)
; `````````````````````````````
; Apply any final normalization rules to fix output.
;
  (ttt:apply-rules
    '((/ ((nquan one.a) (_! (plur _!1))) ((nquan one.a) (_! _!1)))
      (/ ((nquan one.a) (plur _!1)) ((nquan one.a) _!1))
      (/ (a.d (plur block.n)) (a.d block.n))
      (/ (past perf) (pres perf))
      ; not not on => on
      (/ (_*1 not not _*2) (_*1 _*2))
      ; do I not => I do not
      (/ ((tense? do.aux-s) indiv? (not (verb-untensed? _*))) (indiv? ((tense? do.aux-s) not (verb-untensed? _*))))
      ; not have => have not
      (/ (not ((tense? aspect?) (verb? _*))) ((tense? aspect?) not (verb? _*)))
      ; pron verb that => that pron verb
      (/ (pron? ((tense? verb?) relative?)) (sub relative? (pron? ((tense? verb?) *h))))
      ; fix adv-e with sentential preposition
      (/ (adv-e (sent-prep? _!)) (sent-prep? _!))
      ; remove redundant 'relative to/with respect to' adjuncts in where questions
      (/ (_!1 ((tense? verb?) (prep? _!2) (adv-a ((! relative_to.p with_respect_to.p) _!3))))
         (_!1 ((tense? verb?) (prep? _!2))))
      ; swap 'should'/'need' position in ULF (should this be in 'uninvert-question'?)
      (/ ((tense? (! should.aux-s need.aux-v)) _! (verb? _*)) (_! ((tense? !) (verb? _*))))
      ; TODO: temporary fix for "toppest"
      (/ (most-n top.a _*) (topmost.a _*))
      )
  ulf)
) ; END normalize-output


(defun normalize-wh-question-presupposition (ulf)
; `````````````````````````````````````````````````````````````
; Extracts result formula and applies task-specific normalization, including
; (for now) fixing bugs, such as the presupposition having "something.d block.n"
; instead of "some.d block.n".
;
  (ttt:apply-rules
    '((/ (something.d _!) (some.d _!))
      ;; (/ (some.d (adj? noun?)) (some.d noun?))
      (/ (nquan (somehow.mod-a many.a)) some.d)
      (/ (I.pro ((past do.aux-s) (verb-untensed? _! (adv-e (! (^* (some.d _!1)))))))
         (I.pro ((past verb-untensed?) _!)))
      (/ (I.pro ((past verb-untensed?) _! (adv-e (! (^* (some.d _!1))))))
         (I.pro ((past verb-untensed?) _!)))
      (/ (ans-to (sub _! ((past do.aux-s) pron? (verb-untensed? _*))))
         (ans-to (sub _! (pron? ((past verb-untensed?) _*)))))
    )
    ulf)
) ; END normalize-wh-question-presupposition


(defun negate-wh-question-presupposition (ulf)
; `````````````````````````````````````````````
; Negates a presupposition by changing "some"/"something" to "no"/"nothing" (in
; the case of negations, we want to remove the double negative), or otherwise adding
; a negation to the ULF.
; NOTE: It seems like, in the future, this function could be made more robust using
; natural logic like inference rules, i.e. making use of NPI/PPI to get the
; "rewritings" correctly.
;
  (cond
    ((ttt:match-expr '(^* (! some.d something.pro)) ulf)
      (ttt:apply-rules '(
          ; nothing does not touch the Twitter block => everything touches the Twitter block
          (/ (nothing.pro ((? (tense? do.aux-s)) not (verb-untensed? _*))) (everything.pro ((tense? verb-untensed?) _*)))
          (/ ((no.d _!) ((? (tense? do.aux-s)) not (verb-untensed? _*))) ((every.d _!) ((tense? verb-untensed?) _*)))
          ; nothing is not to the left of the Twitter block => everything is to the left of the Twitter block
          (/ (nothing.pro ((tense? verb-untensed?) not _*)) (everything.pro ((tense? verb-untensed?) _*)))
          (/ ((no.d _!) ((tense? verb-untensed?) not _*)) ((every.d _!) ((tense? verb-untensed?) _*)))
          ; touches nothing => does touch nothing
          (/ ((tense? verb-untensed?) nothing.pro _*) ((tense? do.aux-s) (verb-untensed? nothing.pro _*)))
          (/ ((tense? verb-untensed?) (no.d _!) _*) ((tense? do.aux-s) (verb-untensed? (no.d _!) _*)))
          ; have moved nothing => have not moved anything
          (/ ((tense? aspect?) (verb-untensed? nothing.pro _*)) ((tense? aspect?) not (verb-untensed? anything.pro _*)))
          (/ ((tense? aspect?) (verb-untensed? (no.d _!) _*)) ((tense? aspect?) not (verb-untensed? (any.d _!) _*)))
          ; does touch nothing => does not touch anything
          (/ ((tense? do.aux-s) (^* (verb-untensed? nothing.pro _*))) ((tense? do.aux-s) not (verb-untensed? anything.pro _*)))
          (/ ((tense? do.aux-s) (^* (verb-untensed? (no.d _!) _*))) ((tense? do.aux-s) not (verb-untensed? (any.d _!) _*)))
          ; do I move nothing => I do not move anything
          (/ ((tense? do.aux-s) _!1 (^* (verb-untensed? nothing.pro _*))) (_!1 ((tense? do.aux-s) not (verb-untensed? anything.pro _*))))
          (/ ((tense? do.aux-s) _!1 (^* (verb-untensed? (no.d _!) _*))) (_!1 ((tense? do.aux-s) not (verb-untensed? (any.d _!) _*))))
          ; every blocks => all blocks
          (/ (every.d (! (^* (plur noun?)))) (all.d !))
          ; never did not move anything => move everything
          (/ (never.adv-f ((tense? do.aux-s) not (verb-untensed? anything.pro _*))) ((tense? verb-untensed?) everything.pro _*))
          (/ (never.adv-f ((tense? do.aux-s) not (verb-untensed? (any.d _!) _*))) ((tense? verb-untensed?) (every.d _!) _*))
          )
      ; something => nothing, some block => no block
      (ttt:apply-rules '((/ something.pro nothing.pro) (/ (some.d _!) (no.d _!))) ulf :max-n 1)))
    (t
      (ttt:apply-rules '(
          ; was moved => wasn't moved
          (/ (_! ((! tense? (tense? aspect?)) (pasv verb-untensed?))) (_! not (! (pasv verb-untensed?))))
          ; be not on => be always on
          (/ (_! ((tense? be.v) not _*))           ; NOTE: this is a little strange since 'always' is treated as adv-e
             (_! ((tense? be.v) always.adv-s _*))) ; elsewhere, but is adv-s here so it's not removed during output                    
          ; be on => be not on
          (/ (_! ((tense? be.v) _*)) (_! ((tense? be.v) not _*)))
          ; touch the Twitter block => do not touch the Twitter block
          (/ (_! ((tense? verb-untensed?) _*)) (_! ((tense? do.aux-s) not (verb-untensed? _*)))))
      ulf :max-n 1 :shallow t)))
) ; END negate-wh-question-presupposition


(defun form-ans-obj (relations)
; ``````````````````````````````````
; Retrieves objects from a list of relations
;
  (make-set (remove-duplicates (remove nil
    (mapcar #'third relations)) :test #'equal))
) ; END form-ans-obj


(defun form-ans-subj (relations)
; ``````````````````````````````````
; Retrieves subjects from a list of relations
;
  (make-set (remove-duplicates (remove nil
    (mapcar #'first relations)) :test #'equal))
) ; END form-ans-subj


(defun form-ans-count (set)
; ```````````````````````````
; Creates ULF determiner corresponding to the size of the answer set.
;
  (num-to-adj (length set))
) ; END form-ans-count


(defun form-ans-color (relations)
; ````````````````````````````````
; Retrieves colors from a list of relations.
;
  (make-set (remove-duplicates (remove nil
    (mapcar (lambda (rel)
      (get-color (first rel))) relations)) :test #'equal))
) ; END form-ans-color


(defun form-ans-color-object (relations)
; ````````````````````````````````````````
; Retrieves color noun phrases from a list of relations.
;
  (make-set (remove-duplicates (remove nil
    (mapcar (lambda (rel)
      (make-color-np (first rel))) relations)) :test #'equal))
) ; END form-ans-color


(defun form-ans-descr (relations)
; ```````````````````````````````````
; Creates a ULF conjunction of relations describing where the subject is.
;
; e.g. (form-ans-descr '(((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n)))
;                        ((the.d (|Twitter| block.n)) on.p (the.d (|McDonald's| block.n)))
;                        ((the.d (|Twitter| block.n)) between.p (the.d (|Target| block.n)) (the.d (|Mercedes| block.n)))
;                        ((the.d (|Twitter| block.n)) between.p (the.d (|Starbucks| block.n)) (the.d (|Toyota| block.n)))))
;       => (set-of (between.p (set-of (the.d (|Target| block.n)) (the.d (|Mercedes| block.n))))
;                  (between.p (set-of (the.d (|Starbucks| block.n)) (the.d (|Toyota| block.n))))
;                  (on.p (the.d ((set-of |McDonald's| |Texaco|) (plur block.n)))))
;
  (make-set (mapcar #'cdr (mapcar #'condense-by-objs (group-relations relations))))
) ; END form-ans-descr


(defun form-ans-time (relations)
; ````````````````````````````````
; Creates a ULF from a list of times.
; TODO: this isn't using the right ULF for 'ago.p' due to issue with not omitting
; curly brackets in output. Should be fixed at some point.
;
  (make-set (remove-duplicates (mapcar (lambda (rel)
      (cond
        ((atom rel)
          `(rep (*p ,(get-elapsed-time (get-time-of-episode rel))) ago.p))
        (t rel)))
    relations) :test #'equal))
) ; END form-ans-time


(defun form-ans-explanation (relations)
; ``````````````````````````````````````
; Creates a ULF for an explanation query from a list of factor relations, by converting
; relations to copular expressions and adding 'also.cc' conjunctions (if necessary).
;
; e.g. (form-ans-explanation '(((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n)))
;                              ((the.d (|Twitter| block.n)) on.p (the.d (|McDonald's| block.n))) ))
;       => (because.ps (((the.d (|Twitter| block.n)) ((pres be.v) (on.p (the.d (|Texaco| block.n))))) also.cc
;                       ((the.d (|Twitter| block.n)) ((pres be.v) (on.p (the.d (|McDonald's| block.n)))))))
;
; e.g. (form-ans-explanation '(((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n)))))
;      => (because.ps ((the.d (|Twitter| block.n)) ((pres be.v) (on.p (the.d (|Texaco| block.n))))))
;
  ; NOTE: uses 'also' connective so that we can later cleanly split off factors
  ;       when generating a response from the gist clause.
  (list 'because.ps (make-conjunction (mapcar #'prop-to-cop relations) :conj 'ALSO.CC))
  ;; (list 'because.ps (make-set relations))
) ; END form-ans-explanation


(defun compare-certainty (relation)
; ```````````````````````````````````
; Compares certainty of relation to the global threshold parameter.
;
  (> (second relation) *certainty-threshold*)
) ; END compare-certainty


(defun certain-rels (relations)
; ````````````````````````````````````
; Retrieves relations from a list of relations with certainties above the threshold.
;
  (remove nil (mapcar (lambda (rel)
    (if (compare-certainty rel) (car rel))) relations))
) ; END certain-rels


(defun uncertain-rels (relations)
; ````````````````````````````````````
; Retrieves relations from a list of relations with certainties below the threshold.
;
  (remove nil (mapcar (lambda (rel)
    (if (not (compare-certainty rel)) (car rel))) relations))
) ; END uncertain-rels


(defun group-relations (relations)
; `````````````````````````````````
; Groups relations into sublists according to predicate using a hash table.
;
; e.g. (group-relations '(((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n)))
;                         ((the.d (|Twitter| block.n)) on.p (the.d (|McDonald's| block.n)))
;                         ((the.d (|Twitter| block.n)) near.p (the.d (|Target| block.n)))
;                         ((the.d (|Twitter| block.n)) to_the_left_of.p (the.d (|Starbucks| block.n)))))
;       => ((((the.d (|Twitter| block.n)) to_the_left_of.p (the.d (|Starbucks| block.n))))
;           (((the.d (|Twitter| block.n)) near.p (the.d (|Target| block.n))))
;           (((the.d (|Twitter| block.n)) on.p (the.d (|McDonald's| block.n)))
;            ((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n)))))
;
  (let ((relation-ht (make-hash-table :test #'equalp)) result)
    ; Store each relation in hash table under corresponding pred
    (dolist (rel relations)
      (if (gethash (second rel) relation-ht)
        (setf (gethash (second rel) relation-ht) (cons rel (gethash (second rel) relation-ht)))
        (setf (gethash (second rel) relation-ht) (list rel))))
    ; Map over each list of props in hash table under each unique pred and cons to result list
    ; NOTE: if pred is ternary, e.g. BETWEEN.P, group each relation separately
    (maphash (lambda (pred props) 
      (if (ternary-spatial-prep-p pred)
        (mapcar (lambda (rel) (setq result (cons (list rel) result))) props)
        (setq result (cons props result))))
      relation-ht)
  result)
) ; END group-relations


(defun condense-by-objs (relations)
; ```````````````````````````````````
; Given a list of relations (assumed to have the same subject and predicate), condense relations into a
; single relation with a (set-of ...) object.
; NOTE: we make an exception for "between" (or any other predicates with more than one object).
;
; e.g. (condense-by-objs '(((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n)))
;                          ((the.d (|Twitter| block.n)) on.p (the.d (|Starbucks| block.n)))))
;       => (|Twitter| on.p (the.d ((set-of |Texaco| |Starbucks|) (plur block.n))))
; e.g. (condense-by-objs '(((the.d (|Twitter| block.n)) between.p (the.d (|Texaco| block.n)) (the.d (|Starbucks| block.n)))))
;       => (|Twitter| between.p (set-of (the.d (|Texaco| block.n)) (the.d (|Starbucks| block.n))))
;
  (let ((pred (second (car relations)))
        (subj (first (car relations)))
        (objs (make-plur-np-obj relations)))
    (if (ternary-spatial-prep-p pred)
      ; If ternary predicate, e.g. BETWEEN.P, leave uncondensed but turn objects into sets
      (car (mapcar #'collapse-ternary-relation relations))
      ; Otherwise, return new condensed 
      `(,subj ,pred ,objs)))
) ; END condense-by-objs


(defun collapse-ternary-relation (relation)
; ```````````````````````````````````````````
; Given a ternary relation, collapse the objects to a set.
; e.g. (collapse-ternary-relation
;       '((the.d (|Twitter| block.n)) between.p (the.d (|Texaco| block.n)) (the.d (|Starbucks| block.n))))
;       => ((the.d (|Twitter| block.n)) between.p (set-of
;             (the.d ((the.d (|Texaco| block.n)) block.n))
;             (the.d ((the.d (|Starbucks| block.n)) block.n))))
;
  (if (ternary-spatial-prep-p (second relation))
    `(,(first relation) ,(second relation) (set-of ,(third relation) ,(fourth relation)))
    relation)
) ; END collapse-ternary-relation


(defun make-plur-np-obj (relations)
; ```````````````````````````````````
; Makes plural np with conjoined object names as modifier.
; TODO: currently assumes that all objects are individuals of the
; same type with nnp modifiers; this should be made more general in the future.
;
; e.g. (make-plur-np-obj '(((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n)))
;                          ((the.d (|Twitter| block.n)) on.p (the.d (|Starbucks| block.n)))))
;       => (the.d ((set-of |Texaco| |Starbucks|) (plur block.n)))
;
  (let ((nnps (make-set (remove-duplicates (remove nil (mapcar (lambda (rel) (caadr (third rel))) relations)))))
        (type (get-head-noun (third (car relations)))))
    `(the.d (,nnps ,(if (listp nnps) (list 'plur type) type))))
) ; END make-plur-np-obj


(defun make-color-np (np)
; ``````````````````````````
; Forms an indefinite noun phrase from a color adjective and type.
; e.g. (make-color-np '(the.d (|Twitter| block.n))) => (a.d (red.a block.n))
;
  (let ((color (get-color np)) (type (get-head-noun np)))
    `(a.d (,color ,type)))
) ; END make-color-np


(defun get-color (np)
; `````````````````````
; Gets the color of a given name.
; e.g. (get-color '(the.d (|Twitter| block.n))) => red.a
;
  (find-if (lambda (pred) (get-from-context (list np pred))) *color-adj-list*)
) ; END get-color


; TTT flags and other preds are defined as follows
; ``````````````````````````````````````````````````
(defun yn-flag? (p)
  (ttt:match-expr '(! yn-word? (tense? yn-word?) ((tense? aspect?) yn-word?) ((tense? aspect?) (yn-word? _*))
                      ((tense? aspect?) (pasv yn-word?)) ((tense? aspect?) ((pasv yn-word?) _*))) p))

(defun count-flag? (p)
  (ttt:match-expr '(! (HOW.MOD-A MANY.A)) p))

(defun ident-prep-flag? (p)
  (ttt:match-expr '(prep? (! WHAT.PRO WHICH.PRO (WHAT.D _!) (WHICH.D _!))) p))

(defun ident-flag? (p)
  (ttt:match-expr '(! WHAT.PRO WHICH.PRO (WHAT.D _!) (WHICH.D _!)) p))

(defun descr-flag? (p)
  (ttt:match-expr '(! (AT.P (WHAT.D PLACE.N))) p))

(defun time-flag? (p)
  (ttt:match-expr '(! (AT.P (WHAT.D time-word?))) p))

(defun exist-flag? (p)
  (ttt:match-expr '(! THERE.PRO) p))

(defun color-object-flag? (p)
  (ttt:match-expr '(WHAT.D (COLOR.A _!)) p))

(defun color-flag? (p)
  (ttt:match-expr '(OF.P (WHAT.D (! COLOR.N (PLUR COLOR.N)))) p))

(defun explanation-flag? (p)
  (ttt:match-expr '(! WHY.ADV-S) p))

(defun yn-word? (p)
  (member p '(BE.V DO.AUX-S DO.AUX-V CAN.AUX-S CAN.AUX-V)))

(defun time-word? (p)
  (member p '(TURN.N TIME.N STAGE.N STEP.N QUESTION.N ITERATION.N MOVE.N PERIOD.N)))

(defun color-word? (p)
  (member p '(RED.A ORANGE.A YELLOW.A GREEN.A BLUE.A PURPLE.A PINK.A WHITE.A BLACK.A MAGENTA.A GRAY.A GREY.A VIOLET.A INDIGO.A BROWN.A)))

(defun poss-ques? (ulf)
  (and (listp ulf) (equal (car ulf) 'poss-ques)))