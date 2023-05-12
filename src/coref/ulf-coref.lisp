; Ben Kane 7-16-2019
;
; Code for resolving references in ulf.
;

(in-package :eta)

(defun coref-ulf (ulf ds)
;`````````````````````````
; The top level coreference function for ulf. Wrapper for process-ulf function.
;
  (first (process-ulf ulf ds))
) ; END coref-ulf


(defun coref-gist (gist)
;````````````````````````
; Currently unused.
;
  gist
) ; END coref-gist


(defun add-entity (entity ds)
; ````````````````````````````
; Adds an entity to the reference list.
;
  (push entity (ds-reference-list ds))
) ; END add-entity


(defun corefer (de1 de2 ds)
; ```````````````````````````
; Makes entity de1 corefer to entity de2 by adding the references of the new entity to the
; references of the existing entity, and making the references of the new-entity point to the
; existing entity as a referent, as well as updating recency. If the coreferent provides
; a more specific type and modifiers, update those as well.
; de2 is assumed to be newer (so we copy the recency of 1 to 2)
;
  ;; (setf (get de1 'references)
  ;;   (remove-duplicates (append (get de1 'references) (get de2 'references)) :test #'equal))
  (setf (get de2 'references)
    (remove-duplicates (append (get de2 'references) (get de1 'references)) :test #'equal))
  (if (get de1 'recency)
    (setf (get de2 'recency) (get de1 'recency)))
  (if (compare-type (get de1 'type) (get de2 'type))
    (setf (get de2 'type) (get de1 'type)))
  (setf (get de2 'mods)
    (remove-duplicates (append (get de1 'mods) (get de2 'mods)) :test #'equal))
  (setf (ds-reference-list ds) (cons de2 (remove de1 (ds-reference-list ds))))
) ; END corefer


(defun prune-reference-list (recency ds)
; ````````````````````````````````````````
; Prunes the reference list of all discourse entities past a certain recency threshold (keeping a DE
; in the case that it doesn't have a recency assigned).
;
  (remove-if (lambda (de) (and (get de 'recency) (< recency (get de 'recency)))) (ds-reference-list ds))
) ; END prune-reference-list


(defun update-reference-list (ds)
; ```````````````````````````````
; Updates the reference list after a sentence by incrementing the recency of all entities which are assigned
; a recency.
;
  (setf (ds-reference-list ds) (mapcar (lambda (de)
    (if (and (get de 'recency)) (setf (get de 'recency) (1+ (get de 'recency)))) de) (ds-reference-list ds)))
) ; END update-reference-list


(defun print-entities (ds &key verbose)
; ````````````````````````````````````````
; Outputs all entities in reference list.
;
  (mapcar
    (lambda (x) (if verbose (format t "entity: ~a~% |- type: ~a~% |- mods: ~a~% |- recency: ~a~% |- salience: ~a~%"
                              x (get x 'type) (get x 'mods) (get x 'recency) (get x 'salience))
                            (format t "- ~a~%" x)) x)
    (ds-reference-list ds))
) ; END print-entities


;; (defun create-implicit-discourse-entity-from-verb (ulf)
;; ; ````````````````````````````````````````````````````````````````````````
;; ; A verb can implicitly introduce a couple different discourse entities, corresponding to some of the permutations
;; ; of subjects and objects of the verb. e.g.:
;; ; 1.  Beth and Mary met John at a bar.
;; ; 2a. They bought him a drink.
;; ; 2b. They had a lot of fun.
;; ; NOTE: actually this can happen with nouns that are further apart, like "John went to the bar to buy Mary a drink,
;; ; they had a lot of fun". I'm not sure how to deal with this in general.
;; ;
  
;; ) ; END create-implicit-discourse-entity-from-verb


(defun intra-sentence-candidates (de-list)
; ``````````````````````````````````````````
; Takes a de-list (more precisely a tree of discourse entities), and creates a list of lists where each sub-list consists
; of each discourse entity and its possible candidates from the same list. Generally, an intra-sentence candidate for a
; discourse entity can be itself, any of its parents, any preceeding siblings, or any descendents of preceeding siblings.
;
  (let (poss-candidate-list)
    (labels ((form-candidates-recur (lst)
      (cond
        ((atom lst) nil)
        ((and (listp lst) (atom (car lst)))
          (setq poss-candidate-list (cons (car lst) poss-candidate-list))
          (cons (append (list (car lst)) poss-candidate-list)
                (form-candidates-recur (cdr lst))))
        (t (append (form-candidates-recur (car lst))
                   (form-candidates-recur (cdr lst)))))))
    (form-candidates-recur de-list)))
) ; END intra-sentence-candidates


(defun inter-sentence-candidates (de-list ds)
; ``````````````````````````````````````````````
; Takes a de-list (more precisely a tree of discourse entities), and creates a list of lists where each sub-list consists
; of each discourse entity and its possible inter-sentence candidates. Here we maintain a (flat) reference list of previous
; discourse entities, and all previous discourse entities (within a particular cutoff) are assigned to all entities in de-list.
;
  (let ((poss-candidate-list (prune-reference-list *recency-cutoff* ds)))
    (labels ((form-candidates-recur (lst)
      (cond
        ((atom lst) nil)
        ((and (listp lst) (atom (car lst)))
          (cons (append (list (car lst)) poss-candidate-list)
                (form-candidates-recur (cdr lst))))
        (t (append (form-candidates-recur (car lst))
                   (form-candidates-recur (cdr lst)))))))
    (form-candidates-recur de-list)))
) ; END inter-sentence-candidates


(defun get-candidate-lists (de-list ds)
; ``````````````````````````````````````
; Creates lists of candidates for each new discourse entity in de-list, consisting of both intra-sentence candidates and
; inter-sentence candidates.
;
  (mapcar (lambda (intra inter) (list (car intra) (append (cdr intra) (cdr inter))))
    (intra-sentence-candidates de-list) (inter-sentence-candidates de-list ds))
) ; END get-candidate-lists


(defun subst-discourse-entities (ulf)
; ``````````````````````````````````````
; Substitutes all references in a ulf with symbols representing instantiated discourse entities
; (with attached information about type, mods, recency, etc.) for all ulf phrases which refer to individuals.
; Returns a list (ulf-new de-list) where de-list is a tree of all the new discourse entities that were created,
; nested in the same way as the discourse entities appeared in the ulf.
;
; e.g.,
; * (subst-discourse-entities '((the.d man.n) ((past see.v) |Mary|)))
; '((DE58.DE ((PAST SEE.V) DE59.DE)) (DE58.DE DE59.DE))
; 
  (labels ((subst-discourse-entities-recur (ulf depth)
    (let (de-list parts ulf-result de-list-parts de)
      ; If we encounter a non-atomic ulf expression, we want to recur into each part (before creating the
      ; respective discourse entity, since a discourse entity can recursively reference other discourse entities,
      ; e.g. "the block on the table").
      (when (listp ulf)
        (setq parts (mapcar (lambda (x) (subst-discourse-entities-recur x (1+ depth))) ulf))
        (setq ulf-result (mapcar #'first parts))
        (setq de-list-parts (mapcan #'second parts)))
      (cond
        ; Whenever we find a ulf denoting an individual type (e.g. determiner+noun, reified noun, proper name, etc.)
        ; we create a discourse entity. Initialize recency to zero and assign salience depending on the phrase depth
        ; the ulf was found at.
        ((indiv? ulf)
          (setq de (create-discourse-entity (if ulf-result ulf-result ulf)))
          (setf (get de 'recency) 0)
          (setf (get de 'salience) (cond
            ((< depth 2) 1)
            ((= depth 2) 0.5)
            ((> depth 2) 0.25)))
          ; Append the newly created discourse entity to the list so far, and set ulf result to new discourse entity.
          (setq de-list (if de-list-parts (list (list de de-list-parts)) (list de)))
          (setq ulf-result de))
        ; If a non-individual atomic ulf is found, the result ulf is simply that ulf.
        ((atom ulf)
          (setq ulf-result ulf))
        ; Otherwise, we have some non-individual non-atomic ulf, so return the de-list and ulf that was found recursively
        ; in the earlier step.
        (t
          (setq de-list de-list-parts)))
      ; Return a pair consisting of the resulting ulf and the list of created discourse entities.
      (list ulf-result de-list))))
    (subst-discourse-entities-recur ulf 0))
) ; END subst-discourse-entities


(defun create-discourse-entity (ref)
; `````````````````````````````````````
; Creates a discourse entity from a reference ulf by generating a symbol for the reference, and attaching information
; to that symbol about the ulf it was created from, initializing a list of phrases used to refer to that entity,
; getting the type & mods of the entity, and the category of the reference (e.g. indefinite reference, reflexive pronoun, ...)
;
  (let ((de (gentemp "DE")))
    ; Link original ref to entity, and initialize list of references to that entity
    (setf (get de 'ulf) ref)
    (setf (get de 'references) (list de))
    ; Attach type & mods of that entity
    (let ((type+mods (get-type+mods (reconstruct-ulf ref))))
      (setf (get de 'type) (car type+mods))
      (setf (get de 'mods) (cdr type+mods)))
    ; Attach categorization of entity (reference/pronoun type)
    (setf (get de 'cat)
      (cond
        ((reified-sentence? ref) 'sentence)
        ((reified-event? ref) 'event)
        ((reified-action? ref) 'action)
        ((proper-name? ref) 'proper-name)
        ((definite-np? ref) 'definite-np)
        ((indexical-np? ref) 'indexical-np)
        ((wh-np? ref) 'wh-np)
        ((wh-pron? ref) 'wh-pron)
        ((indefinite-np? ref) 'indefinite-np)
        ((existential-there? ref) 'existential-there)
        ((reflexive? ref) 'reflexive)
        ((relative? ref) 'relative)
        ((anaphor? ref) 'anaphor)
        (t 'sentence) ; assume this if no other category is matched
      ))
    ; Attach name (if proper name)
    (if (proper-name? ref) (setf (get de 'name) ref))
  de)
) ; END create-discourse-entity


(defun preprocess-ulf (ulf)
; ``````````````````````````
; Preprocess ulf. Steps:
; 1. Apply macros [NOT DONE ATM]
; 2. Replace ellipsis
; 3. Convert possessives to poss-by form, e.g. (the.d ((poss-by |Mary|) (small.a dog.n))) [NOT DONE YET]
; 4. Other necessary transformations?
;
  (ttt:apply-rule '(/ ellipsis? (replace-ellipsis! ellipsis?)) ulf)
) ; END preprocess-ulf


(defun process-ulf (ulf ds)
; ``````````````````````````
; Processes a ulf by obtaining and substituting most likely coreferences. First preprocesses the ulf by
; applying macros and other substitutions, and then substitutes discourse entity symbols into the ulf for
; all references. Then generates candidate lists for each discourse entity, weights each candidate, gets the
; highest weighted candidate, and makes that candidate corefer to the discourse entity (or accomodates if all
; candidates are weighted below zero). Finally, substitute the "most specific instance" of that discourse entity
; into the ULF for that reference (e.g. he.pro -> |John|).
;
  ; Preprocess the incoming ulf
  (setq ulf (preprocess-ulf ulf))
  ; Upon recieving new sentence, update recency of previous references
  (update-reference-list ds)
  ; Candidate list for each reference consists of all DEs referenced in the
  ; previous sentence, and the DEs introduced in the new sentence
  (let* ((ulf+de-list (subst-discourse-entities ulf))
         (ulf (first ulf+de-list))
         (de-list (second ulf+de-list))
         (candidate-lists (get-candidate-lists de-list ds))
         results)

  ; Do for each pair of discourse entity and candidates for coreference
  (setq results (mapcar (lambda (candidate-list)
    ; Calculate weight for each candidate, and determine the most likely reference
    (let* ((weighted-options (assign-weights candidate-list ulf)))

      ;; (format t "~%~%ulf: ~a" ulf) ; DEBUGGING
      ;; (format t "~%weighted options: ~a~%" weighted-options) ; DEBUGGING

      (get-most-likely weighted-options))) candidate-lists))

  ;; (format t "~%results: ~a~%" results) ; DEBUGGING

  ; TODO: this is a super hack intended to support elliptical questions in the blocks world QA
  ; setting - e.g., "why?". Currently the coref module does not support more complex forms of
  ; references, such as sentential references, so this is added to allow for that particular use
  ; case. Ultimately something more general needs to be implemented.
  (setq results (mapcar (lambda (pair)
      (cond
        ; If any DE refers to "your previous question", find the user's previous question
        ; in the conversation log, create new discourse entity, and replace in pair
        ((equal (get (car pair) 'ulf) '(your.d (previous.a question.n)))
          ; Get the contents of the previous question (second to last question ULF
          ; in conversation log), excluding the question mark
          (let ((prev-question (first (car (remove nil
                    (mapcar (lambda (ulfs) (if (question-ulf? (car ulfs)) (car ulfs)))
                      (mapcar #'dialogue-turn-semantics (ds-conversation-log ds))))))))
            (if prev-question
              (cons (car pair) (create-discourse-entity prev-question))
              pair)))
        ; Otherwise, pair is unchanged
        (t pair)))
    results))

  ;; (format t "~%results (updated): ~a~%" results) ; DEBUGGING

  ; Corefer each discourse entity to its top candidate (or accomodate that entity if the top candidate is itself)
  (mapcar (lambda (pair)
      (if (equal (car pair) (cdr pair))
        (add-entity (car pair) ds)
        (corefer (cdr pair) (car pair) ds)))
    results)

  ; Return the resolved ulf as well as the discourse entities that were matched
  (list ulf results))
) ; END process-ulf


(defun assign-weights (candidate-list ulf)
; ````````````````````````````````````````````
; Calculates and assigns weights to candidates for coreference to a discourse entity. Each weight is calculated
; as a linear combination of various "constraints" or features that are calculated between each pair of entities.
; If the discourse entity and a candidate are equivalent, that candidate is given weight 0, as the "default" choice
; of accomodating that entity (i.e. creating a new discourse entity).
;
  (let* ((de1 (car candidate-list)) (weights+constraints (get-weights+constraints)) 
                                    (weights (mapcar #'first weights+constraints))
                                    (constraints (mapcar #'second weights+constraints)))
    (list de1 (mapcar (lambda (de2)
      (list de2 (cond
        ((equal de1 de2) 0)
        (t
          ;; (format t "~%~a [~a]~% |~% V~%~a [~a]~%``````````````````````````````````````````~%"
          ;;   de1 (get de1 'ulf) de2 (get de2 'ulf)) ; DEBUGGING

          (reduce #'+ (mapcar (lambda (w c)

            ;; (format t "    * [~a] ~a = ~a~%" w c (funcall c de1 de2 ulf)) ; DEBUGGING
            (* w (funcall c de1 de2 ulf)))
           weights constraints))))))
    (second candidate-list))))
) ; END assign-weights


(defun get-most-likely (candidate-list)
; ```````````````````````````````````````
; Gets the most likely reference from a candidate list by sorting the list based on weights.
;
  (cons (car candidate-list)
    (caar (sort (second candidate-list) #'> :key #'second)))
) ; END get-most-likely


(defun get-most-specific-reference (de)
; ```````````````````````````````````````
; Gets the most specific ulf referring to a discourse entity, sorting by category of the reference
; (e.g. proper names preferred to definite noun phrases, definite noun phrases preferred to indefinite
; noun phrases, etc.).
;
  (get (car
    (sort (get de 'references) #'< :key
      (lambda (de-ref)
        (let ((cat (get de-ref 'cat)))
          (cond
            ((equal cat 'proper-name) 0)
            ((equal cat 'relative) 0)
            ((equal cat 'definite-np) 1)
            ((equal cat 'sentence) 1)
            ((equal cat 'event) 1)
            ((equal cat 'action) 1)
            ((equal cat 'indefinite-np) 2)
            ((equal cat 'indexical-np) 3)
            ((equal cat 'wh-np) 3)
            ((equal cat 'wh-pron) 3)
            ((equal cat 'reflexive) 3)
            ((equal cat 'anaphor) 3)
            (t 4))))))
  'ulf)
) ; END get-most-specific-reference


(defun reconstruct-ulf (ulf &key verbose)
; ````````````````````````````````````````
; Reconstructs original ulf from processed ulf. Supply :verbose t
; as an argument to print information about each pronoun/reference.
;
  (cond
    ((and (atom ulf) (get ulf 'ulf))
      (cond (verbose
        (format t "sk: ~a~% |- ulf: ~a~% |- cat: ~a~% |- type: ~a~% |- mods: ~a~%"
          ulf (get ulf 'ulf) (get ulf 'cat) (get ulf 'type) (get ulf 'mods))))
      (reconstruct-ulf (get ulf 'ulf) :verbose verbose))
    ((and (atom ulf)) ulf)
    (t (mapcar (lambda (x)
        (reconstruct-ulf x :verbose verbose))
      ulf)))
) ; END reconstruct-ulf


(defun resolve-references (ulf)
; ````````````````````````````````
; Retrieves the references for each discourse entity and substitutes them
; in for that discourse entity to obtain a final result. This depends on coreference mode parameter:
; 0 : simply reconstruct the original ulf
; 1 : mode 2 but excluding i.pro and you.pro from resolved references
; 2 : substitute most specific references only for anaphors and indexical np's (e.g. that block)
; 3 : substitute most specific references for all references
; More may be added in the future
; NOTE: Maintain a list of visited references to avoid infinite recursion (if accidental cycle with reference links)
;
  ;; (format t "resolving: ~a~%" ulf) ; DEBUGGING
  (labels ((resolve-references-recur (ulf visited)
      (cond
        ((and (equal *coreference-mode* 1) (atom ulf) (not (member ulf visited)) (get ulf 'references)
              (member (get ulf 'cat) '(anaphor indexical-np)) (not (member (get ulf 'ulf) '(i.pro you.pro))))
          (resolve-references-recur (get-most-specific-reference ulf) (cons ulf visited)))
        ((and (equal *coreference-mode* 2) (atom ulf) (not (member ulf visited)) (get ulf 'references)
              (member (get ulf 'cat) '(anaphor indexical-np)))
          (resolve-references-recur (get-most-specific-reference ulf) (cons ulf visited)))
        ((and (equal *coreference-mode* 3) (atom ulf) (not (member ulf visited)) (get ulf 'references))
          (resolve-references-recur (get-most-specific-reference ulf) (cons ulf visited)))
        ((and (atom ulf) (get ulf 'ulf)) (resolve-references-recur (get ulf 'ulf) visited))
        ((atom ulf) ulf)
        (t (mapcar (lambda (x)
            (resolve-references-recur x visited))
          ulf)))))
    (resolve-references-recur ulf nil))
) ; END resolve-references