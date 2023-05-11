;; Feb 21/2023
;; ===========================================================
;;
;; Contains the schema-based planning module used by Eta, built around the 'plan-step' structure
;; defined below. Each plan-step is a node in a plan polytree containing an episode name and
;; corresponding WFF, as well as possibly other information (probabilistic certainty scores,
;; obligations associated with the episode, etc.). Each plan-step is also marked as an intention
;; or expectation, depending on whether the episode is an action with Eta as the agent, or some
;; other type of episode.
;;
;; Each plan-step contains a pointer to a list of abstract plan-steps that it instantiates (parents),
;; as well as a pointer to a list of more concrete substeps (children).
;;
;; Functions described in more detail in their headers.
;;

(setf *print-circle* t) ; needed to prevent recursive loop when printing plan-step

(defstruct plan-node
;```````````````````````````````
; contains the following fields:
; step : the plan step associated with the node
; prev : the previous step in the overall 'surface' plan
; next : the next step in the overall 'surface' plan
;
  step
  prev
  next
) ; END defstruct plan-node



(defun deepcopy-plan-node (old1) ; {@}
;`````````````````````````````````
; Deep copy a plan-node structure recursively (note that we need to maintain a
; hash table mapping visited plan-step IDs to the new plan-step structures, in
; order to prevent creation of duplicates).
;
  (let ((visited (make-hash-table)))
    (labels ((deepcopy-plan-node-recur (old prev next)
      (let ((new (make-plan-node)))
        (setf (plan-node-step new)
          (deepcopy-plan-step (plan-node-step old) :visited visited))
        (when (plan-node-prev old)
          (setf (plan-node-prev new)
            (if prev
              prev
              (deepcopy-plan-node-recur (plan-node-prev old) nil new))))
        (when (plan-node-next old)
          (setf (plan-node-next new)
            (if next
              next
              (deepcopy-plan-node-recur (plan-node-next old) new nil))))
        new)))
    (deepcopy-plan-node-recur old1 nil nil)
))) ; END deepcopy-plan-node



(defstruct plan-step
;```````````````````````````````
; contains the following fields:
; id         : a unique ID for this plan-step
; substeps   : a list of concrete substeps that are associated with this step
; supersteps : a list of abstract steps that this step instantiates
; ep-name    : episode name of step (gist-clauses, etc. are implicitly attached to ep-name)
; wff        : action formula corresponding to episode name
; certainty  : how certain the step is (if the step is an expected step). The patience of the
;              system in waiting to observe the step is proportional to this
; obligation : the obligation(s) associated with the step
; schemas    : IDs for each schema instance that created this step (if any). Since a plan-step can
;              instantiate multiple abstract actions, it can also have multiple associated schemas
;
  (id (gentemp "STEP"))
  substeps
  supersteps
  ep-name
  wff
  (certainty 1.0) ; defaults to 1, i.e. a certain step
  obligation
  schemas
) ; END defstruct plan-step



(defun deepcopy-plan-step (old &key substep visited) ; {@}
;````````````````````````````````````````````````````````````
; Deep copy a plan-step structure recursively (note that we need to maintain a
; hash table mapping visited plan-step IDs to the new plan-step structures, in
; order to prevent creation of duplicates).
; Since we assume that plan-nodes are at the "surface" of the plan hierarchy,
; this function recurs "downwards" (i.e., towards supersteps).
;
  (let (new)
    (cond
      ; If step has already been visited, get new step structure from hash table
      ((and (hash-table-p visited) (gethash (plan-step-id old) visited))
        (setq new (gethash (plan-step-id old) visited))
        (when substep
          (push substep (plan-step-substeps new))))
          
      ; Otherwise, create new step structure and add to visited hash table
      (t
        (setq new (make-plan-step))
        (setf (plan-step-ep-name new) (copy-tree (plan-step-ep-name old)))
        (setf (plan-step-wff new) (copy-tree (plan-step-wff old)))
        (setf (plan-step-certainty new) (copy-tree (plan-step-certainty old)))
        (setf (plan-step-obligation new) (copy-tree (plan-step-obligation old)))
        (setf (plan-step-schemas new) (copy-tree (plan-step-schemas old)))

        (when substep
          (push substep (plan-step-substeps new)))

        (when (plan-step-supersteps old)
          (setf (plan-step-supersteps new)
            (mapcar (lambda (superstep-old)
                (deepcopy-plan-step superstep-old :substep new :visited visited))
              (plan-step-supersteps old))))

        (setf (gethash (plan-step-id old) visited) new)))

    new
)) ; END deepcopy-plan-step



(defun add-superstep-to-plan (subplan-node-start plan-node) ; {@}
;`````````````````````````````````````````````````````````````````
; Given a plan node assumed to be the start node of a subplan, add the step of
; a given plan-node as a superstep of each node within the subplan.
;
  (loop while (plan-node-next subplan-node-start) do
    (add-superstep-to-plan-step (plan-node-step plan-node) (plan-node-step subplan-node-start))
    (setq subplan-node-start (plan-node-next subplan-node-start)))
  (add-superstep-to-plan-step (plan-node-step plan-node) (plan-node-step subplan-node-start))
) ; END add-superstep-to-plan



(defun add-supersteps-to-plan-node (subplan-node plan-node-start plan-node-end) ; {@}
;``````````````````````````````````````````````````````````````````````````````````
; Given a subplan node and a sequence of plan nodes bounded between a given start and end
; node, add each plan node as a superstep of the given subplan node.
;
  (loop while (and (plan-node-next plan-node-start) (not (eq plan-node-start plan-node-end))) do
    (add-superstep-to-plan-step (plan-node-step plan-node-start) (plan-node-step subplan-node))
    (setq plan-node-start (plan-node-next plan-node-start)))
  (add-superstep-to-plan-step (plan-node-step plan-node-start) (plan-node-step subplan-node))
) ; END add-supersteps-to-plan-node



(defun add-superstep-to-plan-step (superstep plan-step) ; {@}
;`````````````````````````````````````````````````````````````````
; Adds bidirectional subplan/superplan links between a plan-step and a superstep.
; Also add the schemas of the superstep to the plan-step in the case where the
; plan-step doesn't have any associated schemas (e.g., the step was created from
; an episode list in expanding an action).
; TODO: should we instead always append the schemas of the superstep?
;
  (push superstep (plan-step-supersteps plan-step))
  (push plan-step (plan-step-substeps superstep))
  (when (null (plan-step-schemas plan-step))
    (setf (plan-step-schemas plan-step) (plan-step-schemas superstep)))
  ;; (setf (plan-step-schemas plan-step) (remove-duplicates
  ;;   (append (plan-step-schemas superstep) (plan-step-schemas plan-step))))
) ; END add-superstep-to-plan-step



(defun expand-plan-node (plan-node subplan-node-start) ; {@}
;````````````````````````````````````````````````````````
; Given a plan-node and the start/end plan-nodes of some subplan, insert the
; subplan into the plan in place of the given plan-node. This modifies the
; pointers of the previous/next steps in the plan (if any), and also adds the
; plan-step of the original plan-node to the list of supersteps of each substep.
; Returns the start node of the subplan.
;
  (let ((subplan-node-end (get-last-plan-node subplan-node-start)))
    (add-superstep-to-plan subplan-node-start plan-node)
    (when (plan-node-prev plan-node)
      (setf (plan-node-next (plan-node-prev plan-node)) subplan-node-start)
      (setf (plan-node-prev subplan-node-start) (plan-node-prev plan-node)))
    (when (plan-node-next plan-node)
      (setf (plan-node-prev (plan-node-next plan-node)) subplan-node-end)
      (setf (plan-node-next subplan-node-end) (plan-node-next plan-node)))
    subplan-node-start
)) ; END expand-plan-node



(defun insert-before-plan-node (plan-node new-plan-node-start) ; {@}
;``````````````````````````````````````````````````````````````
; Inserts a new plan before the given plan node (adjusting the pointers),
; and returns the new plan node.
;
  (let ((new-plan-node-end (get-last-plan-node new-plan-node-start)))
    (when (plan-node-prev plan-node)
      (setf (plan-node-next (plan-node-prev plan-node)) new-plan-node-start)
      (setf (plan-node-prev new-plan-node-start) (plan-node-prev plan-node)))
    (setf (plan-node-prev plan-node) new-plan-node-end)
    (setf (plan-node-next new-plan-node-end) plan-node)
    new-plan-node-start
)) ; END insert-before-plan-node



(defun merge-plan-nodes (plan-node-start plan-node-end new-plan-node) ; {@}
;```````````````````````````````````````````````````````````````````````
; Merges a sequence of plan nodes (bounded between plan-node-start and plan-node-end)
; into a given subplan node. Returns the new subplan node.
; TODO: do we also need to deal with cases where the plan-nodes are discontiguous in the plan?
;
  (add-supersteps-to-plan-node new-plan-node plan-node-start plan-node-end)
  (when (plan-node-prev plan-node-start)
    (setf (plan-node-next (plan-node-prev plan-node-start)) new-plan-node)
    (setf (plan-node-prev new-plan-node) (plan-node-prev plan-node-start)))
  (when (plan-node-next plan-node-end)
    (setf (plan-node-prev (plan-node-next plan-node-end)) new-plan-node)
    (setf (plan-node-next new-plan-node) (plan-node-next plan-node-end)))
  new-plan-node
) ; END merge-plan-nodes



(defun add-schema-to-plan (plan-node-start schema-id) ; {@}
;``````````````````````````````````````````````````````
; Adds the given schema-id to each plan-step in a plan structure, beginning
; from the given plan node.
;
  (loop while (plan-node-next plan-node-start) do
    (push schema-id (plan-step-schemas (plan-node-step plan-node-start)))
    (setq plan-node-start (plan-node-next plan-node-start)))
  (push schema-id (plan-step-schemas (plan-node-step plan-node-start)))
) ; END add-schema-to-plan



(defun init-plan-from-schema (schema args &key plan-var-table schema-instances) ; {@}
;`````````````````````````````````````````````````````````````````````````````````
; Given a general schema, instantiate a schema instance (replacing
; variables occurring in the header with the supplied args, if any),
; and then instantiate a plan structure from the episodes list of that
; schema. By default, we assume that the episodes of the schema define
; sequential steps.
;
; If a plan-var-table and schema-instances table are given as keys, record the
; instantiated schema and any variable replacements that are made in the
;
; TODO: the plan structure created when instantiating a schema is currently
; "flat" - in the future, we might want to add support for annotating abstraction
; hierarchies in the schema, in which case these would be added as supersteps to
; the steps of the plan-nodes that are created.
;
; TODO: add support for using schema episode-relations when chaining
; together plan-nodes.
;
  (let (schema-instance plan-node-duplicates plan-node duplicates)
    ; Return error if schema has no episodes
    (when (null (dial-schema-episodes schema))
      (format t "*** Attempt to form plan from schema ~a with no episodes"
        (schema-predicate schema))
      (return-from init-plan-from-schema nil))

    ; Create copy of general schema to instantiate
    (setq schema-instance (instantiate-dial-schema schema args))
    (when schema-instances
      (setf (gethash (schema-id schema-instance) schema-instances) schema-instance))

    ; Create a plan structure from the episodes of the schema (in
    ; addition to certainties and obligations)
    (setq plan-node-duplicates (init-plan-from-episode-list
      (get-schema-section schema-instance :episodes)
      :certainties (group-facts-in-schema-section (get-schema-section schema-instance :certainties))
      :obligations (group-facts-in-schema-section (get-schema-section schema-instance :obligations))
      :return-duplicates t))
    (setq plan-node (first plan-node-duplicates))
    (setq duplicates (second plan-node-duplicates))

    ; Add schema-id to newly created plan
    (when plan-node (add-schema-to-plan plan-node (schema-id schema-instance)))

    ; Add any variable replacements to plan-var-table
    (when plan-var-table
      (dolist (pair duplicates)
        (push (list (first pair) (schema-id schema-instance))
          (gethash (second pair) plan-var-table))))


  ; If goals of plan are already satisfied, skip over plan by returning nil,
  ; otherwise return new plan
  ;; TODO REFACTOR : modify the below once goal queue is implemented
  ;; (if (and (plan-goals plan) (every #'check-goal-satisfied (plan-goals plan)))
  ;;   nil
  ;;   plan)
  plan-node
)) ; END init-plan-from-schema



(defun init-plan-from-episodes-from-schemas (episodes schemas &key plan-var-table schema-instances) ; {@}
;````````````````````````````````````````````````````````````````````````````````````````````````````
; Creates a plan structure corresponding to a list of episodes, subject to the corresponding
; obligations and certainties from a set of schemas. This function is necessary for expanding
; conditional/looping plan steps in schemas, since the sub-episode lists of these steps will
; have duplicated variables that need to be associated with the corresponding schema episodes.
;
; TODO: it might be possible to optimize this function by combining it with
; init-plan-from-episode-list in some way; the main difference is that this function is intended
; for episode lists where the variables are already assumed to be duplicates of schema variables
; (and thus need to inherit all relevant information), whereas the other function is for "fresh"
; episode lists that are supplied by functions, rather than schemas.
;
  (let (vars var-replacements steps certainties obligations certainties-all obligations-all
        plan-node-duplicates plan-node duplicates)
    (setq vars (get-all-variables episodes))
    (dolist (schema schemas)
      (setq certainties (group-facts-in-schema-section
        (get-schema-section (gethash schema schema-instances) :certainties :no-binding t)))
      (setq obligations (group-facts-in-schema-section
        (get-schema-section (gethash schema schema-instances) :obligations :no-binding t)))
      (when plan-var-table
        (dolist (var vars)
          (setq var-replacements (remove-if-not
            (lambda (pair) (equal (second pair) schema))
              (gethash var plan-var-table)))
          (dolist (pair var-replacements)
            (setq certainties-all (append (subst (dual-var var) (dual-var (first pair)) certainties) certainties-all))
            (setq obligations-all (append (subst var (first pair) obligations) obligations-all)))))
      (setq certainties-all (remove-duplicates certainties-all :test #'equal))
      (setq obligations-all (remove-duplicates obligations-all :test #'equal)))

    (setq plan-node-duplicates
      (init-plan-from-episode-list episodes :certainties certainties-all :obligations obligations-all :return-duplicates t))
    (setq plan-node (first plan-node-duplicates))
    (setq duplicates (second plan-node-duplicates))

    ; Copy any plan-var-table entries for duplicated variables
    (when plan-var-table
      (dolist (pair duplicates)
        (setf (gethash (second pair) plan-var-table) (gethash (first pair) plan-var-table))))
    
    plan-node
)) ; END init-plan-from-episodes-from-schemas



(defun init-plan-from-episode-list (episodes &key certainties obligations return-duplicates) ; {@}
;``````````````````````````````````````````````````````````````````````````````````````````````
; Given a list of episodes, create a plan structure corresponding to that
; sequence of episodes. May be called as a subroutine in instantiating
; a plan from an overall schema, or from procedurally creating a subplan
; for a particular action (e.g., a say-to action from a paraphrase-to action).
;
; The episodes are converted to plan nodes representing each step in the (sub)plan;
; the first node is returned. Optionally, certainties and obligations for episode
; may be given, in which case these are attached to the corresponding plan steps.
; The assumption is that the ordering of the episodes in the list is the default
; ordering of the resulting plan steps.
;
  (let (steps duplicates first-node prev-node curr-node)

    ; Remove :episodes keyword (if given), and group episodes into steps
    (if (equal :episodes (car episodes)) (setq episodes (cdr episodes)))
    (setq steps (group-facts-in-schema-section episodes))

    ; Give error if first element of first pair isn't episode variable starting with '?'
    (when (not (variable? (caar steps)))
      (format t "*** malformed step ~a while trying to form plan (doesn't begin with episode variable)~%"
        (car steps))
      (return-from init-plan-from-episode-list nil))

    ; Replace variables in episodes list with new unique variable names
    (setq duplicates (duplicate-vars steps))
    (setq steps (first duplicates))
    (setq duplicates (second duplicates))
    (dolist (pair duplicates)
      (setq certainties (subst (dual-var (second pair)) (dual-var (first pair)) certainties))
      (setq obligations (subst (second pair) (first pair) obligations)))

    ; Iterate over steps in episodes list
    (dolist (step steps)

      ; Make plan step structure and set data
      (setq curr-node (make-plan-node))
      (if (null first-node) (setq first-node curr-node))
      (setf (plan-node-step curr-node)
        (make-plan-step
          :ep-name (first step)
          :wff (second step)))
      ; When episode name has certainty associated, add to step
      ; certainties are formulas like (!c1 (!e16 0.3))
      (dolist (certainty certainties)
        (when (equal (first step) (dual-var (first (second certainty))))
          (setf (plan-step-certainty (plan-node-step curr-node)) (second (second certainty)))))
      ; When episode name has obligation associated, add to step
      ; obligations are formulas like (!o1 (?e1 obligates <wff>))
      (dolist (obligation obligations)
        (when (equal (first step) (first (second obligation)))
          (setf (plan-step-obligation (plan-node-step curr-node)) (third (second obligation)))))
      ; When previous step exists, set bidirectional pointers
      (when prev-node
        (setf (plan-node-prev curr-node) prev-node)
        (setf (plan-node-next prev-node) curr-node))
      ; Previous step becomes current step
      (setq prev-node curr-node))

    ;; (format t "plan instantiated from episode list is:~%")
    ;; (print-plan-status first-node) ; DEBUGGING

    (if return-duplicates
      (list first-node duplicates)
      first-node)
)) ; END init-plan-from-episode-list



(defun init-plan-from-episode-list-repeating (episodes-embedded episodes-loop &key schemas plan-var-table schema-instances) ; {@}
;```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
; Initializes a plan from an episode list for a :repeat-until loop. Before initializing
; the subplan, we need to create duplicate episode variables for all of the embedded episodes
; that get instantiated during this particular iteration (while keeping the original episode
; variables for the loop 'yet to be unrolled'). These duplicate variables need to inherit the
; certainties and obligations of the original episode variables in the plan schema (if any).
;
  (let (duplicates episodes-embedded-cloned)
    (setq duplicates (duplicate-vars episodes-embedded))
    (setq episodes-embedded-cloned (first duplicates))
    (setq duplicates (second duplicates))
    (dolist (pair duplicates)
      (when plan-var-table
        (setf (gethash (second pair) plan-var-table) (gethash (first pair) plan-var-table))))
    (init-plan-from-episodes-from-schemas 
      (cons :episodes (append episodes-embedded-cloned episodes-loop))
      schemas
      :plan-var-table plan-var-table
      :schema-instances schema-instances)
)) ; END init-plan-from-episode-list-repeating



(defun certainty-to-period (certainty) ; {@}
;``````````````````````````````````````````
; Maps a certainty from [0,1] to a period corresponding to the
; period (in seconds) that Eta must wait to consider an expected episode
; failed and move on in the plan.
; The proportion between the period (in task cycles) and the
; quantity -log(1 - certainty) is determined by the global
; constant *expected-step-failure-period-coefficient*.
;
  (if (or (>= certainty 1) (< certainty 0))
    'inf
    (* *expected-step-failure-period-coefficient*
      (* -1 (log (- 1 certainty)))))
) ; END certainty-to-period



(defun get-parent-ep-name (plan-step) ; {@}
;``````````````````````````````````````
; If the plan-step is a subplan of another step, get the episode
; name corresponding to that step (in the case that there are multiple
; supersteps, this currently just returns the first).
;
  (let ((supersteps (plan-step-supersteps plan-step)))
    (if supersteps (plan-step-ep-name (car supersteps)))
)) ; END get-parent-ep-name



(defun get-schema-instance-ids (plan-node) ; {@}
;````````````````````````````````````````
; Gets the schema instance IDs corresponding to a given plan-node.
;
  (plan-step-schemas (plan-node-step plan-node))
) ; END get-schema-instance-ids



(defun bind-variable-in-plan (plan-node val var &key plan-var-table schema-instances) ; {@}
;```````````````````````````````````````````````````````````````````````````````````````
; Top-level function for binding a variable in the current plan structure
; (i.e., beginning from the current plan-node).
;
; This consists of the following steps:
; 1. Destructively substitute val for var everywhere in plan structure
; 2. Check plan-var-table for (schema-var, schema-id) pairs
; 3. Retrieve the schema instance for each schema-id
; 4. Bind schema-var to val in each schema instance
;
  (let (pairs schema-var schema-id schema-instance)
    (bind-variable-in-plan-node plan-node val var t t)
    (when (and plan-var-table schema-instances)
      (setq pairs (gethash var plan-var-table))
      (dolist (pair pairs)
        (setq schema-var (first pair))
        (setq schema-id (second pair))
        (setq schema-instance (gethash schema-id schema-instances))
        (bind-variable-in-schema schema-instance schema-var val)))
)) ; END bind-variable-in-plan



(defun bind-variable-in-plan-node (plan-node val var left right) ; {@}
;`````````````````````````````````````````````````````````````````
; Recursively binds a variable throughout a plan structure.
;
  (bind-variable-in-plan-step (plan-node-step plan-node) val var)
  (when (and (plan-node-prev plan-node) left)
    (bind-variable-in-plan-node (plan-node-prev plan-node) val var t nil))
  (when (and (plan-node-next plan-node) right)
    (bind-variable-in-plan-node (plan-node-next plan-node) val var nil t))
) ; END bind-variable-in-plan-node



(defun bind-variable-in-plan-step (plan-step val var) ; {@}
;```````````````````````````````````````````````````````
; Recursively binds a variable throughout a plan-step subtree.
;
  (setf (plan-step-ep-name plan-step) (subst val var (plan-step-ep-name plan-step)))
  (setf (plan-step-wff plan-step) (subst val var (plan-step-wff plan-step)))
  (setf (plan-step-obligation plan-step) (subst val var (plan-step-obligation plan-step)))
  (mapcar (lambda (superstep) (bind-variable-in-plan-step superstep val var))
    (plan-step-supersteps plan-step))
) ; END bind-variable-in-plan-step



(defun get-episode-vars (episodes) ; {@}
;```````````````````````````````````
; Form a list of all episode vars (in proposition form) from a list of episodes.
;
  (let (var vars)
    (cond
      ; Base case - if episodes is a symbol, return the symbol if it is an action
      ; var, or nil otherwise.
      ((symbolp episodes)
        (if (variable? episodes)
          `(,(if (ep-var? episodes) (intern (format nil "~a" episodes)) episodes)) nil))
      ; Recursive case
      (t
        (remove-duplicates
          (remove nil (apply #'append (mapcar #'get-episode-vars episodes)))
          :test #'equal))))
) ; END get-episode-vars



(defun get-step-ep-name (plan-step) ; {@}
;`````````````````````````````````````
; Gets the episode name associated with a plan step.
;
  (plan-step-ep-name plan-step)
) ; END get-step-ep-name



(defun get-step-wff (plan-step) ; {@}
;`````````````````````````````````````
; Gets the wff associated with a plan step.
;
  (plan-step-wff plan-step)
) ; END get-step-wff



(defun get-step-certainty (plan-step) ; {@}
;``````````````````````````````````````
; Gets the certainty associated with a plan step.
;
  (plan-step-certainty plan-step)
) ; END get-step-certainty



(defun get-step-obligations (plan-step) ; {@}
;````````````````````````````````````````
; Gets any obligations associated with a particular step in a plan in the
; schema that the step is part of (look at the parent step as well in case of
; no obligations).
; TODO: this will likely need to be changed in the future once a more general
; mechanism is figured out.
;
  (let ((obligations (plan-step-obligation plan-step)) supersteps)
    (setq supersteps (plan-step-supersteps plan-step))
    ; TODO: this is a bit hacky currently because say-to.v actions may need to access
    ; the parent paraphrase-to.v actions in order to access obligations. Rather, it seems
    ; that the say-to.v actions should inherit the obligations upon creation.
    (when (and supersteps (null obligations))
      (setq obligations (plan-step-obligation (car supersteps))))

    (if (member 'and obligations)
      (remove 'and obligations)
      (if obligations (list obligations) nil))
)) ; END get-step-obligations



(defun format-plan-step (step &key schema-instances) ; {@}
;````````````````````````````````````````````````````````
; Formats the step corresponding to a given plan node, as
; "ep-name : wff [certainty]"
; Or, if a hash table of schema instances is given, as
; "ep-name : wff [certainty] <schema-pred>"
;
  (if schema-instances
    (format nil "~a : ~a [~a] <~a>"
      (get-step-ep-name step)
      (get-step-wff step)
      (get-step-certainty step)
      (str-join (mapcar (lambda (schema)
          (format nil "~a" (schema-predicate (gethash schema schema-instances))))
        (plan-step-schemas step)) #\,))
    (format nil "~a : ~a [~a]"
      (get-step-ep-name step)
      (get-step-wff step)
      (get-step-certainty step)))
) ; END format-plan-step



(defun print-plan-status (node &key (before 3) (after 5) schema-instances) ; {@}
;`````````````````````````````````````````````````````````````````````````````
; Prints the current plan status (i.e., steps that are currently in
; the surface plan, with a pointer to the currently due step). Allows
; the number of steps to be shown before and after this pointer to be
; specified as key arguments.
;
  (let ((prev (plan-node-prev node)) (next (plan-node-next node))
        steps-prev step-curr steps-next)
    (loop while (and prev (> before 0)) do
      (push (format-plan-step (plan-node-step prev) :schema-instances schema-instances) steps-prev)
      (setq prev (plan-node-prev prev))
      (decf before))
    (setq step-curr (format-plan-step (plan-node-step node) :schema-instances schema-instances))
    (loop while (and next (> after 0)) do
      (push (format-plan-step (plan-node-step next) :schema-instances schema-instances) steps-next)
      (setq next (plan-node-next next))
      (decf after))
    ; Print
    (format t "~% --- CURRENT PLAN STATUS: ----------~%")
    (if prev (format t "   ...~%"))
    (dolist (step-prev steps-prev)
      (format t "   ~a~%" step-prev))
    (format t ">> ~a~%" step-curr)
    (dolist (step-next (reverse steps-next))
      (format t "   ~a~%" step-next))
    (if next (format t "   ...~%"))
    (format t " -----------------------------------~%")
)) ; END print-plan-status



(defun print-plan-tree-from-node (plan-node &key schema-instances) ; {@}
;```````````````````````````````````````````````````````````````````
; Prints the subtree of the plan structure reachable from the given node.
;
  (print-plan-tree-from-surface-step (plan-node-step plan-node) :schema-instances schema-instances)
) ; END print-plan-tree-from-node



(defun print-plan-tree-from-surface-step (plan-step &key schema-instances) ; {@}
;```````````````````````````````````````````````````````````````````````````
; Prints the subtree of the plan structure reachable from the given surface step.
;
  (let ((indent-str "       "))
    (setq *print-pretty* nil)
    (labels ((print-tree-recur (step i)
      (when (null step) (return-from print-tree-recur nil))
      (format t "~a~a~%" (str-repeat indent-str i) (format-plan-step step :schema-instances schema-instances))
      (mapcar (lambda (superstep) (print-tree-recur superstep (+ i 1))) (plan-step-supersteps step))))  
    (print-tree-recur plan-step 0)
    (setq *print-pretty* t)
    nil
))) ; END print-plan-tree-from-surface-step



(defun print-plan-tree-from-root-step (plan-step &key schema-instances) ; {@}
;````````````````````````````````````````````````````````````````````````
; Prints the subtree of the plan structure reachable from the given root step.
;
  (let ((indent-str "       "))
    (setq *print-pretty* nil)
    (labels ((print-tree-recur (step i)
      (when (null step) (return-from print-tree-recur nil))
      (format t "~a~a~%" (str-repeat indent-str i) (format-plan-step step :schema-instances schema-instances))
      (mapcar (lambda (substep) (print-tree-recur substep (+ i 1))) (plan-step-substeps step))))  
    (print-tree-recur plan-step 0)
    (setq *print-pretty* t)
    nil
))) ; END print-plan-tree-from-root-step



(defun print-plan-tree-from-all-roots (plan-node &key schema-instances) ; {@}
;````````````````````````````````````````````````````````````````````````
; Prints the subtree of the plan structure reachable from each root step.
;
  (dolist (root (get-all-plan-structure-roots plan-node))
    (print-plan-tree-from-root-step root)
    (format t "~%"))
) ; END print-plan-tree-from-all-roots



(defun get-first-plan-node (plan-node) ; {@}
;`````````````````````````````````````````````````
; Gets the first plan-node in the plan structure, given an arbitrary plan-node.
;
  (loop while (plan-node-prev plan-node) do
    (setq plan-node (plan-node-prev plan-node)))
  plan-node
) ; END get-first-plan-node



(defun get-last-plan-node (plan-node) ; {@}
;`````````````````````````````````````````````````
; Gets the last plan-node in the plan structure, given an arbitrary plan-node.
;
  (loop while (plan-node-next plan-node) do
    (setq plan-node (plan-node-next plan-node)))
  plan-node
) ; END get-last-plan-node



(defun get-all-unique-plan-step-ids (plan-node) ; {@}
;`````````````````````````````````````````````````
; This is a test function to make sure plan methods (primarily deepcopy)
; are working correctly; it returns all unique ids traversable within a plan
; structure, beginning at the current plan-node.
;
  (let (ret)
    (labels
      ((get-all-unique-plan-step-ids-recur1 (node left right)
        (get-all-unique-plan-step-ids-recur2 (plan-node-step node))
        (when (and (plan-node-prev node) left)
          (get-all-unique-plan-step-ids-recur1 (plan-node-prev node) t nil))
        (when (and (plan-node-next node) right)
          (get-all-unique-plan-step-ids-recur1 (plan-node-next node) nil t)))
      (get-all-unique-plan-step-ids-recur2 (step)
        (setq ret (cons (plan-step-id step) ret))
        (mapcar #'get-all-unique-plan-step-ids-recur2 (plan-step-supersteps step))))
  (get-all-unique-plan-step-ids-recur1 plan-node t t)
  (remove-duplicates ret)
))) ; END get-all-unique-plan-step-ids



(defun get-all-unique-plan-schema-ids (plan-node) ; {@}
;`````````````````````````````````````````````````
; This is a test function to make sure plan methods (primarily deepcopy)
; are working correctly; it returns all unique ids from any schemas within a plan
; structure, beginning at the current plan-node.
;
  (let (ret)
    (labels
      ((get-all-unique-plan-schema-ids-recur1 (node left right)
        (get-all-unique-plan-schema-ids-recur2 (plan-node-step node))
        (when (and (plan-node-prev node) left)
          (get-all-unique-plan-schema-ids-recur1 (plan-node-prev node) t nil))
        (when (and (plan-node-next node) right)
          (get-all-unique-plan-schema-ids-recur1 (plan-node-next node) nil t)))
      (get-all-unique-plan-schema-ids-recur2 (step)
        (setq ret (append (plan-step-schemas step) ret))
        (mapcar #'get-all-unique-plan-schema-ids-recur2 (plan-step-supersteps step))))
  (get-all-unique-plan-schema-ids-recur1 plan-node t t)
  (remove-duplicates ret)
))) ; END get-all-unique-plan-schema-ids



(defun get-all-plan-structure-roots (plan-node) ; {@}
;`````````````````````````````````````````````````
; Get all root plan-steps in a plan structure
; (i.e., those without any supersteps).
;
  (let (ret)
    (labels
      ((get-all-plan-structure-roots-recur1 (node left right)
        (get-all-plan-structure-roots-recur2 (plan-node-step node))
        (when (and (plan-node-prev node) left)
          (get-all-plan-structure-roots-recur1 (plan-node-prev node) t nil))
        (when (and (plan-node-next node) right)
          (get-all-plan-structure-roots-recur1 (plan-node-next node) nil t)))
      (get-all-plan-structure-roots-recur2 (step)
        (when (null (plan-step-supersteps step))
          (setq ret (cons step ret)))
        (mapcar #'get-all-plan-structure-roots-recur2 (plan-step-supersteps step))))
  (get-all-plan-structure-roots-recur1 plan-node t t)
  (remove-duplicates ret :test (lambda (x y) (equal (plan-step-id x) (plan-step-id y))))
))) ; END get-all-plan-structure-roots



(defun print-plan-var-table (plan-var-table) ; {@}
;`````````````````````````````````````````````
; Prints the entries in the plan variable table.
;
  (format t "~% --- PLAN VAR TABLE: ----------~%")
  (maphash (lambda (var pairs)
      (format t "~a:~%" var)
      (dolist (pair pairs)
        (format t "  ~a~%" pair)))
    plan-var-table)
  (format t " ---------------------------------~%")
) ; END print-plan-var-table



(defun make-test-plan-structure () ; {@}
;```````````````````````````````````````````
; Creates an artificial plan structure for testing purposes.
;
  (let* (
    (plan-var-table (make-hash-table))
    (schema-instances (make-hash-table))
    (s1 (make-dial-schema :predicate 'schema1.v))
    (s2 (make-dial-schema :predicate 'schema2.v))
    (s3 (make-dial-schema :predicate 'schema3.v))
    (s4 (make-dial-schema :predicate 'schema4.v))
    (sid1 (schema-id s1))
    (sid2 (schema-id s2))
    (sid3 (schema-id s3))
    (sid4 (schema-id s4))
    (pe1  (make-plan-step :ep-name '?e1 :wff '(test ?e1)))
    (pe2  (make-plan-step :ep-name '?e2 :wff '(test ?e1)))
    (pe3  (make-plan-step :ep-name '?e3 :wff '(test ?e1 ?e2 ?words)))
    (pe4  (make-plan-step :ep-name '?e4 :wff '(test ?e1 ?e2 ?e3)))
    (pe5  (make-plan-step :ep-name '?e5 :wff '(test ?e1 ?e2 ?e3 ?e4 ?words)))
    (pe6  (make-plan-step :ep-name '?e6 :wff '(test ?e4 ?words ?e5)))
    (pe7  (make-plan-step :ep-name '?e7 :wff '(test ?e1 ?e6)))
    (pe8  (make-plan-step :ep-name '?e8 :wff '(test ?e9 ?e7)))
    (pe9  (make-plan-step :ep-name '?e9 :wff '(test ?e1 ?e12)))
    (pe10 (make-plan-step :ep-name '?e10 :wff '(test ?e7 ?e8 ?e9)))
    (pe11 (make-plan-step :ep-name '?e11 :wff '(test ?e1 ?words)))
    (pe12 (make-plan-step :ep-name '?e12 :wff '(test ?e1 ?e11)))
    (n8  (make-plan-node :step pe8))
    (n9  (make-plan-node :step pe9))
    (n10 (make-plan-node :step pe10))
    (n11 (make-plan-node :step pe11))
    (n12 (make-plan-node :step pe12))
    (n7  (make-plan-node :step pe7))
    )
  (setf (plan-step-substeps pe1) (list pe3 pe4 pe5))
  (setf (plan-step-substeps pe2) (list pe4 pe6 pe7))
  (setf (plan-step-substeps pe3) (list pe8 pe9))
  (setf (plan-step-substeps pe4) (list pe10))
  (setf (plan-step-substeps pe5) (list pe9 pe11))
  (setf (plan-step-substeps pe6) (list pe12))
  (setf (plan-step-supersteps pe3) (list pe1))
  (setf (plan-step-supersteps pe4) (list pe1 pe2))
  (setf (plan-step-supersteps pe5) (list pe1))
  (setf (plan-step-supersteps pe6) (list pe2))
  (setf (plan-step-supersteps pe7) (list pe2))
  (setf (plan-step-supersteps pe8) (list pe3))
  (setf (plan-step-supersteps pe9) (list pe3 pe5))
  (setf (plan-step-supersteps pe10) (list pe4))
  (setf (plan-step-supersteps pe11) (list pe5))
  (setf (plan-step-supersteps pe12) (list pe6))
  (setf (plan-step-schemas pe1) (list sid1))
  (setf (plan-step-schemas pe2) (list sid2))
  (setf (plan-step-schemas pe3) (list sid1 sid3))
  (setf (plan-step-schemas pe4) (list sid1 sid2))
  (setf (plan-step-schemas pe5) (list sid1 sid2 sid4))
  (setf (plan-step-schemas pe6) (list sid2))
  (setf (plan-step-schemas pe7) (list sid2))
  (setf (plan-step-schemas pe8) (list sid1 sid3))
  (setf (plan-step-schemas pe9) (list sid1 sid3 sid4))
  (setf (plan-step-schemas pe10) (list sid1 sid2))
  (setf (plan-step-schemas pe11) (list sid1 sid2 sid4))
  (setf (plan-step-schemas pe12) (list sid2))
  (setf (plan-node-next n8) n9)
  (setf (plan-node-prev n9) n8)
  (setf (plan-node-next n9) n10)
  (setf (plan-node-prev n10) n9)
  (setf (plan-node-next n10) n11)
  (setf (plan-node-prev n11) n10)
  (setf (plan-node-next n11) n12)
  (setf (plan-node-prev n12) n11)
  (setf (plan-node-next n12) n7)
  (setf (plan-node-prev n7) n12)
  (dolist (schema (list s1 s2 s3 s4))
    (setf (gethash (schema-id schema) schema-instances) schema))
  (dolist (triple
          `(
            (?e1 ?ea1 ,sid1)
            (?e2 ?eb1 ,sid2)
            (?e3 ?ea2 ,sid1)
            (?e3 ?ec1 ,sid3)
            (?words ?w ,sid1)
            (?words ?w ,sid3)
            (?e4 ?ea3 ,sid1)
            (?e4 ?eb2 ,sid2)
            (?e5 ?ea4 ,sid1)
            (?e5 ?eb3 ,sid2)
            (?e5 ?ed1 ,sid4)
            (?e6 ?eb4 ,sid2)
            (?e7 ?eb5 ,sid2)
            (?e8 ?ea5 ,sid1)
            (?e8 ?ec2 ,sid3)
            (?e9 ?ea6 ,sid1)
            (?e9 ?ec3 ,sid3)
            (?e9 ?ed2 ,sid4)
            (?e10 ?ea7 ,sid1)
            (?e10 ?eb6 ,sid2)
            (?e11 ?ea8 ,sid1)
            (?e11 ?eb7 ,sid2)
            (?e11 ?ed3 ,sid4)
            (?e12 ?eb8 ,sid2)
          ))
    (push (list (second triple) (third triple)) (gethash (first triple) plan-var-table)))
  (list n11 plan-var-table schema-instances)
)) ; END make-test-plan-structure