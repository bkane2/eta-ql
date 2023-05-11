;; Feb 21/2023
;; ===========================================================
;;
;; Contains utilities for reading, storing, and processing schemas.
;;
;; NOTE on variable binding:
;;  Variable bindings in instantiated schemas are currently handled
;; using a 'bindings' hash table that stores values for each variable
;; within the schema. Helper functions are defined for retrieving each
;; schema section (e.g., for adding intentions/expectations to the plan,
;; or for inference) that make the necessary substitutions for each bound
;; variable.
;;  One thing I'm currently unsure of is whether it would suffice
;; to instead destructively modify the instantiated schema, so that the
;; contents are updated with the variable bindings without maintaining a
;; separate hash table and making "lazy" substitutions when retrieving facts
;; from a schema.
;;  In either case, the assumption is that this module hides all the code
;; required for applying variable bindings. Using the getter functions in
;; this module will ensure that the schema sections are returned with the
;; correct substitutions applied.
;; 
;;

;`````````````````````````````````````````````````````
; Store a global hash table of episode schema headers to
; the corresponding schema structures.
;
(defparameter *schemas* (make-hash-table))



(defstruct schema
;```````````````````
; contains the following fields:
; id            : a unique ID for this schema
; predicate     : the main predicate of the schema
; participants  : the participant roles of a schema present in the header (e.g., '(^me ^you))
; vars          : the variables scoped within a schema
; bindings      : current bindings for each variable in the schema (initialized to empty hash table)
; header        : the full header of the schema
; contents      : the contents of the schema as a list
  (id (gentemp "SCHEMA"))
  predicate
  participants
  vars
  (bindings (make-hash-table))
  header
  contents
) ; END defstruct schema



(defstruct (dial-schema (:include schema))
;```````````````````````````````````````````
; contains the following fields:
; types             : the nominal types of each participant/variable
; rigid-conds       : non-fluent conditions relevant to episode
; static-conds      : fluent conditions that are not expected to change during episode
; preconds          : fluent conditions that are expected to hold at the beginning of episode
; postconds         : fluent conditions that are expected to hold at end of episode
; goals             : goals of participants in schema (e.g., (^me want.v (that ...)))
; episodes          : the expected/intended sub-episodes of the schema episode
; episode-relations : the temporal/causal relations between episodes of schema
; obligations       : dialogue obligations associated with particular episodes
; necessities       : probabilities associated with schema formulas
; certainties       : probabilities associated with schema episodes
;
  types
  rigid-conds
  static-conds
  preconds
  postconds
  goals
  episodes
  episode-relations
  obligations
  necessities
  certainties
) ; END defstruct dial-schema



(defun deepcopy-dial-schema (old &key keep-id)
;```````````````````````````````````````````````
; Deep copy an episode schema structure
;
  (let (new)
    (if keep-id
      (setq new (make-dial-schema :id (schema-id old)))
      (setq new (make-dial-schema)))
    (setf (dial-schema-predicate new) (copy-tree (dial-schema-predicate old)))
    (setf (dial-schema-participants new) (copy-tree (dial-schema-participants old)))
    (setf (dial-schema-vars new) (copy-tree (dial-schema-vars old)))
    (setf (dial-schema-bindings new) (deepcopy-hash-table (dial-schema-bindings old)))
    (setf (dial-schema-contents new) (copy-tree (dial-schema-contents old)))
    (setf (dial-schema-header new) (copy-tree (dial-schema-header old)))
    (setf (dial-schema-types new) (copy-tree (dial-schema-types old)))
    (setf (dial-schema-rigid-conds new) (copy-tree (dial-schema-rigid-conds old)))
    (setf (dial-schema-static-conds new) (copy-tree (dial-schema-static-conds old)))
    (setf (dial-schema-preconds new) (copy-tree (dial-schema-preconds old)))
    (setf (dial-schema-postconds new) (copy-tree (dial-schema-postconds old)))
    (setf (dial-schema-goals new) (copy-tree (dial-schema-goals old)))
    (setf (dial-schema-episodes new) (copy-tree (dial-schema-episodes old)))
    (setf (dial-schema-episode-relations new) (copy-tree (dial-schema-episode-relations old)))
    (setf (dial-schema-obligations new) (copy-tree (dial-schema-obligations old)))
    (setf (dial-schema-necessities new) (copy-tree (dial-schema-necessities old)))
    (setf (dial-schema-certainties new) (copy-tree (dial-schema-certainties old)))
    new
)) ; END deepcopy-dial-schema



(defun store-dial-schema (predicate schema-contents)
;````````````````````````````````````````````````````
; Given a predicate for an dial-schema and the schema (as a list of sections),
; create a new schema structure and store it in the global schema hash table,
; with the predicate as a key.
;
  (let (schema header participants vars)
    (push (list :predicate predicate) schema)
    (setq header (car (get-keyword-contents schema-contents '(:header))))
    (push (list :participants (remove 'set-of (remove predicate (butlast (flatten header) 2)))) schema)
    (push (list :vars (remove-duplicates (remove-if-not #'variable? (flatten schema-contents)))) schema)
    (push (list :contents schema-contents) schema)

    ; schema sections
    (push (list :header header) schema)
    (dolist (section '(:types :rigid-conds :static-conds :preconds :postconds :goals
                       :episodes :episode-relations :obligations :necessities :certainties))
      (let ((section-contents (car (get-keyword-contents schema-contents (list section)))))
        (when section-contents
          (push (list section section-contents) schema))))

    (setf (gethash predicate *schemas*)
      (apply #'make-dial-schema (apply #'append schema)))
)) ; END store-dial-schema



(defstruct (epi-schema (:include schema))
;```````````````````````````````````````````
; contains the following fields:
; types             : the nominal types of each participant/variable
; rigid-conds       : non-fluent conditions relevant to episode
; static-conds      : fluent conditions that are not expected to change during episode
; preconds          : fluent conditions that are expected to hold at the beginning of episode
; postconds         : fluent conditions that are expected to hold at end of episode
; goals             : goals of participants in schema (e.g., (^me want.v (that ...)))
; episodes          : the expected/intended sub-episodes of the schema episode
; episode-relations : the temporal/causal relations between episodes of schema
; obligations       : dialogue obligations associated with particular episodes
; necessities       : probabilities associated with schema formulas
; certainties       : probabilities associated with schema episodes
;
  types
  rigid-conds
  static-conds
  preconds
  postconds
  goals
  episodes
  episode-relations
  obligations
  necessities
  certainties
) ; END defstruct epi-schema



(defun deepcopy-epi-schema (old &key keep-id)
;```````````````````````````````````````````````
; Deep copy an episode schema structure
;
  (let (new)
    (if keep-id
      (setq new (make-epi-schema :id (schema-id old)))
      (setq new (make-epi-schema)))
    (setf (epi-schema-predicate new) (copy-tree (epi-schema-predicate old)))
    (setf (epi-schema-participants new) (copy-tree (epi-schema-participants old)))
    (setf (epi-schema-vars new) (copy-tree (epi-schema-vars old)))
    (setf (epi-schema-bindings new) (deepcopy-hash-table (epi-schema-bindings old)))
    (setf (epi-schema-contents new) (copy-tree (epi-schema-contents old)))
    (setf (epi-schema-header new) (copy-tree (epi-schema-header old)))
    (setf (epi-schema-types new) (copy-tree (epi-schema-types old)))
    (setf (epi-schema-rigid-conds new) (copy-tree (epi-schema-rigid-conds old)))
    (setf (epi-schema-static-conds new) (copy-tree (epi-schema-static-conds old)))
    (setf (epi-schema-preconds new) (copy-tree (epi-schema-preconds old)))
    (setf (epi-schema-postconds new) (copy-tree (epi-schema-postconds old)))
    (setf (epi-schema-goals new) (copy-tree (epi-schema-goals old)))
    (setf (epi-schema-episodes new) (copy-tree (epi-schema-episodes old)))
    (setf (epi-schema-episode-relations new) (copy-tree (epi-schema-episode-relations old)))
    (setf (epi-schema-obligations new) (copy-tree (epi-schema-obligations old)))
    (setf (epi-schema-necessities new) (copy-tree (epi-schema-necessities old)))
    (setf (epi-schema-certainties new) (copy-tree (epi-schema-certainties old)))
    new
)) ; END deepcopy-epi-schema



(defun store-epi-schema (predicate schema-contents)
;````````````````````````````````````````````````````
; Given a predicate for an epi-schema and the schema (as a list of sections),
; create a new schema structure and store it in the global schema hash table,
; with the predicate as a key.
;
  (let (schema header participants vars)
    (push (list :predicate predicate) schema)
    (setq header (car (get-keyword-contents schema-contents '(:header))))
    (push (list :participants (remove 'set-of (remove predicate (butlast (flatten header) 2)))) schema)
    (push (list :vars (remove-duplicates (remove-if-not #'variable? (flatten schema-contents)))) schema)
    (push (list :contents schema-contents) schema)

    ; schema sections
    (push (list :header header) schema)
    (dolist (section '(:types :rigid-conds :static-conds :preconds :postconds :goals
                       :episodes :episode-relations :obligations :necessities :certainties))
      (let ((section-contents (car (get-keyword-contents schema-contents (list section)))))
        (when section-contents
          (push (list section section-contents) schema))))

    (setf (gethash predicate *schemas*)
      (apply #'make-epi-schema (apply #'append schema)))
)) ; END store-epi-schema



(defstruct (obj-schema (:include schema))
;``````````````````````````````````````````````
; contains the following fields:
; types              : the nominal types of each participant/variable
; rigid-conds        : non-fluent conditions relevant to object
; skeletal-prototype : 3D mesh decomposition of object (.obj filenames)
; purposes           : telic purpose associated with object
; necessities        : probabilities associated with schema formulas
;
  types
  rigid-conds
  skeletal-prototype
  purposes
  necessities
) ; END defstruct obj-schema



(defun deepcopy-obj-schema (old &key keep-id)
;```````````````````````````````````````````````
; Deep copy an object schema structure
;
  (let (new)
    (if keep-id
      (setq new (make-obj-schema :id (schema-id old)))
      (setq new (make-obj-schema)))
    (setf (obj-schema-predicate new) (copy-tree (obj-schema-predicate old)))
    (setf (obj-schema-participants new) (copy-tree (obj-schema-participants old)))
    (setf (obj-schema-vars new) (copy-tree (obj-schema-vars old)))
    (setf (obj-schema-bindings new) (deepcopy-hash-table (obj-schema-bindings old)))
    (setf (obj-schema-contents new) (copy-tree (obj-schema-contents old)))
    (setf (obj-schema-header new) (copy-tree (obj-schema-header old)))
    (setf (obj-schema-types new) (copy-tree (obj-schema-types old)))
    (setf (obj-schema-rigid-conds new) (copy-tree (obj-schema-rigid-conds old)))
    (setf (obj-schema-skeletal-prototype new) (copy-tree (obj-schema-skeletal-prototype old)))
    (setf (obj-schema-purposes new) (copy-tree (obj-schema-purposes old)))
    (setf (obj-schema-necessities new) (copy-tree (obj-schema-necessities old)))
    new
)) ; END deepcopy-obj-schema



(defun store-obj-schema (predicate schema-contents)
;````````````````````````````````````````````````````
; Given a predicate for an obj-schema and the schema (as a list of sections),
; create a new schema structure and store it in the global schema hash table,
; with the predicate as a key.
;
  (let (schema header participants vars)
    (push (list :predicate predicate) schema)
    (setq header (car (get-keyword-contents schema-contents '(:header))))
    (push (list :participants (remove 'set-of (remove predicate (flatten header)))) schema)
    (push (list :vars (remove-duplicates (remove-if-not #'variable? (flatten schema-contents)))) schema)
    (push (list :contents schema-contents) schema)

    ; schema sections
    (push (list :header header) schema)
    (dolist (section '(:types :rigid-conds :skeletal-prototype :purposes :necessities))
      (let ((section-contents (car (get-keyword-contents schema-contents (list section)))))
        (when section-contents
          (push (list section section-contents) schema))))

    (setf (gethash predicate *schemas*)
       (apply #'make-obj-schema (apply #'append schema)))
)) ; END store-obj-schema



(defun store-schema (predicate schema-contents)
;````````````````````````````````````````````````
; Stores a schema as either an epi-schema or obj-schema.
;
  (cond
    ((equal (car schema-contents) 'dial-schema)
      (store-dial-schema predicate schema-contents))
    ((member (car schema-contents) '(event-schema epi-schema))
      (store-epi-schema predicate schema-contents))
    ((equal (car schema-contents) 'obj-schema)
      (store-obj-schema predicate schema-contents))
    (t (format t "~% *** error: schema for ~a must begin with either epi-schema or obj-schema ~%" predicate)))
) ; END store-schema



(defun deepcopy-schema (schema &key keep-id)
;`````````````````````````````````````````````
; Deep copy a schema structure.
;
  (cond
    ((dial-schema-p schema) (deepcopy-dial-schema schema :keep-id keep-id))
    ((epi-schema-p schema) (deepcopy-epi-schema schema :keep-id keep-id))
    ((obj-schema-p schema) (deepcopy-obj-schema schema :keep-id keep-id)))
) ; END deepcopy-schema



(defun print-schema-predicates (&key surface-english)
;`````````````````````````````````````````````````````
; Prints all of the stored schema predicates (if :surface-english t
; is given, convert from ULF to words)).
;
  (maphash (lambda (k v) (format t "~a~%"
      (if surface-english
        (string-downcase (str-replace (string (car (sym-split k 2))) "-" " "))
        (string-downcase (string k)))))
    *schemas*)
) ; END print-schema-predicates



(defun schema-predicate? (x)
;```````````````````````````
; Predicate which returns true if x is a stored schema predicate, e.g. 'discuss-food.v'.
;
  (gethash x *schemas*)
) ; END schema-predicate?



(defun get-stored-schema (predicate)
;````````````````````````````````````
; Gets a schema structure given a predicate key.
;
  (gethash predicate *schemas*)
) ; END get-stored-schema



(defun get-schemas-of-type (type)
;```````````````````````````````````
; Gets all stored schemas of a particular type.
;
  (let (schemas)
    (maphash (lambda (predicate schema)
        (when (or
            (and (equal type 'dial-schema) (dial-schema-p schema))
            (and (equal type 'epi-schema)  (epi-schema-p schema))
            (and (equal type 'obj-schema)  (obj-schema-p schema)))
          (push schema schemas)))
      *schemas*)
    schemas
)) ; END get-schemas-of-type



(defun bind-variable-in-schema (schema var val)
;``````````````````````````````````````````````````
; Binds a variable in a schema by adding it to the binding hash table.
;
  (setf (gethash var (schema-bindings schema)) val)
) ; END bind-variable-in-schema



(defun bind-schema-args (args schema) 
;```````````````````````````````````````
; Substitute the successive arguments in the 'args' list for successive
; variables occurring in the schema or plan header exclusive of the 
; episode variable characterized by the header predication (for 
; episodic headers).
;
; Generally, 'args' should correspond to the variables in the participants
; list of the schema, but we allow for the possibility of ^me and ^you as
; implicit arguments if fewer arguments than variables are given - in which
; case, they're added to the arguments list in that order.
;
; On the other hand, if more arguments than variables are given, we assume that
; ^me and ^you might be provided as redundant arguments, and remove those from
; the front of the list if present. Otherwise, we remove superfluous arguments
; starting from the end of the list.
;
  (let (participants vars)
    (setq participants (schema-participants schema))
    (setq vars (remove-if-not #'variable? participants))

    ; Return with warning if no variables in participants list
    (when (null vars)
      (format t "@@@ Warning: Attempt to substitute values~%    ~a~%    in participants ~a, which has no variables~%"
                args participants)
      (return-from bind-schema-args schema))

    (cond
      ; Case 1: More args than variables
      ((> (length args) (length vars))
        (format t "@@@ Warning: More values supplied, viz.,~%    ~a,~%    than participants ~a has variables~%"
                  args participants)
        ; If ^me or ^you are already in participants of the schema, remove them from the args list
        (if (member '^me participants) (setq args (remove '^me args)))
        (if (member '^you participants) (setq args (remove '^you args)))
        ; Otherwise remove superfluous arguments from end of list
        (setq args (butlast args (- (length args) (length vars))))
        (format t "@@@ Now using args: ~a~%" args))

      ; Case 2: Fewer args than variables
      ((< (length args) (length vars))
        (format t "@@@ Warning: Fewer values supplied, viz.,~%    ~a,~%    than participants ~a has variables~%"
                  args participants)
        ; Assume first two missing args are ^me and ^you if they don't appear in the header
        (if (and (>= (- (length vars) (length args)) 2) (not (member '^you participants)))
          (setq args (cons '^you args)))
        (if (not (member '^me participants))
          (setq args (cons '^me args)))
        (setq vars (butlast vars (- (length vars) (length args))))
        (format t "@@@ Now using args: ~a, for vars: ~a~%" args vars)))
            
    ; Length of 'args' and 'vars' are equal (or have just been equalized)
    (mapcar (lambda (arg var)
        ; Bind each variable to its argument in the schema, and also substitute in participants list
        (bind-variable-in-schema schema var arg)
        (setf (schema-participants schema) (subst arg var (schema-participants schema))))
      args vars)

    schema
)) ; END bind-schema-args



(defun subst-binding-expr (expr schema)
;````````````````````````````````````````
; Applies the variable bindings of a given schema to the given expression.
;
  (maphash (lambda (var val)
        (setq expr (subst val var expr)))
      (schema-bindings schema))
  expr
) ; END subst-binding-expr



(defun get-schema-contents (schema &key no-binding)
;`````````````````````````````````````````````````````
; Gets the contents of a schema (substituting any bindings).
; (if :no-binding t is given, do not substitute bindings)
;
  (if no-binding
    (schema-contents schema)
    (subst-binding-expr (schema-contents schema) schema))
) ; END get-schema-contents



(defun get-schema-section (schema section &key no-binding)
;````````````````````````````````````````````````````````````````````````
; Gets a section of a schema by the given keyword (substituting any bindings).
; (if :no-binding t is given, do not substitute bindings)
;
  (let (type)
    (setq type (cond
      ((dial-schema-p schema) 'dial)
      ((epi-schema-p schema) 'epi)
      ((obj-schema-p schema) 'obj)))
    (if no-binding
      (funcall (sym-join (list type 'schema section) #\-) schema)
      (subst-binding-expr (funcall (sym-join (list type 'schema section) #\-) schema) schema))
)) ; END get-schema-section



(defun get-schema-section-wffs (schema section &key no-binding)
;``````````````````````````````````````````````````````````````````````````````
; Gets all wffs in a section of a schema by the given keyword (substituting any bindings).
; (if :no-binding t is given, do not substitute bindings)
;
  (mapcar #'second (group-facts-in-schema-section
    (get-schema-section schema section :no-binding no-binding)))
) ; END get-schema-section-wffs



(defun get-schema-sections-wffs (schema sections &key no-binding)
;````````````````````````````````````````````````````````````````````````````````
; Gets and appends all wffs in a list of sections of a schema.
;
  (apply #'append (mapcar (lambda (section)
      (get-schema-section-wffs schema section :no-binding no-binding))
    sections))
) ; END get-schema-sections-wffs



(defun instantiate-dial-schema (schema args)
;```````````````````````````````````````````
; Instantiates a general dial-schema given a list of arguments corresponding to the
; variables in the schema header. This creates a copy of the general schema with
; specific variable bindings. It also adds certain inferences to the dialogue context,
; specifically those from the :types and :rigid-conds within the schema.
;
; TODO: the addition of facts in the instantiated schema to context needs to be
; considered more systematically.
;
; TODO: should ?e be instantiated and (<schema-header> ** E1) be
; stored in context at this point?
;
  (let ((schema-instance (deepcopy-dial-schema schema)))
    (bind-schema-args args schema-instance)

    (instantiate-dial-schema-types schema)
    (instantiate-dial-schema-rigid-conds schema)

    schema-instance
)) ; END instantiate-dial-schema



(defun instantiate-dial-schema-types (schema)
;`````````````````````````````````````````````
; Adds the instantiated types within a schema to context.
; If the subject of a type predication is a variable, we create a skolem constant for that
; individual, as well as 'observing' context to find a suitable object to identify with that
; skolem constant, if any.
; TODO: once more general schema matching is implemented, these variable bindings should
; be done upon matching the :types in a schema to corresponding formulas in context.
; USES DS
;
  (let (name wff var)
    (dolist (pair (group-facts-in-schema-section (get-schema-section schema :types)))
      (setq name (first pair))
      (setq wff (second pair))
      ; If typed variable, find value for variable through observation and
      ; substitute in both type and in contents of each schema section.
      (when (variable? (car wff))
        (setq var (car wff))
        ; Get skolem name, and bind in schema
        (setq sk-name (observe-variable-type var (second wff)))
        (bind-variable-in-schema schema var sk-name)
        (setq wff (subst sk-name var wff)))
      ; Store type as fact in context.
      (store-in-context wff))
)) ; END instantiate-dial-schema-types



(defun instantiate-dial-schema-rigid-conds (schema)
;``````````````````````````````````````````````````````
; Adds the instantiated rigid-conds within a schema to context.
; USES DS
;
  (let (name wff)
    (dolist (pair (group-facts-in-schema-section (get-schema-section schema :rigid-conds)))
      (setq name (first pair))
      (setq wff (second pair))
      (store-in-context wff))
)) ; END instantiate-dial-schema-rigid-conds



(defun group-facts-in-schema-section (contents)
;`````````````````````````````````````````````````
; Groups contents of a schema section, assumed to be a series of
; declarations of the following form:
; <name> <wff>
; into a list of (name wff) pairs. Here, name is assumed to be
; a variable of the form ?x or !x.
;
  (cond
    ((null contents) nil)
    (t (cons (list (first contents) (second contents))
             (group-facts-in-schema-section (cddr contents)))))
) ; END group-facts-in-schema-section



(defun print-schema (schema &key bindings)
;```````````````````````````````````````````
; Prints a given schema (with any variable substitutions made).
;
  (format t "~% ----------- ~a: ----------~%" (schema-id schema))
  (format t " >> predicate: ~a~%" (schema-predicate schema))
  (format t " >> participants: ~a~%" (schema-participants schema))
  (when bindings
    (format t " >> bindings:~%")
    (maphash (lambda (k v)
        (format t "     * ~a : ~a~%" k v))
      (schema-bindings schema)))
  (format t " >> contents:~%")
  (format t "~s~%" (get-schema-contents schema))
  (format t " -----------------------------------~%")
) ; END print-schema



(defun print-schema-instances (schema-instances &key bindings)
;````````````````````````````````````````````````````````````````
; Prints all schema instances (given a hash table).
;
  (maphash (lambda (schema-id schema)
    (print-schema schema :bindings bindings)) schema-instances)
) ; END print-schema-instances