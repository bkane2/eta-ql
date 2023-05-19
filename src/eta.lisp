;; July 10/19 
;; ===========================================================
;;
;; TODO REFACTOR : update this
;; For inputs, we use the question it answers to create a list
;; of simple, explicit English clauses, especially the first of 
;; which is intended to capture the "gist" of what was said,
;; i.e., the content of an utterance most likely to be needed
;; to understand the next turn in the dialogue. The intent is that
;; logical interpretations will later play that role, and this
;; has been initiated by supplying a hash table of (some) Eta 
;; output interpretations,
;;     *output-semantics*
;;     *output-gist-clauses*
;; These tables can be used to set up
;; the 'interpretation' and 'output-gist-clauses' properties of
;; action proposition names, generated in forming plans from 
;; schemas.
;;
;; One important goal in setting up these tables is to be able
;; later to match certain user inputs to Eta question gists/
;; interpretations, to see if the inputs already answer the
;; questions, making them redundant. 
;;
;; TODO: Regarding coreference and memory, it seems like there are
;; a couple separate things:
;; 1. Eta needs a way to parameterize say-to.v actions (and the corresponding
;; gist clauses) based on previous user answers. For example, if Eta asks "what
;; was your favorite class?" and the user replies "Macroeconomics", instead of the
;; next question being "did you find your favorite class hard", it should be
;; "did you find Macroeconomics hard?"
;; 2. Eta needs a way to "trigger" bringing up past information in response to
;; a user question, perhaps based on some similarity metric
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- Common-Lisp -*-

; [This is partly derivative from "doolittle", an improvement of
; Weizenbaum's ELIZA that carries information forward from the
; previous question/answer pair, makes greater use of features,
; uses more flexible, hierarchical pattern matching, and initially
; classifies inputs by their general form (instead of by keyword).]
	
; To run the program, do the following (while in the present
; eta directory):
; lisp
; (load "start")

(in-package :eta)


(setf *print-circle* t) ; needed to prevent recursive loop when printing plan-step

(defstruct agent-config
;```````````````````````````````
; contains the following fields:
; avatar                : specify the name of one of the available avatars to use
; avatar-name           : the full name of the avatar to use
; perception-servers    : A list of perception subsystems registered with Eta.
;                         Supported: |Audio|, |Terminal|, |Blocks-World-System|
; specialist-servers    : A list of specialist subsystems registered with Eta.
;                         Supported: |Spatial-Reasoning-System|
; emotion-tags          : T to allow insertion of emotion tags (e.g., [SAD]) at beginning
;                              of outputs. If no emotion tag is explicitly specified in the output,
;                              a default [NEUTRAL] tag will be prepended.
;                         NIL to disable emotion tags. Any tags at the beginning of :out
;                              directives will be stripped.
; generation-mode       : The method to use for generating responses at system reaction schema steps:
;                         GPT3: uses GPT-3 to generate a response using a prompt created automatically from
;                               the current schema, relevant knowledge, and sentence to paraphrase (if any).
;                               Note that rule-based methods can still be used on top of GPT3.
;                         RULE (default): uses pattern transduction trees to select reactions/responses.
; interpretation-mode   : The method to use for interpreting user utterances as gist clauses:
;                         GPT3: uses GPT-3 to rephrase utterances as gist clauses given context,
;                               using a multi-example prompt.
;                               Note that rule-based methods can still be used on top of GPT3; GPT3 will
;                               only be used as a fallback if no gist clause is found using rule-based methods.
;                         RULE (default): uses pattern transduction trees to extract gist clauses given context.
; parser-mode           : The method to use for parsing gist clauses into ULFs:
;                         BLLIP: uses the ULF parser created by Len Schubert based on the Charniak BLLIP parser. Requires
;                                both the :lenulf and :standardize-ulf packages to be included in the dependencies, and for
;                                BLLIP to be installed locally, with the correct path specified in the :lenulf package.
; timegraph-mode        : T to use the timegraph; NIL otherwise.
;                         RULE (default): uses pattern transduction trees to parse gist clauses into ULF.
; session-number        : The number session to load (a session-number of 1 corresponds to the files in the day1 directory
;                         of an avatar) in a multi-session dialogue.
;
  (avatar "lissa-gpt")
  (avatar-name "Eta")
  (perception-servers '(|Audio| |Terminal|))
  (specialist-servers '())
  (emotion-tags NIL)
  (model-names '(("information-retrieval" ("sentence-transformer" "sentence-transformers/all-distilroberta-v1" :api t))))
  (generation-mode 'GPT3)
  (interpretation-mode 'RULE)
  (parser-mode 'RULE)
  (timegraph-mode NIL)
  (session-number 1)
) ; END defstruct agent-config




;`````````````````````````````````````````````
; Accessor functions for agent-config
;

(defun get-perception-servers (config-agent)
  (agent-config-perception-servers config-agent)
) ; END get-perception-servers

(defun get-specialist-servers (config-agent)
  (agent-config-specialist-servers config-agent)
) ; END get-specialist-servers

(defun get-model-names (config-agent)
  (agent-config-model-names config-agent)
) ; END get-model-names

(defun is-emotion-mode (config-agent)
  (agent-config-emotion-tags config-agent)
) ; END is-emotion-mode

(defun get-generation-mode (config-agent)
  (agent-config-generation-mode config-agent)
) ; END get-generation-mode

(defun get-interpretation-mode (config-agent)
  (agent-config-interpretation-mode config-agent)
) ; END get-interpretation-mode

(defun get-parser-mode (config-agent)
  (agent-config-parser-mode config-agent)
) ; END get-parser-mode

(defun get-timegraph-mode (config-agent)
  (agent-config-timegraph-mode config-agent)
) ; END get-timegraph-mode

(defun set-generation-mode (config-agent mode)
  (setf (agent-config-generation-mode config-agent) mode)
) ; END set-generation-mode

(defun set-interpretation-mode (config-agent mode)
  (setf (agent-config-interpretation-mode config-agent) mode)
) ; END set-interpretation-mode

(defun set-parser-mode (config-agent mode)
  (setf (agent-config-parser-mode config-agent) mode)
) ; END set-parser-mode

(defun set-timegraph-mode (config-agent mode)
  (setf (agent-config-timegraph-mode config-agent) mode)
) ; END set-timegraph-mode





(defstruct user-config
;```````````````````````````````
; contains the following fields:
; user-id        : a unique ID for the user
; user-name      : the name of the user
; start-schema   : the dialogue schema predicate to use to begin dialogue
; use-embeddings : a temporary option used for experiments; controls whether
;                  or not embedding-based knowledge retrieval is used
;
  (user-id "_test")
  (user-name "John Doe")
  (start-schema 'have-eta-dialog.v)
  (use-embeddings t)
) ; END defstruct user-config





(defstruct session
;```````````````````
; contains the following fields:
; id                 : a unique ID for this session
; config-agent       : a copy of the agent config struct
; config-user        : a copy of the user config struct
; ds                 : the dialogue state of this session
; io-path            : the io-path of this session
; ^me                : the value for the indexical variable ^me
; ^you               : the value for the indexical variable ^you
; output-count       : used for keeping track of output number in output.txt
; output-buffer      : used for maintaining a buffer of consecutive Eta
;                      outputs so that they can be combined and written
;                      as a single turn to turn-output.txt
; step-failure-timer : used to keep track of elapsed time for determining whether
;                      a step in the plan has failed
; quit-conversation  : a true/false flag used to determine when this session
;                      should be closed
;
  (id (gentemp "SESSION"))
  config-agent
  config-user
  ds
  io-path
  ^me
  ^you
  output-count
  output-buffer
  step-failure-timer
  quit-conversation
) ; END defstruct session





;`````````````````````````````````````````````
; Accessor functions for session (uses list of
; sessions stored in global *sessions* var).
;

(defun get-io-path (&optional fname)
  (if *sessions* (concatenate 'string (session-io-path (car *sessions*)) fname))
) ; END get-io-path

(defun get-user-id ()
  (if *sessions* (user-config-user-id (session-config-user (car *sessions*))))
) ; END get-user-id

(defun get-ds ()
  (if *sessions* (session-ds (car *sessions*)))
) ; END get-ds

(defun get-^me ()
  (if *sessions* (session-^me (car *sessions*)))
) ; END get-^me

(defun get-^you ()
  (if *sessions* (session-^you (car *sessions*)))
) ; END get-^you

(defun get-output-count ()
  (if *sessions* (session-output-count (car *sessions*)))
) ; END get-output-count

(defun get-output-buffer ()
  (if *sessions* (session-output-buffer (car *sessions*)))
) ; END get-output-buffer

(defun get-step-failure-timer ()
  (if *sessions* (session-step-failure-timer (car *sessions*)))
) ; END get-step-failure-timer

(defun get-quit-conversation ()
  (if *sessions* (session-quit-conversation (car *sessions*)))
) ; END get-quit-conversation

(defun get-use-embeddings ()
  (if *sessions* (user-config-use-embeddings (session-config-user (car *sessions*))))
) ; END get-use-embeddings

(defun has-plan ()
  (if *sessions* (ds-curr-plan (session-ds (car *sessions*))))
) ; END has-plan

(defun set-output-count (count)
  (if *sessions* (setf (session-output-count (car *sessions*)) count))
) ; END set-output-count

(defun set-output-buffer (buffer)
  (if *sessions* (setf (session-output-buffer (car *sessions*)) buffer))
) ; END set-output-buffer

(defun push-output-buffer (x)
  (if *sessions* (push x (session-output-buffer (car *sessions*))))
) ; END push-output-buffer

(defun set-step-failure-timer (time)
  (if *sessions* (setf (session-step-failure-timer (car *sessions*)) time))
) ; END set-step-failure-timer

(defun set-quit-conversation (quit-conversation)
  (if *sessions* (setf (session-quit-conversation (car *sessions*)) quit-conversation))
) ; END set-quit-conversation





(defun init-session (config-agent config-user)
;```````````````````````````````````````````````
; Initializes a dialogue session given user configuration
;
  (let ((session (make-session)) start-schema)
    (setf (session-config-agent session) config-agent)
    (setf (session-config-user session) config-user)
    (setf (session-ds session) (init-ds))
    (setf (session-io-path session)
      (concatenate 'string *io-dir* (agent-config-avatar config-agent) "/" (user-config-user-id config-user) "/"))
    (setf (session-^me session) (intern (agent-config-avatar-name config-agent)))
    (setf (session-^you session) (intern (user-config-user-name config-user)))
    (setf (session-output-count session) 0)
    (setf (session-output-buffer session) nil)
    (setf (session-step-failure-timer session) (get-universal-time))
    (setf (session-quit-conversation session) nil)

    ; Add any pre-defined aliases and concept sets to session
    (when (boundp '*concept-aliases*)
      (mapcar (lambda (alias)
          (store-aliases-of-concept (first alias) (second alias) (third alias) (session-ds session)))
        *concept-aliases*))
    (when (boundp '*concept-sets*)
      (mapcar (lambda (set)
          (store-concept-set (first set) (second set) (third set) (session-ds session)))
        *concept-sets*))

    ; Add any initial knowledge to session
    (when (boundp '*init-knowledge*)
      (mapcar (lambda (fact) (store-in-kb fact (session-ds session))) *init-knowledge*))

    ; Create plan from initial schema (if doesn't exist, set quit-conversation to t)
    (setq start-schema (user-config-start-schema config-user))
    (when (null (get-stored-schema start-schema))
      (format t "***  start schema for session ~a not found. Quitting conversation.~%" (user-config-user-id config-user))
      (setf (session-quit-conversation session) t)
      (return-from init-session session))
    (setf (ds-curr-plan (session-ds session)) (plan-subschema start-schema nil (session-ds session)))

    ;; (print-plan-status (ds-curr-plan (session-ds session))) ; DEBUGGING

    session
)) ; END init-session





(defstruct ds
;```````````````````````````````
; contains the following fields:
; task-queue       : a list of time-sharing tasks to repeatedly execute in cycles
; curr-plan        : points to the currently due step in the 'surface' dialogue plan (an ordered list of plan nodes)
; schema-instances : hash table containing all schema instances, keyed on their unique IDs
; plan-var-table   : hash table associating each var-name in the plan structure with a list of (var-name, schema-id) pairs
; buffers          : a structure containing buffers (importance-ranked priority queues) for items that the dialogue
;                    system must process during a corresponding task (perceptions, inferences, intentions, etc.)
; reference-list   : contains a list of discourse entities to be used in ULF coref
; equality-sets    : hash table containing a list of aliases, keyed by canonical name
; gist-kb-user     : hash table of all gist clauses + associated topic keys from user
; gist-kb-eta      : hash table of all gist clauses + associated topic keys from Eta
; conversation-log : a list containing dialogue-turn structures for each chronological turn in the conversation
; context          : hash table of facts that are true at Now*, e.g., <wff3>
; memory           : hash table of atemporal/"long-term" facts, e.g., (<wff3> ** E3) and (Now* during E3)
; kb               : hash table of Eta's episodic knowledge base, containing general episodic facts
; tg               : timegraph of all episodes
; time             : the constant denoting the current time period. NOW0 is taken uniquely to refer
;                    to the beginning, with all moves/etc. occurring at subsequent times
; count            : number of Eta outputs generated so far (maintained for latency enforcement, i.e.,
;                    not repeating a previously used response too soon)
; time-last-used   : a hash table of last-used times for each visited rule node (for latency enforcement).
;
; TODO: currently reference-list and equality-sets are separate things, though they serve a similar
; purpose, namely keeping track of which entities co-refer (e.g. skolem variable and noun phrase).
; These should eventually become unified once we have a systemic way of doing coreference over all
; propositions, versus the current module built over the ULF interpretation.
;
  task-queue
  curr-plan
  schema-instances
  plan-var-table
  buffers
  reference-list
  equality-sets
  gist-kb-user
  gist-kb-eta
  conversation-log
  context
  memory
  kb
  tg
  time
  count
  time-last-used
) ; END defstruct ds





;`````````````````````````````````````
; Accessor functions for ds
;

(defun get-time-last-used (ds rule-node)
  (when (null (gethash rule-node (ds-time-last-used ds)))
    (set-time-last-used ds rule-node -10000))
  (gethash rule-node (ds-time-last-used ds))
) ; END get-time-last-used

(defun set-time-last-used (ds rule-node time)
  (setf (gethash rule-node (ds-time-last-used ds)) time)
) ; END set-time-last-used





(defun init-ds () ; {@}
;``````````````````````
; Initializes a dialogue state record structure with any special
; properties of the dialogue state (e.g. hash tables, task queues, etc.)
;
  (let ((ds (make-ds)))
    ; Initialize task queue
    (refill-task-queue ds)

    ; Initialize schema instances hash table
    (setf (ds-schema-instances ds) (make-hash-table))

    ; Initialize plan var table
    (setf (ds-plan-var-table ds) (make-hash-table))

    ; Initialize hash table for aliases/equality sets
    (setf (ds-equality-sets ds) (make-hash-table :test #'equal))

    ; Initialize hash tables for User and Eta topic keys/gist clauses
    (setf (ds-gist-kb-user ds) (make-hash-table :test #'equal))
    (setf (ds-gist-kb-eta ds) (make-hash-table :test #'equal))

    ; Initialize buffers
    (setf (ds-buffers ds) (make-buffers))

    ; Initialize conversation log
    (setf (ds-conversation-log ds) nil)

    ; Initialize fact hash tables
    (setf (ds-context ds) (make-hash-table :test #'equal))
    (setf (ds-memory ds) (make-hash-table :test #'equal))
    (setf (ds-kb ds) (make-hash-table :test #'equal))

    ; Initialize timegraph
    (when (get-timegraph-mode)
      (construct-timegraph ds))

    ; Initialize time
    (setf (ds-time ds) 'NOW0)
    (store-time ds)
    (update-time ds)

    ; Initialize count
    (setf (ds-count ds) 0)

    ; Initialize time-last-used hash table
    (setf (ds-time-last-used ds) (make-hash-table :test #'equal))

    ds
)) ; END init-ds





(defun deepcopy-ds (old) ; {@}
;```````````````````````````````````````````
; Deep copy a dialogue state
;
  (let ((new (make-ds)))
    (setf (ds-task-queue new) (copy-tree (ds-task-queue old)))
    (setf (ds-curr-plan new) (deepcopy-plan-node (ds-curr-plan old)))
    (setf (ds-schema-instances new) (make-hash-table))
    (maphash (lambda (schema-id schema)
        (setf (gethash schema-id (ds-schema-instances new)) (deepcopy-schema schema :keep-id t)))
      (ds-schema-instances old))
    (setf (ds-plan-var-table new) (deepcopy-hash-table (ds-plan-var-table old)))
    (setf (ds-buffers new) (deepcopy-buffers (ds-buffers old)))
    (setf (ds-reference-list new) (copy-tree (ds-reference-list old)))
    (setf (ds-equality-sets new) (deepcopy-hash-table (ds-equality-sets old)))
    (setf (ds-gist-kb-user new) (deepcopy-hash-table (ds-gist-kb-user old)))
    (setf (ds-gist-kb-eta new) (deepcopy-hash-table (ds-gist-kb-eta old)))
    (setf (ds-conversation-log new) (mapcar #'deepcopy-dialogue-turn (ds-conversation-log old)))
    (setf (ds-context new) (deepcopy-hash-table (ds-context old)))
    (setf (ds-memory new) (deepcopy-hash-table (ds-memory old)))
    (setf (ds-kb new) (deepcopy-hash-table (ds-kb old)))
    ; TODO: need to modify below line to use deepcopy function from tg package
    ;; (setf (ds-tg new) (deepcopy-hash-table (ds-tg old)))
    (setf (ds-time new) (copy-tree (ds-time old)))
    (setf (ds-count new) (copy-tree (ds-count old)))

    new
)) ; END deepcopy-ds





(defstruct buffers
;```````````````````````````````
; contains the following fields:
; perceptions : an importance-ranked priority queue of perceptions for the system to interpret
; gists       : an importance-ranked priority queue of gist clauses for the system to semantically interpret
; semantics   : an importance-ranked priority queue of semantic interpretations for the system to pragmatically interpret
; pragmatics  : an importance-ranked priority queue of pragmatic interpretations for the system to generate inferences from
; inferences  : an importance-ranked priority queue of inferences for the system to generate further inferences from (until
;               some depth/importance threshold is reached)
; actions     : an importance-ranked priority queue of possible actions under consideration to add to the plan
; 
  (perceptions (priority-queue:make-pqueue #'>))
  (gists (priority-queue:make-pqueue #'>))
  (semantics (priority-queue:make-pqueue #'>))
  (pragmatics (priority-queue:make-pqueue #'>))
  (inferences (priority-queue:make-pqueue #'>))
  (actions (priority-queue:make-pqueue #'>))
) ; END defstruct buffers





(defun deepcopy-buffers (old) ; {@}
;``````````````````````````````````
; Deep copy a buffers structure
;
  (let ((new (make-buffers)))
    (setf (buffers-perceptions new) (deepcopy-buffer (buffers-perceptions old)))
    (setf (buffers-gists new) (deepcopy-buffer (buffers-gists old)))
    (setf (buffers-semantics new) (deepcopy-buffer (buffers-semantics old)))
    (setf (buffers-pragmatics new) (deepcopy-buffer (buffers-pragmatics old)))
    (setf (buffers-inferences new) (deepcopy-buffer (buffers-inferences old)))
    (setf (buffers-actions new) (deepcopy-buffer (buffers-actions old)))
  new
)) ; END deepcopy-buffers





(defstruct dialogue-turn
;```````````````````````````````
; contains the following fields:
; agent: the agent whose turn is represented
; utterance: the surface utterance constituting the turn
; gists: a list of gist clauses for the turn
; semantics: a list of semantic interpretations (ULF) of the turn
; pragmatics: a list of pragmatic interpretations (ULF) of the turn
; obligations: a list of dialogue obligations associated with the turn
; episode-name: the episode name associated with the turn
; 
  agent
  utterance
  gists
  semantics
  pragmatics
  obligations
  episode-name
) ; END defstruct dialogue-turn





(defun deepcopy-dialogue-turn (old) ; {@}
;``````````````````````````````````````
; Deep copy a dialogue turn structure
;
  (let ((new (make-dialogue-turn)))
    (setf (dialogue-turn-agent new) (copy-tree (dialogue-turn-agent old)))
    (setf (dialogue-turn-utterance new) (copy-tree (dialogue-turn-utterance old)))
    (setf (dialogue-turn-gists new) (copy-tree (dialogue-turn-gists old)))
    (setf (dialogue-turn-semantics new) (copy-tree (dialogue-turn-semantics old)))
    (setf (dialogue-turn-pragmatics new) (copy-tree (dialogue-turn-pragmatics old)))
    (setf (dialogue-turn-obligations new) (copy-tree (dialogue-turn-obligations old)))
    (setf (dialogue-turn-episode-name new) (copy-tree (dialogue-turn-episode-name old)))
  new
)) ; END deepcopy-dialogue-turn





(defun init () ; {@}
;`````````````````````````````
; Initialize global parameters
;
  ; Use response inhibition via latency numbers when *use-latency* = T
  (defvar *use-latency* t)

  ; Queue of tasks that Eta must perform
  (defvar *tasks-list* '(
    execute-curr-step
    perceive-world
    interpret-perceptions
    suggest-possible-actions-from-input
    infer-facts-top-down
    infer-facts-bottom-up
    add-possible-actions-to-plan
    merge-equivalent-plan-steps
    reorder-conflicting-plan-step
    expand-abstract-plan-steps
    evict-buffers
  ))

  (defvar *io-dir* "./io/")
  (defparameter *embedding-path* (concatenate 'string *io-dir* "/" (agent-config-avatar *config-agent*) "/embeddings/"))

  (create-process-io-files)

  ; Coreference mode
  ; 0 : simply reconstruct the original ulf
  ; 1 : mode 2 but excluding i.pro and you.pro from resolved references
  ; 2 : substitute most specific referents only for anaphora and indexical np's (e.g. that block)
  ; 3 : substitute most specific referents for all references
  (defparameter *coreference-mode* 1)

  ; Recency cutoff used when attempting coreference (i.e. the coreference
  ; module will only look this far back, in terms of turns, in the discourse
  ; history to find possible referents).
  (defparameter *recency-cutoff* 2)

  ; Certainty cutoff used to generate responses given a list of relations+certainties from the blocks world
  (defparameter *certainty-threshold* 0.7)

  ; A list of supported speech acts
  (defparameter *speech-acts* '(say-to.v paraphrase-to.v reply-to.v react-to.v))

  ; The certainty of an episode determines the timer period (in seconds) that must be
  ; passed for Eta to consider an expected episode failed and move on in the plan.
  ; This is a function on the certainty of the episode, with a certainty of 1 having
  ; an infinite period, and a certainty of 0 having a period of 0. This constant determines
  ; the coefficient on the certainty-to-period function.
  ; Currently, this coefficient makes a certainty of ~0.632 correspond to 30 seconds.
  (defparameter *expected-step-failure-period-coefficient* 30)

  ; If *emotions* is T, Eta will allow use of emotion tags at beginning of outputs.
  ;; (defparameter *emotions* nil)
  (defparameter *emotions-list* '([NEUTRAL] [SAD] [HAPPY] [WORRIED] [ANGRY]))

  ; Global variables used for IO
  (defparameter *input* nil)

  ; Precompute embeddings for any init-knowledge and epi-schemas
  (when (boundp '*init-knowledge*)
    (precompute-knowledge-embeddings *init-knowledge* *embedding-path*))
  (precompute-schema-embeddings (get-schemas-of-type 'epi-schema) *embedding-path*)

) ; END init











(defun validate-dependencies (response-generator gist-interpreter parser)
;```````````````````````````````````````````````````````````````````````````
; Validates that the dependencies given to Eta are correct, and initializes
; some packages to prevent latency upon first invocation.
;
  (setq parser
    (if (member parser '(BLLIP RULE)) parser 'RULE))
  (setq response-generator
    (if (member response-generator '(GPT3 RULE)) response-generator 'RULE))
  (setq gist-interpreter
    (if (member gist-interpreter '(GPT3 RULE)) gist-interpreter 'RULE))

  ; Check for API key if using gpt3-shell
  (when (or (equal response-generator 'GPT3) (equal gist-interpreter 'GPT3))
    (when (null (get-api-key "openai"))
      (format t "~% --- Warning: GPT3 generation/interpretation mode requires a valid")
      (format t "~%              OpenAI API key to be provided in config/keys/openai.txt.")
      (format t "~%              Changing generation/interpretation mode to RULE.~%")
      (setq response-generator 'RULE)
      (setq gist-interpreter 'RULE)))

  ; Initialize gpt3-shell (if in GPT3 generation/interpretation mode and valid API key exists)
  ; NOTE: currently unused because the API key is passed as an argument to the generation function.
  ;; (when (or (equal response-generator 'GPT3) (equal gist-interpreter 'GPT3))
  ;;   (gpt3-shell:init (get-api-key "openai")))

  ; Initialize information-retrieval (prevents delay on first invocation), and set
  ; correct model paths if custom model paths are given
  (dolist (model (get-model-names *config-agent*))
    (when (equal (car model) "information-retrieval")
      (dolist (submodel (cdr model))
        (when (equal '(:api t) (member :api submodel))
          (when (null (get-api-key "huggingface"))
            (format t "~% --- Warning: Using API for information-retrieval requires a valid")
            (format t "~%              HuggingFace API key to be provided in config/keys/huggingface.txt.~%"))
          (information-retrieval:set-api t))
        (cond
          ((equal (first submodel) "sentence-transformer")
            (information-retrieval:set-model (second submodel)))
          ((equal (first submodel) "cross-encoder")
            (information-retrieval:set-cross-encoder (second submodel)))))))
  (information-retrieval:init :api-key (get-api-key "huggingface"))

  ; Initialize ulf2english (prevents delay on first invocation)
  (ulf2english:ulf2english '(this.pro ((pres be.v) (= (a.d (test.n |ULF|.n))))))

  ; Initialize lenulf if among dependencies (prevents delay on first invocation)
  (when (equal parser 'BLLIP)
    (parse-str-to-ulf-bllip "This is a test sentence."))

  (set-generation-mode *config-agent* response-generator)
  (set-interpretation-mode *config-agent* gist-interpreter)
  (set-parser-mode *config-agent* parser)

) ; END validate-dependencies





(defun eta (agent-config)
;``````````````````````````````````````````````````````````````````````````````````````````````````````````
; Main program: Originally handled initial and final formalities,
; (now largely commented out) and controls the loop for producing,
; managing, and executing the dialog plan (mostly, reading & feature-
; annotating inputs & producing outputs, but with some subplan
; formation, gist clause formation, etc.).
;
  (defparameter *config-agent* agent-config)

  (validate-dependencies
    (get-generation-mode *config-agent*)
    (get-interpretation-mode *config-agent*)
    (get-parser-mode *config-agent*))

  (init)

  (defparameter *sessions* nil)
  (defparameter *sessions-dequeue* nil)
  (defparameter *sessions-closed* nil)

  (format t "~% ==== ETA READY TO START REGISTERING SESSIONS~%")

  ; Eta continues to monitor sessions indefinitely
  (loop while t do
    (cond
      ; If the session queue is non-empty, attempt to do a single pending task for
      ; that session, and dequeue. Remove the session if it is finished.
      (*sessions*
        (cond
          ((and (has-plan) (not (get-quit-conversation)))
            (do-task (select-and-remove-task))
            (push (car *sessions*) *sessions-dequeue*))
          ((get-output-buffer)
            (write-output-buffer (get-output-buffer) (get-io-path))
            (set-output-buffer nil)
            (write-closed-session (concatenate 'string *io-dir* "closed-sessions.txt") (get-user-id)))
          (t (write-closed-session (concatenate 'string *io-dir* "closed-sessions.txt") (get-user-id))))
        (setq *sessions* (cdr *sessions*)))
      ; Once the session queue is empty, move all dequeued sessions back to queue and
      ; add any newly registered sessions.
      (t
        (setq *sessions* (reverse *sessions-dequeue*))
        (setq *sessions-dequeue* nil)
        (listen-for-new-sessions))))
  
) ; END eta





(defun listen-for-new-sessions ()
;```````````````````````````````````
; Listens for new sessions by reading the paths of new config files from
; "/io/new-sessions.txt", creating user-config structs, and initiating a new
; session.
;
  (let (config-fnames config-user new-session)
    (setq config-fnames (read-new-sessions (concatenate 'string *io-dir* "new-sessions.txt")))
    (setq closed-sessions (read-closed-sessions (concatenate 'string *io-dir* "closed-sessions.txt")))
    (dolist (config-fname config-fnames)
      (setq config-user (apply #'make-user-config (read-config config-fname)))
      (cond
        ((member (user-config-user-id config-user) closed-sessions :test #'string-equal)
          (format t "~% == Session ~a already completed~%" (user-config-user-id config-user)))
        (t
          (setq new-session (init-session *config-agent* config-user))
          (create-session-io-files new-session)
          (format t "~% == Adding new session ~a~%" (user-config-user-id config-user))
          (push new-session *sessions*))))
)) ; END listen-for-new-sessions





(defun create-process-io-files ()
;````````````````````````````````
; Creates io directories and files necessary to execute Eta.
; 
  (ensure-directories-exist *io-dir*)
  (ensure-directories-exist (concatenate 'string *io-dir* (agent-config-avatar *config-agent*) "/"))
  (ensure-directories-exist *embedding-path*)
  (ensure-directories-exist (concatenate 'string *embedding-path* "schemas/"))
  (with-open-file (outfile (concatenate 'string *io-dir* "new-sessions.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (outfile (concatenate 'string *io-dir* "closed-sessions.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
) ; END create-process-io-files





(defun create-session-io-files (session)
;```````````````````````````````````````
; Creates fresh io directories and files for a new session.
; 
  (let ((path (session-io-path session))
        (perception-servers (get-perception-servers (session-config-agent session)))
        (specialist-servers (get-specialist-servers (session-config-agent session))))
    (ensure-directories-exist path)
    (ensure-directories-exist (concatenate 'string path "in/"))
    (ensure-directories-exist (concatenate 'string path "out/"))
    (ensure-directories-exist (concatenate 'string path "conversation-log/"))

    ; Ensure all standard input & output files for registered subsystems exist and are empty
    ; Note: input files only created for non-terminal systems,
    ;       output files are only created for non-terminal and non-audio systems
    (mapcar (lambda (system)
      (let ((fname-in (if (not (member system '(|Terminal|)))
                      (concatenate 'string path "in/" (string system) ".lisp")))
            (fname-out (if (not (member system '(|Terminal| |Audio|)))
                      (concatenate 'string path "out/" (string system) ".lisp"))))
        (if fname-in
        (with-open-file (outfile fname-in :direction :output :if-exists
                                          :supersede :if-does-not-exist :create)))
        (if fname-out
        (with-open-file (outfile fname-out :direction :output :if-exists
                                          :supersede :if-does-not-exist :create)))))
      (append perception-servers specialist-servers))

    ; Ensure that empty conversation log files exist
    (with-open-file (outfile (concatenate 'string path "conversation-log/" "text.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string path "conversation-log/" "text-readable.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string path "conversation-log/" "gist.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string path "conversation-log/" "semantic.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string path "conversation-log/" "pragmatic.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string path "conversation-log/" "obligations.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))

    ; Delete the content of output.txt, if it exists, otherwise create
    (with-open-file (outfile (concatenate 'string path "output.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    ; Delete the content of turn-output.txt and turn-emotion.txt, otherwise create
    (with-open-file (outfile (concatenate 'string path "turn-output.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string path "turn-emotion.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))      
)) ; END create-session-io-files





(defun refill-task-queue (ds) ; {@}
;```````````````````````````````````
; Refills the task queue after it becomes empty.
;
  (setf (ds-task-queue ds) *tasks-list*)
) ; END refill-task-queue





(defun select-and-remove-task () ; {@}
;```````````````````````````````````
; Select the task at the front of the task queue. Return the
; task and update the queue, refilling the queue when empty.
;
  ; When the task queue is empty, refill
  (when (null (ds-task-queue (get-ds)))
    (refill-task-queue (get-ds)))

  ; Pop the current front of the task queue
  (let ((curr-task (car (ds-task-queue (get-ds)))))
    (setf (ds-task-queue (get-ds)) (cdr (ds-task-queue (get-ds))))
    curr-task)
) ; END select-and-remove-task





(defun do-task (task) ; {@}
;````````````````````````
; Given a symbol denoting a task, call the top-level function
; implementing that task.
; NOTE: this can be made a bit more space-efficient just by using
; (funcall task), but I didn't want to assume that all tasks will
; necessarily share the same format.
;
  ;; (format t "executing task: ~a~%" task) ; DEBUGGING
  (case task
    (execute-curr-step (execute-curr-step))
    (perceive-world (perceive-world))
    (interpret-perceptions (interpret-perceptions))
    (suggest-possible-actions-from-input (suggest-possible-actions-from-input))
    (infer-facts-top-down (infer-facts-top-down))
    (infer-facts-bottom-up (infer-facts-bottom-up))
    (add-possible-actions-to-plan (add-possible-actions-to-plan))
    (merge-equivalent-plan-steps (merge-equivalent-plan-steps))
    (reorder-conflicting-plan-step (reorder-conflicting-plan-step))
    (expand-abstract-plan-steps (expand-abstract-plan-steps))
    (evict-buffers (evict-buffers))
  )

) ; END do-task





(defun advance-plan () ; {@}
;```````````````````````
; Advances the dialogue plan to the next step, or signals to
; quit the conversation if there are no further steps.
;
  (if (plan-node-next (ds-curr-plan (get-ds)))
    (setf (ds-curr-plan (get-ds)) (plan-node-next (ds-curr-plan (get-ds))))
    (set-quit-conversation t))
) ; END advance-plan





;``````````````````````````````````````````````````````
;
; [*] CORE TASK IMPLEMENTATIONS
;
;``````````````````````````````````````````````````````





(defun execute-curr-step () ; {@}
;`````````````````````````````````
; Performs the next step of the current dialogue plan, and
; updates the plan state, removing any completed subplans.
;
; TODO: ultimately, I think matching expectations in the plan
; and executing Eta actions should be separate tasks.
;
  (let ((curr-step (plan-node-step (ds-curr-plan (get-ds)))) ep-name wff subj certainty advance-plan?)
    (setq ep-name (get-step-ep-name curr-step))
    (setq wff (get-step-wff curr-step))

    ; Get subject of the episode
    (setq subj (car wff))

    (cond
      ; If keyword episode, execute step
      ((keywordp subj)
        (setq advance-plan? (process-keyword-step curr-step)))
      ; If expected step by the user, process expectation
      ((equal subj '^you)
        (setq advance-plan? (process-expected-step curr-step)))
      ; If intended step by Eta, execute step if possible
      ((equal subj '^me)
        (setq advance-plan? (process-intended-step curr-step)))
      ; Otherwise, treat step as expectation
      (t (setq advance-plan? (process-expected-step curr-step))))

    ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

    ; If plan is to be advanced, reset the timer for checking whether to fail a step, and update the plan
    (when advance-plan?
      (set-step-failure-timer (get-universal-time))
      (advance-plan))

    ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

)) ; END execute-curr-step





(defun perceive-world () ; {@}
;````````````````````````````````````
; Perceive the world by cycling through each of Eta's registered subsystems
; collecting facts from each one, and adding them to context as well as a
; perception queue for further interpretation. In the case where a fact
; is negated (i.e., (not ...)), the fact is instead removed from context.
;
; Periodically (with the parameter determining the period defined in the 'init'
; function), Eta should also flush context of facts with predicates that are
; known to be "instantaneous" telic predicates, e.g., saying, replying, moving, etc.
;
  (let (inputs)

  ; Cycle through all registered perception sources;
  ; for each observation, instantiate an episode and
  ; any relevant temporal relations.
    (dolist (system (get-perception-servers *config-agent*))
      (setq inputs (read-from-system system (get-io-path)))

      ;; (when inputs (format t "received inputs: ~a~%" inputs)) ; DEBUGGING

      ; Remove any facts observed to no longer be true, i.e. of the form (not (...)).
      ; Then remove these facts from the list.
      ; TODO: some facts imply the negation of other ones,
      ; e.g., (^you smiling.a) => (not (^you frowning.a)). Presumably
      ; this would be inferred, but I'm not sure how this inferred
      ; fact can be used to remove a previous (^you smiling.a) fact
      ; from context, unless we look for & remove contradictions from
      ; context each time this task executes. I suppose another option
      ; to inferring the negation would be using a lexical resource,
      ; such as WordNet, which has mutual exclusion relations.
      (mapcar (lambda (input)
        (when (equal 'not (car input)) (remove-old-contextual-fact (second input) (get-ds)))) inputs)
      (remove-if (lambda (input) (equal 'not (car input))) inputs)

      ; Add facts to context
      (when inputs (setq ep-name-new (store-new-contextual-facts inputs (get-ds))))

      ; Add facts to perceptions buffer for further interpretation
      (setq inputs (mapcar (lambda (input) (list ep-name-new input)) inputs))
      (enqueue-in-buffer-ordered inputs (buffers-perceptions (ds-buffers (get-ds)))))

)) ; END perceive-world





(defun interpret-perceptions () ; {@}
;```````````````````````````````
; Given a list of enqueued perceptions, go through each one and interpret it
; in the context of the current (sub)plan (emptying the queue in the process).
;
  (let ((curr-step (plan-node-step (ds-curr-plan (get-ds)))))

    ; Pop each buffered perception and try to interpret each one in
    ; context of the current plan step
    (dolist (fact (pop-all-from-buffer (buffers-perceptions (ds-buffers (get-ds)))))
      (interpret-perception-in-context fact curr-step))

)) ; END interpret-perceptions





(defun suggest-possible-actions-from-input () ; {@}
;````````````````````````````````````````````````
; Attempt to suggest a possible action (perhaps with some associated urgency) that the system might
; add to the plan in reaction to each user gist clause.
;
; These actions may arise from the following cases:
; 1. Reactions to user gist clauses.
; 2. (TBC) Reactions to other perceptions.
; 3. (TBC) Reconsidering previous plan steps that failed to realize intended goal.
; 4. (TBC) The system's desires.
; 5. (TBC) Suggestions from knowledge-based inference.
; 
; TODO: possible future extensions:
; 1. Implement TBC mechanisms.
; 2. Allow urgency scores to be returned by pattern transduction rules.
; 3. Modify urgency scores in some way according to the order of the gist clauses (and statement vs. question)?
;
; TODO REFACTOR : ensure order of priority for gists/possible actions is correct
;
  (let (poss-actions gists)
    (setq gists (mapcar #'second (purify-func (iterate-buffer (buffers-gists (ds-buffers (get-ds)))))))

    ; Form a reaction to each gist clause in the buffer (removing nil gists, unless they are
    ; the only gist present, in which case it may be possible to form a reaction to that)
    (setq poss-actions (remove nil (mapcar #'suggest-reaction-to-input gists)))
    (enqueue-in-buffer-ordered poss-actions (buffers-actions (ds-buffers (get-ds))))

)) ; END suggest-possible-actions-from-input





(defun infer-facts-top-down () ; {@}
;```````````````````````````````
; TBC
;
  nil
) ; END infer-facts-top-down





(defun infer-facts-bottom-up () ; {@}
;``````````````````````````````````
; TBC
; 
  nil
) ; END infer-facts-bottom-up





(defun add-possible-actions-to-plan () ; {@}
;```````````````````````````````````````
; Attempt to add actions from the queue of possible actions into the plan. The action
; is added as the WFF of a new intended episode step, and becomes the new current plan
; node.
;
; TODO: possible future extensions:
; 1. Have policy for selecting and adding top K possible actions, rather than only selecting first.
; 2. Verify that action can be added to plan before attempting to add it.
; 3. Add action to the plan somewhere other than as the new current node.
; 
  (let (poss-action plan-node)
    ; For now, we assume that the system will only attempt to fit the most urgent
    ; action into the plan, and the rest will be evicted before the next task cycle
    (setq poss-action (pop-item-from-buffer (buffers-actions (ds-buffers (get-ds)))))

    ; If a possible action is found, insert new plan node and make it the currently due step
    (when poss-action 
      (setq plan-node
        (init-plan-from-episode-list (list :episodes (episode-var) poss-action)))
      (setf (ds-curr-plan (get-ds)) (insert-before-plan-node (ds-curr-plan (get-ds)) plan-node)))

)) ; END add-possible-actions-to-plan





(defun expand-abstract-plan-steps () ; {@}
;``````````````````````````````````````
; This attempts to expand the surface steps in the plan using one of:
; 1. A function implementation selected by matching the step WFF
; 2. (TBC) A pattern transduction tree mapping the step WFF to substep WFF(s)
; 3. A schema whose header matches the step WFF (unifying any schema args)
;
; TODO: currently, this only attempts to expand the currently due step, because
; certain plan steps (e.g., conditionals and repeat-until loops) require checking
; certain contextual information in "real time". However, it seems that some steps may
; be able to be expanded at any point, even if not due yet.
; 
  (let ((curr-step (plan-node-step (ds-curr-plan (get-ds)))) wff subplan-node)

    ;; (format t "~%steps of current plan are: ~%")
    ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

    (setq wff (get-step-wff curr-step))

    ; Method 1
    (setq subplan-node (cond
      ((keywordp (car wff))
        (expand-keyword-step curr-step))
      (t
        (expand-episode-step curr-step))))

    ; Method 2 (TODO)
    (when (null subplan-node)
      (setq subplan-node nil))

    ; Method 3
    (when (null subplan-node)
      (setq subplan-node
        (expand-schema-step curr-step)))

    ; If a new subplan was generated, use it to expand the current node
    (when subplan-node
      (setf (ds-curr-plan (get-ds)) (expand-plan-node (ds-curr-plan (get-ds)) subplan-node))
      ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING
      t)
)) ; END expand-abstract-plan-steps





(defun merge-equivalent-plan-steps () ; {@}
;```````````````````````````````````````
; Merge steps in the plan that are deemed to be equivalent, unifiable, or otherwise
; able to be simultaneously instantiated by a single step.
;
; TODO: this currently only supports merging two subsequent (^you reply-to.v ?e) steps 
; in the plan, but this needs to be made more general, and to make use of explicit
; equivalence knowledge.
; 
  (let ((curr-step (plan-node-step (ds-curr-plan (get-ds)))) next-step wff1 wff2
        emb-ep-name1 emb-ep-name2 subj predicate subplan-node)
    (setq wff1 (get-step-wff curr-step))
    (when (plan-node-next (ds-curr-plan (get-ds)))
      (setq next-step (plan-node-step (plan-node-next (ds-curr-plan (get-ds)))))
      (setq wff2 (get-step-wff next-step))
      ; Attempt to merge subsequent relative speech act steps
      (when (and (relative-speech-act? wff1) (not (variable? (third wff1)))
                 (relative-speech-act? wff2) (not (variable? (third wff2)))
                 (equal (first wff1) (first wff2)) (equal (second wff1) (second wff2)))
        (setq subj (first wff1))
        (setq predicate (second wff1))
        (setq emb-ep-name1 (third wff1))
        (setq emb-ep-name2 (third wff2))
        (setq subplan-node (init-plan-from-episode-list
          (list :episodes (episode-var) `(,subj ,predicate (,emb-ep-name1 and ,emb-ep-name2))))))
      ; If the steps are mergeable, merge into current plan
      (when subplan-node
        (setf (ds-curr-plan (get-ds))
          (merge-plan-nodes (ds-curr-plan (get-ds)) (plan-node-next (ds-curr-plan (get-ds))) subplan-node))))
)) ; END merge-equivalent-plan-steps





(defun reorder-conflicting-plan-step () ; {@}
;```````````````````````````````````````
; TBC
; 
  nil
) ; END reorder-conflicting-plan-step

    



(defun evict-buffers () ; {@}
;```````````````````````````
; Clears buffers of pending facts upon each full cycle through
; task list. This assumes that facts are handled "in batch"
; during each cycle, but in the future we may wish for them
; to be handled incrementally. In this case, this function should
; be modified to filter old/irrelevant facts from the buffers
; (perhaps based on importance or some other metric).
;
  (clear-buffer (buffers-perceptions (ds-buffers (get-ds))))
  (clear-buffer (buffers-gists (ds-buffers (get-ds))))
  (clear-buffer (buffers-semantics (ds-buffers (get-ds))))
  (clear-buffer (buffers-pragmatics (ds-buffers (get-ds))))
  (clear-buffer (buffers-inferences (ds-buffers (get-ds))))
  (clear-buffer (buffers-actions (ds-buffers (get-ds))))
) ; END evict-buffers





(defun suggest-reaction-to-input (user-gist-clause) ; {@}
;````````````````````````````````````````````````````
; Starting at a top-level choice tree root, choose an action
; suitable for reacting to 'user-gist-clause' (which is a single
; sentence, without tags (and with a final detached "\." or "?"),
; that captures the main content (gist) of a user input). Return
; a WFF for an action suitable for reacting to that input.
;
; If the action arrived at is a particular verbal output (or gist
; clause to paraphrase), signalled by either an :out or :gist directive
; respectfully, return the respective speech act.
;
; Otherwise, the action will correspond to a schema (potentially with
; specified arguments), signalled by either the :schema or :schema+args
; directive, respectfully.
;
; TODO REFACTOR : is there any need for explicit :schema or :schema+args
; directives now?
;
; USES TT
;
  (let (choice wff schema-name args eta-gist-clause keys)
    
    (if (null user-gist-clause)
      (return-from suggest-reaction-to-input nil))

    (format t "~% ========== Eta Reaction ==========") ; DEBUGGING

    ; Use choice tree '*reaction-to-input* to select reaction to gist clause
    (format t "~%  * Reacting to user clause: ~a " user-gist-clause) ; DEBUGGING
    (setq choice (choose-result-for user-gist-clause '*reaction-to-input*))
    (format t "~%  * Chose reaction: ~a ~%" choice) ; DEBUGGING

    (cond
      ; null choice -- do nothing (return nil)
      ((null choice) nil)

      ; :gist directive
      ((eq (car choice) :gist)
        (cond
          ((atom (first (cdr choice)))
            (setq eta-gist-clause (cdr choice)))
          (t
            (setq eta-gist-clause (first (cdr choice)))
            (setq keys (second (cdr choice)))))
        ;; (format t "~%chosen Eta gist clause = ~a~%" eta-gist-clause) ; DEBUGGING
        `(^me paraphrase-to.v ^you (quote ,eta-gist-clause)))

      ; :out directive
      ((eq (car choice) :out)
        (if (null (cdr choice))
          nil
          (create-say-to-wff (cdr choice))))

      ; :ulf directive
      ((eq (car choice) :ulf)
        (if (null (cdr choice))
          nil
          (if (atom (cdr choice))
            (list (cdr choice))
            (cdr choice))))

      ; :schema directive
      ((eq (car choice) :schema)
        (setq schema-name (cdr choice))
        (list schema-name))

      ; :schema+args directive
      ((eq (car choice) :schema+args)
        ; We assume that the cdr of 'choice' must then be of form
        ; (<schema name> <argument list>)
        ; The idea is that the separate pieces of the word sequence
        ; supply separate gist clauses that Eta may react to in the
        ; steps of the schema. These are provided as sublists in 
        ; <argument list>.
        (setq schema-name (first (cdr choice)) args (second (cdr choice)))
        (cons (car args) (cons schema-name (cdr args)))))

)) ; END suggest-reaction-to-input





;``````````````````````````````````````````````````````
;
; [*] CORE PLAN EXPANSION METHODS
;
;``````````````````````````````````````````````````````





(defun expand-keyword-step (plan-step) ; {@}
;``````````````````````````````````````````````
; Any keyword plan episodes concerned with control flow (e.g., conditional events,
; repeating an event, etc.) are implemented here.
;
  (let (bindings expr subplan-node ep-name wff args-list)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    ;; (format t "~%WFF = ~a,~% in the ETA action ~a being processed~%" wff ep-name) ; DEBUGGING

    ; Form subplan based on the type of keyword step
    (cond

      ;`````````````````````````````
      ; Keyword: Conditional
      ;`````````````````````````````
      ; Simple "if cond, do this, else do this" conditional.
      ; binding yields ((_+ (cond1 name1 wff1 name2 wff2 ... :else (name3 wff3 ...))))
      ((setq bindings (bindings-from-ttt-match '(:if _+) wff))
        (setq expr (get-multiple-bindings bindings))
        ; Generate and subplan (possibly nil)
        (setq subplan-node (plan-if-else plan-step expr)))

      ;``````````````````````````````````````````
      ; Keyword: Sequence of conditionals
      ;``````````````````````````````````````````
      ; An arbitrary number of conditionals which are tried in sequence.
      ; bindings yields ((_+ ((:if cond1 name1.1 wff1.1 name1.2 wff1.2 ...)
      ;                       (:if cond2 name2.1 wff2.1 name2.2 wff2.2 ...) ...
      ;                       (:else name3.1 wff3.1 name3.2 wff3.2 ...))))
      ((setq bindings (bindings-from-ttt-match '(:try-in-sequence _+) wff))
        (setq expr (get-multiple-bindings bindings))
        ; Generate a subplan for the 1st action-wff with a true condition:
        (setq subplan-node (plan-try-in-sequence plan-step expr)))

      ;`````````````````````
      ; Keyword: Looping
      ;`````````````````````
      ; Basic repeat-until loop; potentially other forms of loops in the future.
      ; bindings yields ((_+ (ep-var cond name1 wff1 ...)))
      ; ep-var supplies a (quoted) episode variable, cond supplies the condition of the loop,
      ; and the rest of the list is a number of name, wff pairs.
      ((setq bindings (bindings-from-ttt-match '(:repeat-until _+) wff))
        (setq expr (get-multiple-bindings bindings))
        ; Generate a subplan for the 1st action-wff with a true condition:
        (setq subplan-node (plan-repeat-until plan-step expr)))
      
      ;`````````````````````
      ; Keyword: N/A
      ;`````````````````````
      ; Write an error for any unrecognizable keyword step
      (t (format t "~%*** UNRECOGNIZABLE KEYWORD STEP ~a " wff)))

    ; Expanding a keyword step is a successful instantiation of that step
    (when subplan-node
      (instantiate-plan-step plan-step))

    subplan-node
)) ; END expand-keyword-step





(defun expand-episode-step (plan-step) ; {@}
;``````````````````````````````````````````````
; Any plan expansions via functions for particular episodes (e.g.,
; generating say-to episodes from reply-to episodes) are implemented here.
;
  (let (ep-name wff bindings expr expr1 subplan-node)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    ;; (format t "~%WFF = ~a,~% in the ETA action ~a being processed~%" wff ep-name) ; DEBUGGING
    ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

    ; Determine the type of the current action, and form a subplan accordingly.
    (cond

      ;`````````````````````
      ; Eta: Paraphrasing
      ;`````````````````````
      ; Elaborate a gist-clause by Eta into a surface-level utterance.
      ; expr yields e.g. ((_+ '(I am a senior comp sci major \.))), or nil if unsuccessful.
      ((setq bindings (bindings-from-ttt-match '(^me paraphrase-to.v ^you _+) wff))
        (setq expr (eval-functions (get-single-binding bindings)))
        (setq subplan-node (plan-paraphrase-to plan-step expr)))

      ;`````````````````````
      ; Eta: Replying
      ;`````````````````````
      ; Generate an utterance based on a previous episode using transduction trees or a language model.
      ; expr yields e.g. ((_! E34)), or nil if unsuccessful.
      ((setq bindings (bindings-from-ttt-match '(^me reply-to.v _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-reply-to plan-step expr)))

      ;`````````````````````
      ; Eta: Reacting
      ;`````````````````````
      ; React to a previous episode, *including* potentially instantiating a subschema (as
      ; opposed to replying). Since reaction is now handled generically as a separate task,
      ; this is somewhat deprecated, but is still supported in schemas as a way to 'force'
      ; the system to react to the previous gist clause prior to collecting new perceptions.
      ; For the most part, generic response generation is now handled using (^me reply-to.v ...).
      ; expr yields e.g. ((_! EP34.)), or nil if unsuccessful.
      ((setq bindings (bindings-from-ttt-match '(^me react-to.v _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-reaction-to plan-step expr)))

      ;`````````````````````
      ; Eta: Telling
      ;`````````````````````
      ; e.g., (^me tell.v ^you (ans-to (wh ?x (^me have-as.v name.n ?x))))
      ((setq bindings (bindings-from-ttt-match '(^me tell.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-tell expr)))

      ;`````````````````````
      ; Eta: Answering
      ;`````````````````````
      ; e.g., (^me answer.v ^you '(My name is Lissa \.))
      ((setq bindings (bindings-from-ttt-match '(^me answer.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-answer expr)))

      ;`````````````````````
      ; Eta: Asking
      ;`````````````````````
      ; e.g., (^me ask.v ^you (wh ?x (^you have-as.v name.n ?x)))
      ((setq bindings (bindings-from-ttt-match '(^me ask.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-ask expr)))

      ;`````````````````````
      ; Eta: Describing
      ;`````````````````````
      ; e.g., (^me describe-to.v ^you (the.d image.n))
      ((setq bindings (bindings-from-ttt-match '(^me describe-to.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-description expr)))

      ;`````````````````````
      ; Eta: Suggesting
      ;`````````````````````
      ; e.g. (^me suggest-to.v ^you (that (^you provide-to.v ^me (K ((attr extended.a) (plur answer.n))))))
      ((setq bindings (bindings-from-ttt-match '(^me suggest-to.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-suggest expr)))

      ;`````````````````````
      ; Eta: Asking
      ;`````````````````````
      ; e.g. (^me ask.v ^you (ans-to (wh ?x (^you have-as.v major.n ?x))))
      ((setq bindings (bindings-from-ttt-match '(^me ask.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-question expr)))

      ;`````````````````````
      ; Eta: Saying hello
      ;`````````````````````
      ((equal wff '(^me say-hello-to.v ^you))
        (setq subplan-node (plan-saying-hello)))

      ;``````````````````````
      ; Eta: Saying good-bye
      ;``````````````````````
      ((equal wff '(^me say-bye-to.v ^you))
        (setq subplan-node (plan-saying-bye)))

      ;````````````````````````````
      ; Eta: Conditionally saying
      ;````````````````````````````
      ; Generate an utterance conditionally based on the semantics of a user question, and a
      ; list of relations that provide the answer to the user's question.
      ; exper yields e.g. ((_! ((sub what.pro ...) ?)) (_!1 (((that ...) certain-to-degree 0.8) ...)))
      ((setq bindings (bindings-from-ttt-match '(^me conditionally-say-to.v ^you _! _!1) wff))
        (setq expr (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr1 (get-single-binding bindings))
        (setq subplan-node (plan-conditionally-saying plan-step expr expr1)))

      ;````````````````````````````
      ; Eta: Conditionally paraphrasing
      ;````````````````````````````
      ; Generate a paraphrase-to.v step conditionally based on the semantics of a user question, and a
      ; list of relations that provide the answer to the user's question.
      ; expr yields e.g. ((_! ((sub what.pro ...) ?)) (_!1 (((that ...) certain-to-degree 0.8) ...)))
      ((setq bindings (bindings-from-ttt-match '(^me conditionally-paraphrase-to.v ^you _! _!1) wff))
        (setq expr (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr1 (get-single-binding bindings))
        (setq subplan-node (plan-conditionally-paraphrasing plan-step expr expr1)))

      ;````````````````````````````
      ; Eta: Proposing
      ;````````````````````````````
      ; Generates a proposal utterance from a reified action.
      ; expr yields e.g. ((_! (ka (put.v |B1| (on.p |B2|)))))
      ((setq bindings (bindings-from-ttt-match '(^me propose1-to.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-proposal plan-step expr)))

      ;````````````````````````````
      ; Eta: Issuing corrections
      ;````````````````````````````
      ; Generates a correction utterance from a reified action.
      ; This is equivalent to proposing, except processed differently so as
      ; to suppress corrections on 'undo' actions, and add corrective phrasing.
      ; expr yields e.g. ((_! (ka (move.v |B1| (back.mod-a (on.p |B2|))))))
      ((setq bindings (bindings-from-ttt-match '(^me issue-correction-to.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-correction plan-step expr)))

      ;````````````````````````````
      ; Eta: Trying
      ;````````````````````````````
      ; Forms a subplan for whichever argument is given to the try1.v
      ; action. This currently doesn't do anything apart from this expansion,
      ; but ultimately this might be used for "lookahead" type planning.
      ; expr yields e.g. ((_! (move.v |B1| (on.p |B2|))))
      ((setq bindings (bindings-from-ttt-match '(^me try1.v (to _!)) wff))
        (setq expr (get-single-binding bindings))
        (setq subplan-node (plan-try plan-step expr)))
    )

    subplan-node
)) ; END expand-episode-step





(defun expand-schema-step (plan-step) ; {@}
;``````````````````````````````````````````````
; This function attempts to match the plan-step to the header of a schema,
; unifying any variables in the header if necessary. If matched, the schema
; is instantiated and used to create a subplan.
;
; TODO: this currently assumes that schema headers are flat expressions, of
; the form (arg1 pred arg2 arg3 ...), or perhaps just (pred), but ultimately
; this should support more complex header formulas.
;
  (let (subplan-node ep-name wff args-list)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (cond
      ; In unexpected case WFF = (pred)
      ((and (= (length wff) 1) (schema-predicate? (car wff)))
        (setq subplan-node (plan-subschema (car wff) nil)))

      ; Otherwise, WFF = (arg1 pred arg2 arg3 ...)
      ; Note that arg1 might be (set-of arg1a arg1b ...), in which case
      ; we add each sub-argument to the front of the list in order
      ((and (> (length wff) 1) (schema-predicate? (second wff)))
        (setq args-list (append (extract-set (car wff)) (cddr wff)))
        (setq subplan-node (plan-subschema (second wff) args-list))))
    
    subplan-node
)) ; END expand-schema-step





(defun plan-subschema (schema-predicate args &optional ds) ; {@}
;```````````````````````````````````````````````````````````
; Given a schema predicate and a list of args, create a
; subplan from the instantiated schema.
;
  (when (null ds) (setq ds (get-ds)))
  (init-plan-from-schema (get-stored-schema schema-predicate) args ds
    :plan-var-table (ds-plan-var-table ds)
    :schema-instances (ds-schema-instances ds))
) ; END plan-subschema





(defun plan-nested-episode-list (plan-step episodes) ; {@}
;``````````````````````````````````````````````````````
; Given a list of episodes nested inside a particular plan-step, 
; create a subplan from the episode list, making sure to inherit
; any relevant properties from the schema(s) that the plan-step
; is part of.
;
  (init-plan-from-episodes-from-schemas
    (cons :episodes episodes)
    (plan-step-schemas plan-step)
    :plan-var-table (ds-plan-var-table (get-ds))
    :schema-instances (ds-schema-instances (get-ds)))
) ; END plan-nested-episode-list





(defun plan-nested-episode-list-repeating (plan-step episodes-embedded episodes-loop) ; {@}
;``````````````````````````````````````````````````````````````````````````````````````
; Given a list of looping episodes nested inside a particular plan-step, 
; create a subplan from the episode list, making sure to inherit
; any relevant properties from the schema(s) that the plan-step
; is part of.
;
  (init-plan-from-episode-list-repeating
    episodes-embedded
    episodes-loop
    :schemas (plan-step-schemas plan-step)
    :plan-var-table (ds-plan-var-table (get-ds))
    :schema-instances (ds-schema-instances (get-ds)))
) ; END plan-nested-episode-list-repeating





(defun plan-if-else (plan-step expr) ; {@}
;````````````````````````````````````````
; expr = (cond name1 wff1 name2 wff2 ... :else (name3 wff3 name4 wff4 ...))
; Expr is a condition followed by consecutive name & wff pairs. Optionally,
; this is followed by an :else keyword and additional name & wff pairs.
;
  (let* ((cnd (car expr)) (rst (cdr expr))
         (else-episodes (car (get-keyword-contents rst '(:else))))
         (if-episodes (if (not else-episodes) rst
          (reverse (set-difference rst (member :else rst))))))
    (cond
      ; Try conditional
      ((eval-truth-value cnd)
        (plan-nested-episode-list plan-step if-episodes))
      ; Otherwise try else, if it exists
      (else-episodes
        (plan-nested-episode-list plan-step else-episodes))))
) ; END plan-if-else





(defun plan-try-in-sequence (plan-step expr) ; {@}
;``````````````````````````````````````````````````
; expr = ((:if cond1 name1.1 wff1.1 name1.2 wff1.2 ...)
;         (:if cond2 name2.1 wff2.1 name2.2 wff2.2 ...) ...
;         (:else name3.1 wff3.1 name3.2 wff3.2 ...))
; Expr is a list of consecutive (:if cond e1 e2 ...) lists, potentially followed
; by a final (:else e1 e2 ...) list. These conditions should be tried in sequence,
; instantiating the first one which holds true.
;
  (let* ((lst1 (car expr)) (else1 (if (equal (first lst1) :else) t))
         (cond1 (if (not else1) (second lst1)))
         (episodes1 (if (not else1) (cddr lst1) (cdr lst1))))
    (cond
      ; None of the cases have been matched, so no subplan is generated
      ((null expr) nil)
      ; If the condition is satisfied, create a subplan from the episode list
      ((or else1 (eval-truth-value cond1))
        (plan-nested-episode-list plan-step episodes1))
      ; Otherwise, try next condition & episodes
      (t (plan-try-in-sequence plan-step (cdr expr))))
)) ; END plan-try-in-sequence





(defun plan-repeat-until (plan-step expr) ; {@}
;```````````````````````````````````````````
; expr = (ep-var cond name1 wff1 name2 wff2 ...)
;
; 'ep-name' is the name of the reoccuring :repeat-until episode. It will
; be used again in the recursion at the end of the plan we are forming;
; 'expr' is of form
;    (ep-var cond name1 wff1 name2 wff2 ...), 
; where cond is the stop condition of the repeated event,
; 'name1' is the episode characterized by the first action- or event-wff
; 'wff1', 'name2' is the episode characterized by the 2nd action- or
; event-wff 'wff2', etc.
;
; The subplan (if wff0 is false) will consist of all the steps of the loop (with
; duplicate action names created, which inherit any attached gist clauses/ulf/etc.), 
; and ending with another repeat-until loop, identical to the original one.
;
; TODO: I THINK I'LL ALSO NEED 'plan-seq-acts', 'plan-consec-acts', ETC.
; THESE SHOULD BE PRETTY SIMPLE, JUST LISTING THE ACTIONS & PROVIDING
; seq-ep, consec-ep, ETC. RELATIONS IN THE SUBPLAN. 
;
  (let (ep-name (cond1 (first expr)) (expr-rest (cdr expr)) truth-val)
    (setq ep-name (get-step-ep-name plan-step))
    ; First check termination condition
    (setq truth-val (eval-truth-value cond1))
    (cond
      ; Termination has been reached - return nil so the calling program can delete loop
      (truth-val nil)
      ; Otherwise, create a subplan that has the steps of the loop & a recursive copy of the loop
      (t (plan-nested-episode-list-repeating plan-step expr-rest (list ep-name (cons :repeat-until expr)))))
)) ; END plan-repeat-until





;``````````````````````````````````````````````````````
;
; [*] SPECIFIC PLAN EXPANSION METHODS
; TODO REFACTOR : should have a separate file allowing for
; implementations of primitive actions
; (maybe specific to each avatar??)
;
;``````````````````````````````````````````````````````





(defun plan-paraphrase-to (plan-step gist) ; {@}
;```````````````````````````````````````````
  (let (ep-name wff prev-step prev-step-ep-name user-gist-clauses utterance)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (cond
      ; If the current "say" action is a question, then use 'topic-keys'
      ; and gist-kb-user to see if question has already been answered.
      ; If so, omit action.
      ((obviated-question gist ep-name)
        ; TODO REFACTOR : this probably isn't necessary anymore once
        ; obviated actions are removed using some other mechanism
        nil)

      ; Get subtree, get surface response, attach say-to.v action
      ((eq (car gist) 'quote)
        (setq gist (flatten (second gist)))

        ; Store gist-clause in KB
        (store-gist gist (get ep-name 'topic-keys) (ds-gist-kb-eta (get-ds)))

        ; Use previous user speech act as context for interpretation
        (setq prev-step (find-prev-turn-of-agent (get-^you) (get-ds)))
        (when prev-step
          (setq prev-step-ep-name (dialogue-turn-episode-name prev-step))
          (setq user-gist-clauses (dialogue-turn-gists prev-step)))

        (format t "~% ========== Eta Generation ==========") ; DEBUGGING
        (format t "~%  * Found user gist clauses (from previous episode ~a):~%   ~a " prev-step-ep-name user-gist-clauses) ; DEBUGGING
        (format t "~%  * Paraphrasing utterance:~%    ~a " gist) ; DEBUGGING

        (cond
          ; Use GPT3-based paraphrase model if available
          ((equal (get-generation-mode *config-agent*) 'GPT3)
            (setq utterance (form-surface-utterance-using-language-model
              :conds (get-facts-for-generation '(dial-schemas))
              :gist gist)))
          
          ; Otherwise, use rule-based methods to select surface utterance
          (t
            ; Get utterance from gist-clause and prior gist-clause
            ; TODO: for now this appends all user gist-clauses together, but there might be a better way to do it
            (setq utterance (form-surface-utterance-from-gist-clause gist (apply #'append user-gist-clauses)))))

        ; Attach say-to.v action as subplan to paraphrase-to.v action
        (init-plan-from-episode-list (list :episodes (episode-var) (create-say-to-wff utterance))))

      ; Other argument types unexpected
      (t nil))
)) ; END plan-paraphrase-to





(defun plan-reply-to (plan-step expr) ; {@}
;````````````````````````````````````````
; Starting at a top-level choice tree root, choose a reply utterance 
; based on user gist clauses (which is one or more sentences, without tags
; (and with a final detached "\." or "?"), that try to capture the main content
; (gist) of a user input). Return a subplan for realizing  that utterance.
;
; If the action arrived at is a particular verbal output (instantiated
; reassembly pattern, where the latter was signalled by directive :out, 
; & is indicated by ':out' in the car of the 'choose-result-for' result), 
; form a plan with one action, viz. the action of saying that verbal 
; output.
;
; If the action arrived at is another choice tree root (signalled by
; directive :subtree), this will be automatically pursued recursively
; in the search for a choice, ultimately delivering a verbal output.
;
; USES TT
;
  (let (ep-name wff user-semantics user-gists user-gist choice args eta-gist keys)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (cond
      ; If 'quoted' gist-clause, attribute gist-clause to user
      ((and (listp expr) (eq (car expr) 'quote))
        (setq expr (flatten (second expr)))
        (setq user-gists (list expr))
        (setq user-semantics nil))
      ; Otherwise, find gist clauses associated with episode(s) Eta is replying to
      (t
        (setq expr (extract-set expr))
        (setq user-gists (apply #'append (mapcar #'get-gist-clauses-characterizing-episode expr)))
        (setq user-semantics (apply #'append (mapcar (lambda (e)
          (resolve-references (get-semantic-interpretations-characterizing-episode e (get-ds)))) expr)))))

    (format t "~% ========== Eta Generation ==========") ; DEBUGGING
    ;; (format t "~% user gist clause (for ~a) is ~a" expr user-gists) ; DEBUGGING
    ;; (format t "~% user semantics (for ~a) is ~a ~%" expr user-semantics) ; DEBUGGING

    ; Combine previous gist clauses into one
    (setq user-gist (apply #'append (purify-func user-gists)))

    (if (null user-gist)
      (return-from plan-reply-to nil))

    ; Use choice tree '*reply-to-input* to form reply to user gist clause
    (format t "~%  * Replying to user clause: ~a " user-gist) ; DEBUGGING
    (setq choice (choose-result-for user-gist '*reply-to-input*))
    (format t "~%  * Chose reply: ~a " choice) ; DEBUGGING

    ; 'choice' may be an instantiated reassembly pattern corresponding to a
    ; gist-clause (directive :gist), to be contextually paraphrased by Eta
    ; as a subplan, or a direct output (directive :out) to be spoken to the
    ; user as a subplan.
    ; In the first case we create a 1-step subplan whose action is of
    ; the type (^me paraphrase-to.v ^you '(...)) or (^me say-to.v ^you '(...)), respectively.
    (cond
      ; null choice -- use GPT3 to generate a reply (if in GPT3 response generation mode);
      ; otherwise generate an "empty" say-to.v action.
      ((null choice)
        (init-plan-from-episode-list
          (list :episodes (episode-var) (create-say-to-wff
            (if (equal (get-generation-mode *config-agent*) 'GPT3)
              (form-surface-utterance-using-language-model
                :conds (get-facts-for-generation '(dial-schemas))
                :facts (get-facts-for-generation '(epi-schemas memory)))
              nil)))))

      ; :gist directive
      ((eq (car choice) :gist)
        (cond
          ((atom (first (cdr choice)))
            (setq eta-gist (cdr choice)))
          (t
            (setq eta-gist (first (cdr choice)))
            (setq keys (second (cdr choice)))))
        ;; (format t "~%chosen Eta gist clause = ~a~%" eta-gist) ; DEBUGGING
        ; Store gist clause in KB
        (store-gist eta-gist keys (ds-gist-kb-eta (get-ds)))
        ; Create paraphrase-to.v subplan
        (init-plan-from-episode-list
          (list :episodes (episode-var) `(^me paraphrase-to.v ^you (quote ,eta-gist)))))

      ; :out directive
      ((eq (car choice) :out)
        (if (null (cdr choice)) nil
          (init-plan-from-episode-list
            (list :episodes (episode-var) (create-say-to-wff (cdr choice)))))))
)) ; END plan-reply-to





(defun plan-reaction-to (plan-step expr) ; {@}
;`````````````````````````````````````````
; Starting at a top-level choice tree root, choose an action or
; subschema suitable for reacting to 'user-gist-clause' (which
; is a single sentence, without tags (and with a final detached
; "\." or "?"), that captures the main content (gist) of
; a user input). Return the (new) name of a plan for realizing 
; that action or subschema.
;
; If the action arrived at is a particular verbal output (instantiated
; reassembly pattern, where the latter was signalled by directive :out, 
; & is indicated by ':out' in the car of the 'choose-result-for' result), 
; form a plan with one action, viz. the action of saying that verbal 
; output.
;
; If the action arrived at is another choice tree root (signalled by
; directive :subtree), this will be automatically pursued recursively
; in the search for a choice, ultimately delivering a verbal output
; or a schema name.
;
; If the action arrived at is a :schema+args "action" (a schema name
; along with an argument list), use this schema to form a subplan.
;
; TODO REFACTOR : replace schema directives with init-plan-from-episode-list,
; to be expanded using schema later, vs. creating schema directly. Do we even
; need schema directives anymore?
;
; USES TT
;
  (let (ep-name wff user-semantics user-gists user-gist choice schema-name args eta-gist keys)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (cond
      ; If 'quoted' gist-clause, attribute gist-clause to user
      ((and (listp expr) (eq (car expr) 'quote))
        (setq expr (flatten (second expr)))
        (setq user-gists (list expr))
        (setq user-semantics nil))
      ; Otherwise, find gist clauses associated with episode Eta is replying to
      (t
        (setq expr (extract-set expr))
        (setq user-gists (apply #'append (mapcar #'get-gist-clauses-characterizing-episode expr)))
        (setq user-semantics (apply #'append (mapcar (lambda (e)
          (resolve-references (get-semantic-interpretations-characterizing-episode e (get-ds)))) expr)))))

    ;; (format t "~% user gist clause (for ~a) is ~a" expr user-gists) ; DEBUGGING
    ;; (format t "~% user semantics (for ~a) is ~a ~%" expr user-semantics) ; DEBUGGING

    ; Combine previous gist clauses into one
    (setq user-gist (apply #'append (purify-func user-gists)))

    (if (null user-gist)
      (return-from plan-reaction-to nil))

    (format t "~% ========== Eta Reaction ==========") ; DEBUGGING

    ; Use choice tree '*reaction-to-input* to select reaction to gist clause
    (format t "~%  * Reacting to user clause: ~a " user-gist) ; DEBUGGING
    (setq choice (choose-result-for user-gist '*reaction-to-input*))
    (format t "~%  * Chose reaction: ~a ~%" choice) ; DEBUGGING

    ; 'choice' may be an instantiated reassembly pattern corresponding to a
    ; gist-clause (directive :gist), to be contextually paraphrased by Eta
    ; as a subplan, or a direct output (directive :out) to be spoken to the
    ; user as a subplan. Otherwise it may be the name of a schema (to be initialized).
    ; In the first case we create a 1-step subplan whose action is of
    ; the type (^me paraphrase-to.v ^you '(...)) or (^me say-to.v ^you '(...)), respectively.
    ; In the case of a schema, we initiate a multistep plan.
    (cond
      ; null choice -- do nothing (return nil)
      ((null choice) nil)

      ; :gist directive
      ((eq (car choice) :gist)
        (cond
          ((atom (first (cdr choice)))
            (setq eta-gist (cdr choice)))
          (t
            (setq eta-gist (first (cdr choice)))
            (setq keys (second (cdr choice)))))
        ;; (format t "~%chosen Eta gist clause = ~a~%" eta-gist) ; DEBUGGING
        ; Store gist clause in KB
        (store-gist eta-gist keys (ds-gist-kb-eta (get-ds)))
        ; Add paraphrase-to.v subplan
        (init-plan-from-episode-list
          (list :episodes (episode-var) `(^me paraphrase-to.v ^you (quote ,eta-gist)))))

      ; :out directive
      ((eq (car choice) :out)
        (if (null (cdr choice)) nil
          (init-plan-from-episode-list
            (list :episodes (episode-var) (create-say-to-wff (cdr choice))))))

      ; :schema directive
      ((eq (car choice) :schema)
        (setq schema-name (cdr choice))
        (plan-subschema schema-name nil))

      ; :schema+args directive
      ((eq (car choice) :schema+args)
        ; We assume that the cdr of 'choice' must then be of form
        ; (<schema name> <argument list>)
        ; The idea is that the separate pieces of the word sequence
        ; supply separate gist clauses that Eta may react to in the
        ; steps of the schema. These are provided as sublists in 
        ; <argument list>.
        (setq schema-name (first (cdr choice)) args (second (cdr choice)))
        (plan-subschema schema-name args)))
)) ; END plan-reaction-to





(defun plan-conditionally-saying (plan-step user-semantics expr) ; {@}
;`````````````````````````````````````````````````````````````````
; Generate an utterance conditionally based on the semantics of a user question, and a
; list of relations that provide the answer to the user's question.
;
; TODO: this function currently only supports response generation for blocks world spatial 
; questions, and can be made more general.
;
  (let (ep-name wff tuple ans eta-semantics)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (setq user-semantics (resolve-references user-semantics))

    ; Generate response based on list of relations
    (cond
      ((not (member '|Spatial-Reasoning-System| (get-specialist-servers *config-agent*)))
        (setq ans '(Could not form final answer \: |Spatial-Reasoning-System| not registered \.)))
      (t
        (setq tuple (generate-response (eval user-semantics) (eval expr) (get-ds)))
        (setq ans (first tuple))
        (setq eta-semantics (second tuple))
        (store-semantic-interpretation-characterizing-episode eta-semantics ep-name '^me '^you (get-ds))))

    (format t "answer to output: ~a~%" ans) ; DEBUGGING

    ; Create say-to.v subplan from answer
    (init-plan-from-episode-list (list :episodes (episode-var) (create-say-to-wff ans)))
)) ; END plan-conditionally-saying





(defun plan-conditionally-paraphrasing (plan-step user-semantics expr) ; {@}
;```````````````````````````````````````````````````````````````````````
; Generate a paraphrase-to.v step conditionally based on the semantics of a user question, and a
; list of relations that provide the answer to the user's question.
;
; TODO: this function currently only supports response generation for blocks world spatial 
; questions, and can be made more general.
;
; TODO: I'm not really fond of the way that this is handled currently. It seems like
; getting an answer binding from an external system, and communicating that answer,
; should be two separate steps in the schema.
;
  (let (ep-name wff tuple ans eta-semantics)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (setq user-semantics (resolve-references user-semantics))

    ; Generate response gist based on list of relations
    (cond
      ((not (member '|Spatial-Reasoning-System| (get-specialist-servers *config-agent*)))
        (setq ans '(Could not form final answer \: |Spatial-Reasoning-System| not registered \.)))
      (t
        (setq tuple (generate-response (eval user-semantics) (eval expr) (get-ds)))
        (setq ans (first tuple))
        (setq eta-semantics (second tuple))
        (store-semantic-interpretation-characterizing-episode eta-semantics ep-name '^me '^you (get-ds))))

    (format t "gist answer to output: ~a~%" ans) ; DEBUGGING

    ; Create paraphrase-to.v subplan from answer
    (init-plan-from-episode-list (list :episodes (episode-var) (create-paraphrase-to-wff ans)))
)) ; END plan-conditionally-paraphrasing





(defun plan-proposal (plan-step action-kind) ; {@}
;``````````````````````````````````````````````
; Given a proposal gist clause, convert it to an utterance using
; hierarchical transduction trees, starting at a top-level choice
; tree root.
; NOTE: currently only :out directives are expected, but this can
; be expanded if we find e.g. subschema instantiation is necessary
; (for example, if some particularly complex proposal that needs
; to be broken down into multiple actions).
;
; USES TT
;
  (let (ep-name wff proposal-gist choice)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (cond
      ((null (member '|Spatial-Reasoning-System| (get-specialist-servers *config-agent*)))
        (setq proposal-gist '(Could not create proposal \: '|Spatial-Reasoning-System| not registered \.)))
      (t (setq proposal-gist (generate-proposal action-kind))))

    ;; (format t "proposal gist: ~a~%" proposal-gist) ; DEBUGGING

    (store-gist-clause-characterizing-episode proposal-gist ep-name '^me '^you (get-ds))

    (if (null proposal-gist)
      (return-from plan-proposal nil))

    (setq choice (choose-result-for proposal-gist '*output-for-proposal-tree*))
    ;; (format t "~% proposal choice is ~a ~% " choice) ; DEBUGGING

    (when (or (null choice) (equal choice '(:out)))
      (return-from plan-proposal nil))

    (cond
      ; :out directive
      ((eq (car choice) :out)
        (init-plan-from-episode-list
          (list :episodes (episode-var) (create-say-to-wff (cdr choice))))))
)) ; END plan-proposal





(defun plan-correction (plan-step action-kind) ; {@}
;````````````````````````````````````````````````
; Given a correction gist clause, convert it to an utterance using
; hierarchical transduction trees, starting at a top-level choice
; tree root.
; NOTE: the same as plan-proposal, but uses a different rule tree.
;
; USES TT
;
  (let (ep-name wff correction-gist choice)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (cond
      ((null (member '|Spatial-Reasoning-System| (get-specialist-servers *config-agent*)))
        (setq correction-gist '(Could not create correction \: |Spatial-Reasoning-System| not registered \.)))
      (t (setq correction-gist (generate-proposal action-kind))))

    ;; (format t "correction gist: ~a~%" correction-gist) ; DEBUGGING

    (store-gist-clause-characterizing-episode correction-gist ep-name '^me '^you (get-ds))

    (if (null correction-gist)
      (return-from plan-correction nil))

    (setq choice (choose-result-for correction-gist '*output-for-correction-tree*))
    ;; (format t "~% correction choice is ~a ~% " choice) ; DEBUGGING

    (when (or (null choice) (equal choice '(:out)))
      (return-from plan-correction nil))

    (cond
      ; :out directive
      ((eq (car choice) :out)
        (init-plan-from-episode-list
          (list :episodes (episode-var) (create-say-to-wff (cdr choice))))))
)) ; END plan-correction





(defun plan-tell (info) ; {@}
;`````````````````````````````
; Return the name of a plan for telling the user the 'info';
; 'info' is a reified proposition that may be in a form that makes
; verbalization trivial, e.g.,
;     (meaning-of.f '(I am Eta. I am an autonomous avatar.))
; where the 'meaning-of.f' function in principle provides EL
; propositions corresponding to English sentences -- i.e., semantic
; parser output, reified using 'that'; but of course, for verbal-
; ization we don't need to first convert to EL! Or else the info 
; is directly in EL form, e.g.,
;     (that (^me have-as.v name.n 'Eta)), or
;     (that (^me be.v ((attr autonomous.a) avatar.n))),
; which requires English generation for a fully expanded tell
; act.
;
; e.g. telling one's name could be formulated as
; (^me tell.v ^you (ans-to (wh ?x (^me have-as.v name.n ?x))))
; and answer retrieval should bind ?x to a name. Or we could have
; explicit reified propositions such as (that (^me have-as.v name.n 'Eta))
; or (that (^me be.v ((attr autonomous.a) avatar.n))). The match variable
; info will have as a binding the (wh ...) expression.
;
; If 'info' is a variable, instantiate the variable by using an LLM to
; generate a response utterance, after retrieving relevant knowledge
; from the system's epi-schemas and memory.
;
  (let (utterance)
    (if (null info) (return-from plan-tell nil))

    (format t "~% ========== Eta Generation ==========") ; DEBUGGING

    (cond
      ((variable? info)
        (setq utterance (if (equal (get-generation-mode *config-agent*) 'GPT3)
          (form-surface-utterance-using-language-model
            :conds (get-facts-for-generation '(dial-schemas))
            :facts (get-facts-for-generation '(epi-schemas memory))
            :mode 'statement)
          nil)))
      (t (setq utterance (expr-to-words info :^me (get-^me) :^you (get-^you)))))

    (init-plan-from-episode-list
      (list :episodes (episode-var) (create-say-to-wff utterance)))

)) ; END plan-tell





(defun plan-answer (info) ; {@}
;`````````````````````````````
; Return a plan for answering a question asked by the user with some
; information, which may be a variable, some ULF, or a quoted wordlist.
; 
; If info is a variable, use an LLM to generate a response after
; retrieving relevant knowledge.
;
; NOTE: this is currently identical to the case of plan-tell above.
;
  (let (utterance)
    (if (null info) (return-from plan-answer nil))

    (format t "~% ========== Eta Generation ==========") ; DEBUGGING

    (cond
      ((variable? info)
        (setq utterance (if (equal (get-generation-mode *config-agent*) 'GPT3)
          (form-surface-utterance-using-language-model
            :conds (get-facts-for-generation '(dial-schemas))
            :facts (get-facts-for-generation '(epi-schemas memory))
            :mode 'statement)
          nil)))
      (t (setq utterance (expr-to-words info :^me (get-^me) :^you (get-^you)))))

    (init-plan-from-episode-list
      (list :episodes (episode-var) (create-say-to-wff utterance)))

)) ; END plan-answer





(defun plan-ask (question) ; {@}
;`````````````````````````````
; Return a plan for asking the user a question, which may be a variable,
; some ULF, or a quoted wordlist.
; 
; If question is a variable, use an LLM to generate a question after
; retrieving relevant knowledge.
;
  (let (utterance)
    (if (null question) (return-from plan-ask nil))

    (format t "~% ========== Eta Generation ==========") ; DEBUGGING

    (cond
      ((variable? question)
        (setq utterance (if (equal (get-generation-mode *config-agent*) 'GPT3)
          (form-surface-utterance-using-language-model
            :conds (get-facts-for-generation '(dial-schemas))
            :facts (get-facts-for-generation '(epi-schemas memory))
            :mode 'question)
          nil)))
      (t (setq utterance (expr-to-words question :^me (get-^me) :^you (get-^you)))))

    (init-plan-from-episode-list
      (list :episodes (episode-var) (create-say-to-wff utterance)))

)) ; END plan-ask





(defun plan-description (topic) ; {@}
;`````````````````````````````````````
; Describing, like telling, is an inform-act, but describing conveys a proposition
; at an abstract level (e.g. "who I am", describing one's capabilities or appearance, etc.).
; This involves access to knowledge in the appropriate categories, and this may then
; be further expanded via tell-acts.
;
; In general, describing is a severe challenge in NLG, but here it will be initially assumed
; that we have schemas for expanding any descriptive actions that a plan might call for.
; An even simpler way of packaging related sets of sentences for outputs is to just use a
; tell-act of type (^me tell.v ^you (meaning-of.f '(<sent1> <sent2> ...))), where the
; 'meaning-of.f' function applied to English sentences supplies their semantic interpretation,
; reified with the 'that' operator. Combining the two ideas, we can provide schemas for expanding
; a describe-act directly into a tell-act with a complex meaning-of.f argument.
;
  (if (null info) (return-from plan-description nil))
  ; TBC
) ; END plan-description





(defun plan-suggest (suggestion) ; {@}
;````````````````````````````````````````
; (^me suggest-to.v ^you (that (^you provide-to.v ^me (K ((attr extended.a) (plur answer.n))))))
;
  (if (null suggestion) (return-from plan-suggest nil))
  ; TBC
) ; END plan-suggest





(defun plan-question (query) ; {@}
;```````````````````````````````````
; e.g. (^me ask.v ^you (ans-to (wh ?x (^you have-as.v major.n ?x))))
  (if (null query) (return-from plan-question nil))
  ; TBC
) ; END plan-question





(defun plan-saying-hello () ; {@}
;`````````````````````````````````
  ; TBC
) ; END plan-saying-hello





(defun plan-saying-bye () ; {@}
;```````````````````````````````
  ; TBC
) ; END plan-saying-bye





(defun plan-try (plan-step expr) ; {@}
;`````````````````````````````````
; TBC
;
  (init-plan-from-episode-list
    (list :episodes (episode-var) (cons '^me expr)))
) ; END plan-try





;``````````````````````````````````````````````````````
;
; [*] CORE PLAN EXECUTION METHODS
;
;``````````````````````````````````````````````````````





(defun instantiate-plan-variable (val var &optional plan-node) ; {@}
;```````````````````````````````````````````````````````````````
; Wrapper function to bind a variable to a value in the plan structure.
; plan-node is assumed to be the current node if not given.
;
  (if (null plan-node) (setq plan-node (ds-curr-plan (get-ds))))
  (bind-variable-in-plan plan-node val var
    :plan-var-table (ds-plan-var-table (get-ds))
    :schema-instances (ds-schema-instances (get-ds)))
) ; END instantiate-plan-variable





(defun instantiate-superstep (plan-step) ; {@}
;``````````````````````````````````````````
; Instantiates a given plan-step assumed to be a superstep of some other
; step(s), provided all of the substeps have been fully instantiated. If
; the step only has a single substep, it gets the same episode constant as
; the substep; otherwise a new episode constant is created.
;
; TODO REFACTOR : in the case of multiple substeps, should we store something
; like (E1 part-of E3) (E2 part-of E3) etc. in context?
; 
; TODO REFACTOR : should we do any failure detection here? E.g., if a substep
; fails and is characterized by a ((no.d thing.n) happen.v) WFF, should the
; superstep be instantiated just as it is, or inherit the failure characterization?
; 
  (let ((substeps (plan-step-substeps plan-step)) ep-var ep-name wff)
    (setq ep-var (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))
    (if (not (variable? ep-var)) (return-from instantiate-superstep nil))

    ; Check if all substeps have been instantiated
    (when (and substeps (every (lambda (superstep)
                            (not (variable? (plan-step-ep-name superstep))))
                            substeps))
      (cond
        ; If one substep, use same episode constant
        ((= (length substeps) 1)
          (setq ep-name (plan-step-ep-name (car substeps))))
        ; Otherwise, create a new episode constant subsuming each subepisode
        (t
          (setq ep-name (episode-name))
          (store-init-time-of-episode ep-name (get-ds))))

      ; Substitute in plan and store (wff ** ep-name) in context/memory
      ; (if not keyword step)
      (instantiate-plan-variable ep-name ep-var)
      (when (not (keywordp (car wff)))
        (store-contextual-fact-characterizing-episode wff ep-name (get-ds))))

    ; Recur for all supersteps
    (mapcar #'instantiate-superstep (plan-step-supersteps plan-step))

)) ; END instantiate-superstep





(defun instantiate-plan-step (plan-step) ; {@}
;``````````````````````````````````````````
; Instantiates a given plan-step, destructively substituting the
; skolemized episode wherever it occurs in the plan, as well as binding
; that episode variable in the schema.
;
  (let (ep-var ep-name wff)
    (if (null plan-step) (return-from instantiate-plan-step nil))

    ; Get episode-var (if already instantiated, return nil)
    (setq ep-var (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))
    (if (not (variable? ep-var)) (return-from instantiate-plan-step nil))

    ; Generate a constant for the episode and destructively substitute in plan
    (setq ep-name (episode-name))
    ; TODO: use episode-relations formulas to assert relations in timegraph
    (store-init-time-of-episode ep-name (get-ds))
    (instantiate-plan-variable ep-name ep-var)

    ; Store (wff ** ep-name) in context/memory
    (when (not (keywordp (car wff)))
      (store-contextual-fact-characterizing-episode wff ep-name (get-ds)))

    ; Instantiate all supersteps of this step whose substeps are now fully completed
    (mapcar #'instantiate-superstep (plan-step-supersteps plan-step))

    ;; (format t "action list after substituting ~a for ~a:~%" ep-name ep-var)
    ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

    ; TODO: should (wff ** ep-name) be stored in context at this point?
    ;       as well as instantiating the non-fluent variable, e.g. '!e1'?

)) ; END instantiate-plan-step





(defun process-keyword-step (plan-step) ; {@}
;```````````````````````````````````````````
; Checks whether to skip a keyword step depending on whether or not
; the conditions for expansion hold.
; Returns t if the step should be skipped; nil otherwise.
;
  (let (ep-name wff bindings expr advance-plan?)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))
  
    ;; (format t "~%WFF = ~a,~% in the keyword step ~a being processed~%" wff ep-name) ; DEBUGGING

    (cond

      ;`````````````````````````````
      ; Keyword: Conditional
      ;`````````````````````````````
      ; Simple "if cond, do this, else do this" conditional.
      ; binding yields ((_+ (cond1 name1 wff1 name2 wff2 ... :else (name3 wff3 ...))))
      ((setq bindings (bindings-from-ttt-match '(:if _+) wff))
        (setq expr (get-multiple-bindings bindings))
        (setq advance-plan? (process-if-else expr)))

      ;``````````````````````````````````````````
      ; Keyword: Sequence of conditionals
      ;``````````````````````````````````````````
      ; An arbitrary number of conditionals which are tried in sequence.
      ; bindings yields ((_+ ((:if cond1 name1.1 wff1.1 name1.2 wff1.2 ...)
      ;                       (:if cond2 name2.1 wff2.1 name2.2 wff2.2 ...) ...
      ;                       (:else name3.1 wff3.1 name3.2 wff3.2 ...))))
      ((setq bindings (bindings-from-ttt-match '(:try-in-sequence _+) wff))
        (setq expr (get-multiple-bindings bindings))
        (setq advance-plan? (process-try-in-sequence expr)))

      ;`````````````````````
      ; Keyword: Looping
      ;`````````````````````
      ; repeat-until, potentially other forms of loops in the future.
      ; bindings yields ((_+ (ep-var cond name1 wff1 ...)))
      ; ep-var supplies a (quoted) episode variable, cond supplies the condition of the loop,
      ; and the rest of the list is a number of name, wff pairs.
      ((setq bindings (bindings-from-ttt-match '(:repeat-until _+) wff))
        (setq expr (get-multiple-bindings bindings))
        (setq advance-plan? (process-repeat-until expr)))
      
      ;`````````````````````
      ; Keyword: N/A
      ;`````````````````````
      ; Write an error for any unrecognizable keyword step
      (t (format t "~%*** UNRECOGNIZABLE KEYWORD STEP ~a " wff) (setq advance-plan? t)))

    ; Advancing past a keyword step due to unsatisfied condition is a successful instantiation of that step
    (when advance-plan?
      (instantiate-plan-step plan-step))

    advance-plan?
)) ; END process-keyword-step





(defun process-intended-step (plan-step) ; {@}
;```````````````````````````````````````````
; Processes an intended plan step by attempting to execute the step
; with a primitive function. This may bind variables within the plan,
; in addition to instantiating the current episode var.
;
; TODO: currently this always returns t. If an execution failed, should
; that be handled in this task by returning nil, or should success verification
; be a separate task?
;
  (let (ep-name wff bindings expr expr1 var-bindings advance-plan?)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    ;; (format t "~%WFF = ~a,~% in the ETA action ~a being processed~%" wff ep-name) ; DEBUGGING
    ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

    ; Determine the type of the current action, and execute accordingly.
    (cond

      ;`````````````````````
      ; Eta: Saying
      ;`````````````````````
      ; expr yields e.g. ((_+ '(I am a senior comp sci major\, how about you?)))
      ; or nil, for non-match
      ((setq bindings (bindings-from-ttt-match '(^me say-to.v ^you _+) wff))
        (setq expr (eval-functions (get-single-binding bindings)))
        (instantiate-plan-step plan-step)
        (setq var-bindings (execute-say-to plan-step expr))
        (setq advance-plan? t))

      ;```````````````````````````
      ; Eta: Exiting conversation
      ;```````````````````````````
      ; NOTE: duplicated from say-bye-to.v (though different action arguments) -
      ; meant to reflect a more "absolute" say-bye.v action where Eta directly/abruptly
      ; exits the conversation, whereas say-bye-to.v might be used during the exchange of
      ; pleasantries and farewells at the end of a standard conversation.
      ((equal wff '(^me say-bye.v))
        (instantiate-plan-step plan-step)
        (execute-say-bye)
        (setq advance-plan? t))

      ;`````````````````````````````````````
      ; Eta: Recalling answer from history
      ;`````````````````````````````````````
      ; Recall a set of answer relations to a ULF query, and bind the answer to the given variable.
      ; expr yields e.g. ((_!1 ((sub where.pro ...) ?)) (_!2 ?ans-relations))
      ((setq bindings (bindings-from-ttt-match '(^me recall-answer.v _!1 _!2) wff))
        (setq expr (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr1 (get-single-binding bindings))
        (instantiate-plan-step plan-step)
        (setq var-bindings (execute-recall-answer plan-step expr expr1))
        (setq advance-plan? t))

      ;````````````````````````````````````````
      ; Eta: Seek answer from external source
      ;````````````````````````````````````````
      ; Query a registered subsystem with some query ULF.
      ; expr yields e.g. ((_! |Blocks-World-System|) (_!1 ((sub where.pro ...) ?)))
      ((setq bindings (bindings-from-ttt-match '(^me seek-answer-from.v _! _!1) wff))
        (setq expr (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr1 (get-single-binding bindings))
        (instantiate-plan-step plan-step)
        (execute-seek-answer-from plan-step expr expr1)
        (setq advance-plan? t))

      ;``````````````````````````````````````````
      ; Eta: Recieve answer from external source
      ;``````````````````````````````````````````
      ; Receive a set of answer relations from a registered subsystem, and bind the
      ; answer to the given variable.
      ; expr yields e.g. ((_! |Blocks-World-System|) (_!1 ?ans-relations))
      ((setq bindings (bindings-from-ttt-match '(^me receive-answer-from.v _! _!1) wff))
        (setq expr (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr1 (get-single-binding bindings))
        (instantiate-plan-step plan-step)
        (setq var-bindings (execute-receive-answer-from plan-step expr expr1))
        (setq advance-plan? t))

      ;`````````````````````````
      ; Eta: Committing to STM
      ;`````````````````````````
      ; Store a given wff expression in short-term memory (context).
      ; expr yields e.g. ((_! (^you happy.a)))
      ((setq bindings (bindings-from-ttt-match '(^me commit-to-STM.v (that _!)) wff))
        (setq expr (get-single-binding bindings))
        (instantiate-plan-step plan-step)
        (execute-commit-to-STM plan-step expr)
        (setq advance-plan? t))

      ;````````````````````````````
      ; Eta: Findings
      ;````````````````````````````
      ; Finding some action (or other entity?).
      ; expr yields e.g. ((_! (some.d ?ka (:l (?x) (?x step1-toward.p ?goal-rep)))))
      ((setq bindings (bindings-from-ttt-match '(^me find4.v _!) wff))
        (setq expr (get-single-binding bindings))
        (instantiate-plan-step plan-step)
        (setq var-bindings (execute-find plan-step expr))
        (setq advance-plan? t))

      ;````````````````````````````
      ; Eta: Choosing
      ;````````````````````````````
      ; Choosing a referent for an indefinite quantifier.
      ; expr yields e.g. ((_! (a.d ?c (random.a (:l (?x) (and (?x member-of.p ?cc) ...))))))
      ((setq bindings (bindings-from-ttt-match '(^me choose.v _!) wff))
        (setq expr (get-single-binding bindings))
        (instantiate-plan-step plan-step)
        (setq var-bindings (execute-choose plan-step expr))
        (setq advance-plan? t))

      ;```````````````````````````````````````
      ; Eta: Forming a spatial representation
      ;```````````````````````````````````````
      ; Form some spatial representation of a concept (i.e., of an object schema).
      ; expr yields e.g. ((_! (a.d ?goal-rep ((most.mod-a simple.a) (:l (?x) (and (?x goal-schema1.n) ...))))))
      ((setq bindings (bindings-from-ttt-match '(^me form-spatial-representation.v _!) wff))
        (setq expr (get-single-binding bindings))
        (instantiate-plan-step plan-step)
        (setq var-bindings (execute-form-spatial-representation plan-step expr))
        (setq advance-plan? t))
    )

    ;; (when var-bindings
    ;;   (format t "variable substitutions to be made: ~%  ~a~%" var-bindings)) ; DEBUGGING

    ; Make all variable substitutions in the plan
    (dolist (var-binding var-bindings)
      (instantiate-plan-variable (second var-binding) (first var-binding)))

    advance-plan?
)) ; END process-intended-step





(defun process-expected-step (plan-step) ; {@}
;```````````````````````````````````````````
; Processes an expected plan step by inquiring about the truth of the
; step in context. If the step times out (determined by the certainty
; of the step), fail the step and proceed with the plan.
; Returns t if the plan should be advanced; nil otherwise.
;
; Note: upon matching a fact in context, all "instantaneous" telic facts
; are flushed from context.
;
  (let (certainty match)

    ; Write output buffer
    (when (get-output-buffer)
      (write-output-buffer (get-output-buffer) (get-io-path))
      (set-output-buffer nil))

    ; Check certainty of expected plan step
    (setq certainty (get-step-certainty plan-step))

    (cond
      ; If timer exceeds period (a function of certainty of step), instantiate a 'failed' episode
      ((>=inf (- (get-universal-time) (get-step-failure-timer)) (certainty-to-period certainty)) 
        (fail-curr-step plan-step))

      ; Otherwise, inquire self about the truth of the immediately pending episode. Plan is advanced
      ; (and appropriate substitutions made) if the expected episode is determined to be true.
      (t
        (setq match (inquire-truth-of-curr-step plan-step))
        (if match (flush-context (get-ds)))
        match))
)) ; END process-expected-step





(defun inquire-truth-of-curr-step (plan-step) ; {@}
;``````````````````````````````````````````````
; Checks the truth of the immediately pending episode in the plan,
; through a process of "self-inquiry" (i.e. checking the system's
; context/memory, potentially with some level of inference (TBC) as well).
; 
  (let (ep-var ep-name wff match)
  
    ; Get expected wff, and episode var
    (setq ep-var (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    ; Inquire about truth of wff in context; use first match
    (setq match (get-from-context wff (get-ds)))
    (if (equal match T) (setq match (list wff)))
    (when match
      (setq match (car match))

      ; Get episode name corresponding to contextual fact
      (setq ep-name (get-episode-from-contextual-fact match (get-ds)))
      
      ; Substitute that episode name for the episode variable in the plan
      (instantiate-plan-variable ep-name ep-var)

      ; Make all variable substitutions needed to unify wff and match
      (mapcar (lambda (x y) (if (and (variable? x) y)
          (instantiate-plan-variable y x)))
        wff match)

      ; Remove the matched predicate from context if a telic predicate, i.e., assume
      ; something like (^you say-to.v ^me ...) is no longer relevant after matched once.
      ; TODO: I'm not sure if this is a safe or realistic assumption to make... but it's
      ; currently necessary to prevent the program from automatically looping in the case
      ; where a :repeat-until episode consists of the user saying something and the agent
      ; replying, since the say-to.v expectation will keep matching the same fact in context.
      (if (member (second match) *verbs-telic*) (remove-old-contextual-fact match (get-ds))))

    (if match t nil)
)) ; END inquire-truth-of-curr-step





(defun fail-curr-step (plan-step) ; {@}
;```````````````````````````````````
; Determine that the immediately pending episode in the plan is a
; 'failed' episode; i.e. an expected episode which did not end up
; coming true during the "period" of expectation (governed by the
; certainty of the episode). Instantiate the episode with a new episode
; name characterized as a failure.
; Returns t after instantiating failed episode.
;
  (let (ep-name wff)

    ; Instantiate the episode variable of the new step and substitute
    (instantiate-plan-step plan-step)

    ; Get expected wff and instantiated ep-name
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    ; Add (^you do.v (no.d thing.n)) or ((no.d thing.n) happen.v) to context (depending on
    ; whether a user step or other expectation), characterizing <ep-name>. This is
    ; essentially a no-op and isn't currently used for any further inference or replanning.
    ; TODO: this needs to be reworked after working out semantics for failed episodes.
    (if (equal '^you (car wff))
      (store-contextual-fact-characterizing-episode `(^you do.v (no.d thing.n)) ep-name (get-ds))
      (store-contextual-fact-characterizing-episode '((no.d thing.n) happen.v) ep-name (get-ds)))
    
    t
)) ; END fail-curr-step





(defun process-if-else (expr) ; {@}
;`````````````````````````````
; expr = (cond name1 wff1 name2 wff2 ... :else (name3 wff3 name4 wff4 ...))
; An if-else step should be skipped if the condition is false, and
; there is no :else branch.
;
  (let* ((cnd (car expr)) (rst (cdr expr))
         (else-episodes (car (get-keyword-contents rst '(:else)))))
    (and (not (eval-truth-value cnd)) (null else-episodes))
)) ; END process-if-else





(defun process-try-in-sequence (expr) ; {@}
;``````````````````````````````````````
; expr = ((:if cond1 name1.1 wff1.1 name1.2 wff1.2 ...)
;         (:if cond2 name2.1 wff2.1 name2.2 wff2.2 ...) ...
;         (:else name3.1 wff3.1 name3.2 wff3.2 ...))
; A try-in-sequence step should be skipped if none of the conditions are
; true, and there is no :else branch.
;
  (let* ((lst1 (car expr)) (else1 (if (equal (first lst1) :else) t))
         (cond1 (if (not else1) (second lst1))))
    (cond
      ((null expr) t)
      ((or else1 (eval-truth-value cond1)) nil)
      (t (process-try-in-sequence (cdr expr))))
)) ; END process-try-in-sequence





(defun process-repeat-until (expr) ; {@}
;```````````````````````````````````
; expr = (ep-var cond name1 wff1 name2 wff2 ...)
; A repeat-until step should be skipped if the condition evaluates to true.
;
  (let ((cond1 (first expr)))
    (eval-truth-value cond1)
)) ; END process-repeat-until





(defun interpret-perception-in-context (fact plan-step) ; {@}
;`````````````````````````````````````````````````````````
; Interpret a 'primitive' perception in the context of the current plan step.
; In the case of a perceived "^you say-to.v ^me" action by the user, the relevant
; context is the previous speech act in the plan (typically by Eta), and the user's
; reply is 'automatically' interpreted as a response to that speech act.
;
; e.g., suppose the previous speech act by Eta in the plan is:
; E1 (^me say-to.v ^you '(What is your favorite type of food ?))
;
; And the following action by the user is perceived:
; E2 (^you say-to.v ^me '(Thai food \.))
;
; We want to try to interpret the utterance in E2 using the context of E1 (particularly, the gist-clause
; corresponding to Eta's utterance), extracting a gist-clause and semantic interpretation (when relevant)
; for the user, and possibly additional pragmatic interpretations. After doing so (and storing them in memory),
; we store the fact:
; ((^you reply-to.v E1) ** E2)
;
; TODO REFACTOR : this function could be cleaned up a bit.
;
  (let* (ep-name wff curr-step-wff expr bindings words prev-step prev-step-ep-name prev-step-wff prev-step-gists
         user-gists user-semantics user-pragmatics eta-obligations goal-step ka try-success relative-ep-names
         user-gist-ep-names user-semantic-ep-names user-pragmatics-ep-names)

    (setq ep-name (first fact))
    (setq wff (second fact))

    (setq curr-step-wff (get-step-wff plan-step))

    ; Conditional statement matching different types of 'primitive' observed wffs
    (cond
      ;```````````````````````````
      ; User: Saying -> Replying
      ;```````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^you say-to.v ^me _!) wff))
        (setq expr (get-single-binding bindings))

        ; If say-to.v act has a quoted word list argument, e.g., '(nice to meet you \.)',
        ; it should unquote and decompress the words.
        (cond
          ((quoted-sentence? expr)
            (setq words (decompress (second expr))))
          ; Anything else is unexpected
          (t
            (format t "~%*** SAY-ACTION ~a~%    BY THE USER SHOULD SPECIFY A QUOTED WORD LIST OR VARIABLE" expr)))

        ; Use previous Eta speech act as context for interpretation
        (setq prev-step (find-prev-turn-of-agent (get-^me) (get-ds)))
        (when prev-step
          (setq prev-step-ep-name (dialogue-turn-episode-name prev-step))
          (setq prev-step-gists (dialogue-turn-gists prev-step)))

        ; If current plan step is a relative speech act with past episode(s) as an argument
        ; (e.g., (^you reply-to.v E1)), use the gist-clauses of those episode(s) for interpretation as well
        (when (and (relative-speech-act? curr-step-wff) (not (equal (third curr-step-wff) prev-step-ep-name)))
          (setq relative-ep-names (extract-set (third curr-step-wff)))
          (dolist (relative-ep-name relative-ep-names)
            (setq prev-step-gists (append prev-step-gists
              (get-gist-clauses-characterizing-episode relative-ep-name (get-ds))))))

        (setq prev-step-gists (reverse (remove-duplicates prev-step-gists :test #'equal)))

        (format t "~% ========== User Interpretation ==========")
        (format t "~%  * ETA gist clauses that the user is responding to (from episodes ~a)~%   = ~a "
          (remove nil (cons prev-step-ep-name relative-ep-names)) prev-step-gists)
        (format t "~%  * Using gist clause for context:~%    ~a " (car (last prev-step-gists))) ; DEBUGGING

        ; Compute the "interpretation" (gist clauses) of the user input,
        ; which will be done with a gist-clause packet selected using the
        ; main Eta action clause, and with the user input being the text
        ; to which the tests in the gist clause packet (tree) are applied.
        ;
        ; TODO: In the future, we might instead of or in addition use the semantic
        ; interpretation of Eta's previous speech act.
        ; TODO REFACTOR : now that a reply-to action can be relative to multiple episodes,
        ; we should allow for the use of multiple gist clauses in interpretation.
        (setq user-gists
          (form-gist-clauses-from-input words (car (last prev-step-gists))))

        ; Remove contradicting user gist-clauses (if any)
        (setq user-gists (remove-duplicates (remove-contradiction user-gists) :test #'equal))
        (format t "~%  * Gist clauses for episode ~a:~%   ~a" ep-name user-gists) ; DEBUGGING

        ; Store user gist-clauses in memory and input queue
        (dolist (user-gist user-gists)
          (store-gist-clause-characterizing-episode user-gist ep-name '^you '^me (get-ds)))
        (setq user-gist-ep-names (mapcar (lambda (user-gist) (list ep-name-new user-gist)) user-gists))
        (enqueue-in-buffer-ordered user-gist-ep-names (buffers-gists (ds-buffers (get-ds))))

        ; Obtain semantic interpretation(s) of the user gist-clauses
        (setq user-semantics (remove-duplicates (remove nil
          (mapcar #'form-semantics-from-gist-clause user-gists)) :test #'equal))

        (format t "~%  * Semantics for episode ~a:~%   ~a" ep-name user-semantics) ; DEBUGGING

        ; Store the semantic interpretations in memory and input queue
        (dolist (ulf user-semantics)
          (store-semantic-interpretation-characterizing-episode ulf ep-name '^you '^me (get-ds)))
        (setq user-semantic-ep-names (mapcar (lambda (ulf) (list ep-name-new ulf)) user-semantics))
        (enqueue-in-buffer-ordered user-semantic-ep-names (buffers-semantics (ds-buffers (get-ds))))

        ; Add the fact (^you reply-to.v <prev-step-ep-name>) to context (characterizing ep-name)
        (when prev-step-ep-name
          (store-contextual-fact-characterizing-episode `(^you reply-to.v ,prev-step-ep-name) ep-name (get-ds)))

        ; If current plan step is a relative speech act, add the fact (^you reply-to.v <rel-ep-name>) as well
        (when relative-ep-names
          (store-contextual-fact-characterizing-episode `(^you reply-to.v ,(make-set relative-ep-names :use-and t)) ep-name (get-ds)))

        ; Interpret any additional pragmatic facts from the gist-clause. For instance, the speech act
        ; (^you say-bye-to.v ^me) may be derived from the gist-clause (Goodbye \.).
        (setq user-pragmatics (remove-duplicates (remove nil
          (apply #'append (mapcar #'form-pragmatics-from-gist-clause user-gists))) :test #'equal))

        (format t "~%  * Pragmatics for episode ~a:~%   ~a ~%" ep-name user-pragmatics) ; DEBUGGING

        ; Add each pragmatic wff to context (characterizing ep-name) and input queue
        (dolist (ulf user-pragmatics)
          (store-contextual-fact-characterizing-episode ulf ep-name (get-ds)))
        (setq user-pragmatic-ep-names (mapcar (lambda (ulf) (list ep-name-new ulf)) user-pragmatics))
        (enqueue-in-buffer-ordered user-pragmatic-ep-names (buffers-pragmatics (ds-buffers (get-ds))))

        ; Determine any obligations placed on Eta by utterance (TODO)
        (setq eta-obligations nil)

        ; Log and write dialogue turn
        (log-turn-write (car
          (push (make-dialogue-turn
              :agent (get-^you)
              :utterance words
              :gists user-gists
              :semantics (resolve-references user-semantics)
              :pragmatics user-pragmatics
              :obligations eta-obligations
              :episode-name ep-name)
            (ds-conversation-log (get-ds))))
          (get-io-path))

        ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

      )
      ;````````````````````````````
      ; User: Moving -> Trying
      ;````````````````````````````
      ; TODO REFACTOR : it seems that this is unecessary, provided we can expand a
      ; (^you try1.v (ka ...)) step in the schema to a primitive move.v step, which
      ; we can then unify with the observed move.v step.
      ((setq bindings (bindings-from-ttt-match '(^you action-verb? _*) wff))
        (setq expr (get-multiple-bindings bindings))

        ;; (format t "~%Matched arguments = ~a" expr) ; DEBUGGING

        ; Check context for any current goal step; get reified action from goal step
        (setq goal-step (car (get-from-context '(?ka step1-toward.p ?goal) (get-ds))))
        (setq ka (first goal-step))
        
        (format t "~%Found ka = ~a" ka) ; DEBUGGING

        ; If the (ka ...) consists of an action verb, check arguments of action verb against arguments of
        ; the matched action to check if the action successfully instantiates (ka ...)
        (when (action-verb? (car (second ka)))
          (when (and (equal (length (cdr (second ka))) (length expr))
                     (every (lambda (x y) (equal x y)) (cdr (second ka)) expr))
            (setq try-success t)))

        ;; (format t "~%Match between ~a and ~a successful?: ~a~%" (cdr (second ka)) expr try-success) ; DEBUGGING

        ; If the action was successful, store the following facts in context
        (when try-success
          (store-in-context `((pair ^you ,ep-name) successful.a))
          (store-in-context `((pair ^you ,ep-name) instance-of.p ,ka)))
        
        ; Store (^you try1.v (ka ...)) as characterizing ep-name regardless
        (store-contextual-fact-characterizing-episode `(^you try1.v ,ka) ep-name (get-ds))

      )
      ; Some other wff (currently nothing is done in this case)
      (t nil))

)) ; END interpret-perception-in-context





;``````````````````````````````````````````````````````
;
; [*] SPECIFIC PLAN EXECUTION METHODS
; TODO REFACTOR : should have a separate file allowing for
; implementations of primitive actions
; (maybe specific to each avatar??)
;
;``````````````````````````````````````````````````````





(defun execute-say-to (plan-step expr) ; {@}
;````````````````````````````````````````
; Execute a primitive say-to step.
;
  (let (ep-name wff var-bindings expr-new prev-step prev-step-ep-name
        user-gists eta-gists user-obligations)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    ; If argument is a variable, and in GPT3 generation mode,
    ; bind variable to a generated utterance
    (cond
      ((variable? expr)
        (setq expr-new (if (equal (get-generation-mode *config-agent*) 'GPT3)
          (form-surface-utterance-using-language-model
            :conds (get-facts-for-generation '(dial-schemas))
            :facts (get-facts-for-generation '(epi-schemas memory)))
          nil))
        (push (list expr `(quote ,expr-new)) var-bindings)
        (setq expr expr-new))
      (t (setq expr (flatten (second expr)))))
    
    (setf (ds-count (get-ds)) (1+ (ds-count (get-ds))))
    (setq expr (tag-emotions expr (is-emotion-mode *config-agent*)))

    ; If using GPT3 for gist clause interpretation, add any gists found by GPT3.
    (when (equal (get-interpretation-mode *config-agent*) 'GPT3)
      ; Use previous user speech act as context for interpretation
      (setq prev-step (find-prev-turn-of-agent (get-^you) (get-ds)))
      (when prev-step
        (setq prev-step-ep-name (dialogue-turn-episode-name prev-step))
        (setq user-gists (dialogue-turn-gists prev-step)))
      ; Get Eta gist clauses
      (setq eta-gists (form-gist-clauses-using-language-model expr (car (last user-gists)) '^me))
      (format t "~%  * Storing Eta gist clauses for episode ~a:~%   ~a ~%" ep-name eta-gists) ; DEBUGGING
      ; Store any gist clauses for episode and parent episode
      (mapcar (lambda (gist)
          (when (not (member 'nil gist))
            (store-gist-clause-characterizing-episode gist ep-name '^me '^you (get-ds))))
        eta-gists))

    ; Get any obligations placed on the user from the schema that this episode is part of
    (setq user-obligations (get-step-obligations plan-step))

    ; Log and write dialogue turn
    (log-turn-write (car
      (push (make-dialogue-turn
          :agent (get-^me)
          :utterance expr
          :gists (get-gist-clauses-characterizing-episode ep-name (get-ds))
          :semantics (get-semantic-interpretations-characterizing-episode ep-name (get-ds))
          :pragmatics nil
          :obligations user-obligations
          :episode-name ep-name)
        (ds-conversation-log (get-ds))))
      (get-io-path))

    ;; (print-plan-status (ds-curr-plan (get-ds))) ; DEBUGGING

    ; Add words to output buffer and increment output count
    (set-output-count (1+ (get-output-count)))
    (push-output-buffer expr)

    ; Output words
    (if (member '|Audio| (get-perception-servers *config-agent*))
      (say-words expr (get-io-path) :output-count (get-output-count))
      (print-words expr))

    var-bindings
)) ; END execute-say-to





(defun execute-say-bye () ; {@}
;```````````````````````````
; Write any final buffered output and exit the conversation.
;
  (when (get-output-buffer)
    (write-output-buffer (get-output-buffer) (get-io-path))
    (set-output-buffer nil))
  (set-quit-conversation t)
) ; END execute-say-bye





(defun execute-recall-answer (plan-step user-semantics ans-var) ; {@}
;`````````````````````````````````````````````````````````````````
; Recall an answer to a given spatial query using memory.
;
  (let (object-locations ans var-bindings)
    (setq user-semantics (resolve-references user-semantics))

    ; Get object locations from context
    (setq object-locations (get-from-context '(?x at-loc.p ?y) (get-ds)))
    ;; (format t "found object locations from context: ~a~%" object-locations) ; DEBUGGING

    ; Determine answers by recalling from history
    (setq ans `(quote ,(recall-answer object-locations (eval user-semantics) (get-ds))))
    (format t "recalled answer: ~a~%" ans) ; DEBUGGING

    ; Bind ans to variable given in plan (e.g. ?ans-relations)
    (push (list ans-var ans) var-bindings)
    var-bindings
)) ; END execute-recall-answer




(defun execute-seek-answer-from (plan-step system query) ; {@}
;`````````````````````````````````````````````````````````
; Query a given subsystem by writing a ULF query to the designated IO file.
;
  (setq query (eval (resolve-references query)))

  ; Send query to external source
  (if system (write-subsystem (list query) system (get-io-path)))
) ; END execute-seek-answer-from




(defun execute-receive-answer-from (plan-step system ans-var) ; {@}
;``````````````````````````````````````````````````````````````
; Receive a set of answer relations from a given subsystem, and bind to given variable.
;
  (let (ans var-bindings)
    ; Get answer from subsystem
    (setq ans (read-subsystem system (get-io-path) :block t))
    (if (not (answer-list? ans)) (setq ans nil))
    (if ans (setq ans `(quote ,ans)))

    (format t "received answer: ~a~% (for variable ~a)~%" ans ans-var) ; DEBUGGING

    ; Bind ans to variable given in plan (e.g. ?ans-relations)
    (push (list ans-var ans) var-bindings)
    var-bindings
)) ; END execute-receive-answer-from





(defun execute-commit-to-STM (plan-step expr) ; {@}
;```````````````````````````````````````````````
; Adds a given expression to short term memory (context).
;
  ; Store formula in context
  (store-new-contextual-facts (extract-set expr) (get-ds))
) ; END execute-commit-to-STM





(defun execute-find (plan-step expr) ; {@}
;``````````````````````````````````````
; Finds an action/entity given an indefinite expression, and binds the
; variable to the skolemized entity in the plan.
; 
; TODO: currently assumes that the request must be sent to the
; Spatial-Reasoning-System, but in theory a 'finding' episode could involve
; any specialist server, or possibly even none (e.g., 'finding' in memory).
; The former can be solved somewhat unelegantly by adding the server as an
; argument to this action in the schema; the latter will be more difficult.
;
  (let (ep-name wff sk-var goal-name goal-schema query ans sk-name ep-name1 var-bindings)
    (setq ep-name (get-step-ep-name plan-step))
    (setq wff (get-step-wff plan-step))

    (setq sk-var (second expr))
    ; Get goal name from expr, and corresponding record structure
    (setq goal-name (get-single-binding
      (cdr (bindings-from-ttt-match-deep '(_!1 step1-toward.p _!2) expr))))
    (setq goal-schema (get-record-structure goal-name (get-ds)))
    ; Substitute record structure for goal name in expr
    (setq expr (subst goal-schema goal-name expr))
    ; Request step towards goal schema from BW system
    (setq query `((what.pro be.v (= ,expr)) ?))
    (write-subsystem (list query) '|Spatial-Reasoning-System| (get-io-path))

    ; Get answer from BW system
    ; This is either nil or a list (((ka ...) step1-toward.p ($ ...)))
    (setq ans (read-subsystem '|Spatial-Reasoning-System| (get-io-path) :block t))
    ; Substitute goal name for record structure in ans
    (setq ans (subst goal-name goal-schema ans :test #'equal))
    ; Set sk-name to reified action
    (setq sk-name (get-single-binding
      (bindings-from-ttt-match-deep '(_!1 step1-toward.p _!2) ans)))
    ; Remove existing step1-toward.p propositions from context, and add new one(s)
    (remove-from-context `(?x step1-toward.p ,goal-name) (get-ds))
    (mapcar #'store-in-context ans)
    ; Substitute skolem name for skolem var in schema
    (format t "found ~a for variable ~a~%" sk-name sk-var)

    ; If a step was found for the current episode, store that
    ; the episode was successful in context
    ; TODO: needs to be changed; see comment on 'Eta: Trying' action.
    ;; (setq superstep (plan-subplan-of subplan))
    ;; (setq ep-name1 (get-step-ep-name superstep))
    (setq ep-name1 ep-name)
    (if (and sk-name ep-name1)
      (store-in-context `((pair ^me ,ep-name1) successful.a)))

    ; Bind variable to skolem constant in plan
    (push (list sk-var sk-name) var-bindings)
    var-bindings
)) ; END execute-find





(defun execute-choose (plan-step expr) ; {@}
;`````````````````````````````````````````
; Chooses a referent for an indefinite quantifier, given an episode
; like ?e1 (^me choose.v (a.d ?c (random.a
;            (:l (?x) (and (?x member-of.p ?cc)
;                          (not (^you understand.v ?x)))))))
;
; The lambda abstract is used to select candidates (given the facts stored
; in context), which is optionally preceded by an additional modifier
; (e.g., random.a, (most.mod-a simple.a), etc.) which is used for final
; selection from the candidate list.
;
; The canonical name is substituted for the variable in the rest of the plan
; (a skolem constant is also generated and aliased to the canonical name, but
; is currently unused).
;
  (let (sk-var sk-name var-bindings)

    (setq sk-var (second expr))
    (setq sk-name (choose-variable-restrictions sk-var (third expr)))
    (format t "chose value ~a for variable ~a~%" sk-name sk-var) ; DEBUGGING

    ; Store fact that sk-name chosen in context (removing any existing choice)
    ; TODO: perhaps choose.v actions are 'instantaneous' and should therefore be
    ; removed periodically, along with say-to.v actions etc.
    (remove-from-context '(^me choose.v ?x) (get-ds))
    (store-in-context `(^me choose.v ,sk-name))

    ; Bind variable to skolem constant in plan
    (push (list sk-var sk-name) var-bindings)
    var-bindings
)) ; END execute-choose





(defun execute-form-spatial-representation (plan-step expr) ; {@}
;``````````````````````````````````````````````````````````````
; Form a spatial representation of a concept, given an episode like:
; ?e2 (^me form-spatial-representation.v (a.d ?goal-rep ((most.mod-a simple.a)
;        (:l (?x) (and (?x goal-schema1.n) (?x instance-of.p ?c))))))
;
; First, Eta queries the BW system for the spatial representation, using the indefinite
; quantifier. Then, Eta reads the goal representation from the BW system, generates a name for
; the goal representation, and substitutes it in the schema.
;
  (let (sk-var concept-name concept-schema query ans sk-name goal-schema var-bindings)

    (setq sk-var (second expr))
    ; Get concept name from expr, and corresponding record structure
    (setq concept-name (get-single-binding
      (cdr (bindings-from-ttt-match-deep '(_!1 instance-of.p _!2) expr))))
    (setq concept-schema (get-record-structure concept-name (get-ds)))
    ; Substitute record structure for concept name in expr
    (setq expr (subst concept-schema concept-name expr))
    ; Request goal representation from BW system
    (setq query `((what.pro be.v (= ,expr)) ?))
    (write-subsystem (list query) '|Spatial-Reasoning-System| (get-io-path))

    ; Get answer from BW system
    ; This is a list of relations ((($ ...) goal-schema1.n) (($ ...) instance-of.p ($ ...)))
    (setq ans (read-subsystem '|Spatial-Reasoning-System| (get-io-path) :block t))
    ; Create name for goal representation and add alias
    (setq sk-name (gentemp "BW-goal-rep"))
    (setq goal-schema (get-single-binding
      (bindings-from-ttt-match-deep '(_! goal-schema1.n) ans)))
    (add-alias goal-schema sk-name (get-ds))
    ; Substitute canonical names for record structures in relations
    (setq ans (subst concept-name concept-schema ans :test #'equal))
    (setq ans (subst sk-name goal-schema ans :test #'equal))
    ; Store answer in context
    (mapcar #'store-in-context ans)
    ; Substitute skolem name for skolem var in schema
    (format t "formed representation ~a for variable ~a~%" sk-name sk-var)
    
    ; Bind variable to skolem constant in plan
    (push (list sk-var sk-name) var-bindings)
    var-bindings
)) ; END execute-form-spatial-representation





;``````````````````````````````````````````````````````
;
; [*] CORE REPLANNING FUNCTIONS
;
;``````````````````````````````````````````````````````





(defun check-goal-satisfied (goal) ; {@}
;`````````````````````````````````````
; Check whether a goal is satisfied through process of "self-inquiry",
; i.e. checking context to verify whether the content of the goal
; (assuming, for now, a standard "want that ..." goal) is true in context.
; TODO: should also check knowledge base and/or memory if the goal is
; something like "want that me know ...".
;
  (let (goal-var goal-wff bindings goal-clause)
    (setq goal-var (first goal))
    (setq goal-wff (second goal))

    ; Check goal wff (currently only 'want that ...' goals are supported)
    (cond
      ((setq bindings (bindings-from-ttt-match '(^me want.v (that _!)) goal-wff))
        (setq goal-clause (get-single-binding bindings))
        ; Check if goal clause is true in context
        (if (get-from-context goal-clause (get-ds)) t nil))
      ; Support for tensed version as well
      ((setq bindings (bindings-from-ttt-match '(^me ((pres want.v) (that _!))) goal-wff))
        (setq goal-clause (get-single-binding bindings))
        ; Check if goal clause is true in context
        (if (get-from-context goal-clause (get-ds)) t nil))

      (t
        ;; (format t "~%*** UNSUPPORTED GOAL ~a (~a) " goal-var goal-wff) ; DEBUGGING
        nil))
)) ; END check-goal-satisfied





(defun replan-for-goal (plan-node goal &key plan-var-table schema-instances) ; {@}
;``````````````````````````````````````````````````````````````````````````````
; Given an unsatisfied goal, use a transduction tree to determine an appropriate subplan
; to instantiate and attach to the current plan step.
;
; TODO REFACTOR : replace schema directives with init-plan-from-episode-list,
; to be expanded using schema later, vs. creating schema directly. Do we even
; need schema directives anymore?
;
; USES TT
;
  (let ((curr-step (plan-node-step plan-node))
        goal-var goal-wff bindings goal-words choice schema-name args subplan-node)

    ; Get the goal variable and wff
    (setq goal-var (first goal))
    (setq goal-wff (second goal))

    ; Select subplan for replanning
    (setq choice (choose-result-for goal-wff '*replan-tree*))

    ; Create subplan depending on directive
    (cond
      ; :schema directive
      ((eq (car choice) :schema)
        (setq schema-name (cdr choice))
        (setq subplan-node (plan-subschema schema-name nil)))

      ; :schema+args directive
      ((eq (car choice) :schema+args)
        (setq schema-name (first (cdr choice)) args (second (cdr choice)))
        (setq subplan-node (plan-subschema schema-name args))))
    
    ; If subplan was obtained, expand plan and return t; otherwise return nil
    (when subplan-node
      (expand-plan-node plan-node subplan-node)
      t)

)) ; END replan-for-goal





;``````````````````````````````````````````````````````
;
; [*] CORE MISC FUNCTIONS
; TODO REFACTOR : might be able to move these to util
;
;``````````````````````````````````````````````````````





(defun choose-variable-restrictions (sk-var restrictions) ; {@}
;``````````````````````````````````````````````````````````
; Handles any indefinite quantification of a variable filled
; through a choice  made by Eta, given a list of restrictions.
; 'sk-var' is the variable to be replaced, e.g., '?c'.
; 'restrictions' may be a lambda abstract, possibly preceded by
; some adjective modifier, e.g.,
; (random.a (:l (?x) (and (?x member-of.p ?cc)
;                         (not (^you understand.v ?x)))))
;
  (let (sk-name sk-const lambda-descr modifier candidates)
    (setq sk-const (skolem (implode (cdr (explode sk-var)))))
    ; Allow for initial modifier 
    (when (not (lambda-descr? restrictions))
      (setq modifier (car restrictions)) (setq restrictions (second restrictions)))
    (setq lambda-descr restrictions)
    (setq candidates (find-all-instances-context lambda-descr))
    (format t "given restriction ~a, found candidates ~a~%" lambda-descr candidates) ; DEBUGGING
    (format t "using modifier ~a to choose~%" modifier) ; DEBUGGING
    (setq sk-name (cond
      ((equal modifier 'random.a)
        (nth (random (length candidates)) candidates))
      (t (car candidates))))
    sk-name)
) ; END choose-variable-restrictions





(defun obviated-question (sentence eta-action-name) ; {@}
;````````````````````````````````````````````````````
; Check whether this is a (quoted, bracketed) question.
; If so, check what facts, if any, are stored in gist-kb-user under 
; the 'topic-keys' obtained as the value of that property of
; 'eta-action-name'. If there are such facts, check if they
; seem to provide an answer to the gist-version of the question.
; NOTE: modified to check if gist clause contains question rather than surface
; sentence (B.K. 4/17/2020)
; TODO REFACTOR : can this be replaced with more generic method for detecting obviated questions?
;
  (let (gist-clauses topic-keys facts)
    ;; (format t "~% ****** input sentence: ~a~%" sentence)
    (setq gist-clauses (get-gist-clauses-characterizing-episode eta-action-name (get-ds)))
    ;; (format t "~% ****** gist clauses are ~a **** ~%" gist-clauses)
    ;; (format t "~% ****** quoted question returns ~a **** ~%" (some #'question? gist-clauses)) ; DEBUGGING
    (if (not (some #'question? gist-clauses))
      (return-from obviated-question nil))
    (setq topic-keys (get eta-action-name 'topic-keys))
    ;; (format t "~% ****** topic key is ~a ****** ~%" topic-keys) ; DEBUGGING
    (if (null topic-keys) (return-from obviated-question nil))
    (setq facts (remove nil (mapcar (lambda (key) (gethash key (ds-gist-kb-user (get-ds)))) topic-keys)))
    ;; (format t "~% ****** gist-kb ~a ****** ~%" (ds-gist-kb-user (get-ds)))
    ;; (format t "~% ****** list facts about this topic = ~a ****** ~%" facts)
    ;; (format t "~% ****** There is no fact about this topic. ~a ****** ~%" (null facts)) ; DEBUGGING
    (if (null facts) (return-from obviated-question nil))
    ; We have an Eta question, corresponding to which we have stored facts
    ; (as user gist clauses) that seem topically relevant.
    ; NOTE: in this initial version, we don't try to verify that the facts
    ; actually obviate the question, but just assume that they do. 
  facts)
) ; END obviated-question





(defun obviated-action (eta-action-name) ; {@}
;`````````````````````````````````````````
; Check whether this is an obviated action (such as a schema instantiation),
; i.e. if the action has a topic-key(s) associated, check if any facts are stored
; in gist-kb-user under the topic-key(s). If there are such facts, we assume that
; these facts obviate the action, so the action can be deleted from the plan.
; TODO REFACTOR : can this be replaced with more generic method for detecting obviated actions?
;
  (let (topic-keys facts)
    (setq topic-keys (get eta-action-name 'topic-keys))
    ;; (format t "~% ****** topic key for ~a is ~a ****** ~%" eta-action-name topic-keys) ; DEBUGGING
    (if (null topic-keys) (return-from obviated-action nil))
    (setq facts (remove nil (mapcar (lambda (key) (gethash key (ds-gist-kb-user (get-ds)))) topic-keys)))
    ;; (format t "~% ****** gist-kb ~a ****** ~%" (ds-gist-kb-user (get-ds)))
    ;; (format t "~% ****** list facts about this topic = ~a ****** ~%" facts)
    ;; (format t "~% ****** There is no fact about this topic. ~a ****** ~%" (null facts)) ; DEBUGGING
    (if (null facts) (return-from obviated-action nil))
  facts)
) ; END obviated-action





;``````````````````````````````````````````````````````
;
; [*] CORE INTERPRETATION/GENERATION
; TODO REFACTOR : should move these to separate nlp module
;
;``````````````````````````````````````````````````````





(defun get-history-for-generation () ; {@}
;````````````````````````````````````````````
; Gets the dialogue history for use in response generation.
;
  (let (history-agents history-utterances history)
    (setq history-agents (reverse (mapcar #'dialogue-turn-agent (ds-conversation-log (get-ds)))))
    (setq history-utterances
      (reverse (mapcar #'untag-emotions (mapcar #'dialogue-turn-utterance (ds-conversation-log (get-ds))))))
    (setq history (mapcar (lambda (agent utterance) (list agent utterance)) history-agents history-utterances))
    history
)) ; END get-history-for-generation





(defun get-prev-utterance-for-generation (history) ; {@}
;`````````````````````````````````````````````````````
; Gets the previous utterance for use in response generation (or creates a generic one
; if no previous utterance exists).
;
  (let (prev-utterance)
    (setq prev-utterance (second (car (last
      (remove-if (lambda (turn) (equal (first turn) (get-^me))) history)))))
    (when (null prev-utterance)
      (setq prev-utterance '(Hello \.)))
    prev-utterance
)) ; END get-history-for-generation





(defun get-facts-for-generation (types) ; {@}
;````````````````````````````````````````
; Retrieve facts for use in response generation. 
; The type of facts to be used must be specified using the 'types' argument; either:
; 'dial-schemas : use certain conditions and goals of the current dialogue schema(s).
; 'epi-schemas  : use a subset of facts contained within a retrieved epi-schema (representing a
; 'memory       : use a subset of facts retrieved from memory based on similarity with previous turn.
; Or a list of any of these types.
;
  (let ((schemas (get-schema-instance-ids (ds-curr-plan (get-ds)))) schema-instance
        rigid-conds static-conds preconds goals relevant-memory query-str facts
        relevant-epi-schemas relevant-epi-schema-knowledge)

    ; Get query string for retrieval
    (setq query-str (words-to-str (get-prev-utterance-for-generation (get-history-for-generation))))

    (when (atom types) (setq types (list types)))

    ; Get relevant dialogue schema knowledge (conditions/goals/etc.)
    (when (member 'dial-schemas types)
      ; TODO: add other relevant schema categories here in the future
      (dolist (schema schemas)
        (setq schema-instance (gethash schema (ds-schema-instances (get-ds))))
        (setq rigid-conds (append rigid-conds (get-schema-section-wffs schema-instance :rigid-conds)))
        (setq static-conds (append static-conds (get-schema-section-wffs schema-instance :static-conds)))
        (setq preconds (append preconds (get-schema-section-wffs schema-instance :preconds)))
        (setq goals (append goals (get-schema-section-wffs schema-instance :goals))))

      (format t "~%  * Generating response using schemas: <~a> "
        (str-join (mapcar (lambda (schema)
          (string (schema-predicate (gethash schema (ds-schema-instances (get-ds)))))) schemas) #\,)) ; DEBUGGING
    )

    ; Get relevant habitual/event schema knowledge
    (when (and (member 'epi-schemas types) (get-use-embeddings))
      (setq relevant-epi-schemas (reverse (retrieve-relevant-epi-schemas query-str *embedding-path*)))

      (format t "~%  * Generating response using retrieved epi-schemas~%      (from \"~a\"):~%   <~a> "
        query-str (str-join (mapcar (lambda (schema) (string (schema-predicate schema))) relevant-epi-schemas) #\,)) ; DEBUGGING

      ; TODO: ensure that header precedes the facts for each schema
      (setq relevant-epi-schema-knowledge (apply #'append (mapcar (lambda (schema)
          (retrieve-relevant-schema-facts query-str schema *embedding-path*))
        relevant-epi-schemas)))

      (format t "~%  * Using the following facts from the retrieved epi-schemas:~%   ~a " relevant-epi-schema-knowledge) ; DEBUGGING
    )

    ; Get relevant episodic memory
    (when (and (member 'memory types) (get-use-embeddings))
      (setq relevant-memory (reverse (retrieve-relevant-knowledge-from-kb query-str *embedding-path*)))

      (format t "~%  * Generating response using retrieved facts~%      (from \"~a\"):~%   ~a " query-str relevant-memory) ; DEBUGGING
    )

    ; Combine facts and convert to strings
    (setq facts (remove-duplicates
      (append relevant-epi-schema-knowledge relevant-memory rigid-conds static-conds preconds goals) :test #'equal))

    facts
)) ; END get-facts-for-generation





(defun form-surface-utterance-using-language-model (&key conds facts gist mode) ; {@}
;````````````````````````````````````````````````````````````````````````````````
; Generate a surface utterance using a language model (currently, GPT-3).
;
; This will automatically generate a prompt using the given conditions and facts.
;
; In the case where a particular Eta gist-clause is given as input, this will be
; treated as a paraphrasing task: the prompt will be followed by several examples
; of paraphrases (relevant examples are retrieved using pattern transduction based
; on the given gist-clause), and the model will be prompted to paraphrase the
; given gist-clause in the context of the previous user utterance.
;
; In the case where gist-clause is nil, this will be treated as unconstrained
; generation: the prompt will be followed by the full dialogue hisory, and the
; model will be prompted to generate the next response.
;
; :mode 'question or :mode 'statement can be provided as a keyword argument to condition
; the LLM to generate either a question or statement, respectively (by default, the model
; may generate either).
;
; USES TT
;
  (let (utterance prev-utterance incomplete-utterances history conds-str facts-str
        history-str choice examples examples-str emotion ^me ^you)

    (setq ^me (get-^me))
    (setq ^you (get-^you))

    ; Split off emotion in given gist clause (if any)
    (when gist
      (setq emotion (first (split-emotion-tag gist)))
      (if emotion (setq gist (second (split-emotion-tag gist)))))

    ;; (format t "~%  * Generating response using gist clause: ~a " gist) ; DEBUGGING

    ; Get history and previous utterance
    (setq history (get-history-for-generation))
    (setq prev-utterance (get-prev-utterance-for-generation history))

    ; Convert facts and history to strings
    (setq conds-str (remove nil (mapcar (lambda (cond) (expr-to-str cond :^me ^me :^you ^you)) conds)))
    (setq facts-str (remove nil (mapcar (lambda (fact) (expr-to-str fact :^me ^me :^you ^you)) facts)))
    (setq history-str (mapcar (lambda (turn) (list (string (first turn)) (words-to-str (second turn)))) history))

    ; Generate response
    (cond
      ; Gist clause given: Paraphrase task
      (gist

        ; Get example strings
        (setq choice (choose-result-for gist '*paraphrase-prompt-examples-tree*))
        (when (eq (car choice) :prompt-examples)
          (setq examples (cdr choice)))
        (setq examples-str
          (mapcar (lambda (example) (mapcar #'words-to-str example)) examples))

        ; Get any Eta utterances that came since the previous user utterance (if any)
        (when (member ^you history :key (lambda (x) (first x)))
          (setq incomplete-utterances (mapcar #'second
            (last history (position ^you (reverse history) :key (lambda (x) (first x)))))))
        
        ; Get utterance
        (setq utterance (get-gpt3-paraphrase conds-str facts-str examples-str
          (words-to-str prev-utterance)
          (words-to-str gist)
          ^me ^you
          :incomplete-utterance (str-join (mapcar #'words-to-str incomplete-utterances) #\ )
          :mode mode)))


      ; No gist clause: Unconstrained generation task
      (t

        ; Get utterance
        (setq utterance (get-gpt3-response
          conds-str
          facts-str
          history-str
          ^me ^you
          :mode mode))))

    ;; (format t "~%  * Generated utterance = ~a" utterance) ; DEBUGGING   

    ; Generate emotion tag for utterance (if enabled)
    ; If the gist clause already had an emotion specified, use that instead of generating tag
    (when (is-emotion-mode *config-agent*)
      (if (null emotion)
        (setq emotion (get-gpt3-emotion
          (words-to-str utterance)
          (last history-str 3)
          ^me ^you)))
      (setq utterance (cons emotion utterance)))

    ;; (format t "~%  * Generated utterance = ~a" utterance) ; DEBUGGING   

    utterance
)) ; END form-surface-utterance-using-language-model





(defun form-surface-utterance-from-gist-clause (gist-clause prior-gist-clause) ; {@}
;``````````````````````````````````````````````````````````````````````````````
; Given a gist-clause by Eta, elaborate it into a surface utterance using the
; context provided by the prior gist-clause using choice trees.
;
; First this uses Eta's gist-clause to select a relevant subtree for response
; generation, then it uses the context of the previous gist-clause to select
; the surface utterance.
;
; USES TT
;
  (let (choice relevant-tree utterance)

    ; Get the relevant pattern transduction tree given Eta's gist clause
    ;````````````````````````````````````````````````````````````````````````````````````````````````
    ;; (format t "~% gist-clause = ~a" gist-clause) ; DEBUGGING
    (setq choice (choose-result-for gist-clause '*gist-clause-trees-for-response*))

    ; Get the surface utterance using context (if applicable)
    ;````````````````````````````````````````````````````````````````````````````````````````````````
    (cond
      ; null choice; simply return the gist clause as the surface utterance
      ((null choice)
        (setq utterance gist-clause))

      ; :out directive; output utterance directly without using context
      ((eq (car choice) :out)
        (setq utterance (cdr choice)))

      ; :subtrees directive; use first subtree to select response based on context
      ((eq (car choice) :subtrees)
        (setq relevant-tree (cadr choice))
        ;; (format t "~% relevant tree = ~a" relevant-tree) ; DEBUGGING   
        ;; (format t "~% prior-gist-clause = ~a" prior-gist-clause) ; DEBUGGING
        (setq utterance (cdr
          (choose-result-for prior-gist-clause relevant-tree)))))

    ;; (format t "~% utterance = ~a" utterance) ; DEBUGGING   

    utterance
)) ; END form-surface-utterance-from-gist-clause





(defun form-gist-clauses-using-language-model (words prior-gist-clause &optional (agent '^you)) ; {@}
;`````````````````````````````````````````````````````````````````````````````````````````````````
; Extract gist clauses from words using a language model (currently, GPT-3), given
; the context of the previous gist clause.
;
; This will generate a prompt depending on the previous gist clause, using the
; rules stored at *gist-prompt-examples-tree*. This will allow the system to provide
; a few relevant examples of gist clause extraction.
;
; If the argument agent is given as '^me (i.e., interpreting an Eta utterance), the pronouns in
; the prior gist clause will be swapped in order to match the correct frame of reference. If the
; agent is '^you (i.e., interpreting a user utterance), the pronouns in the resulting gist clause
; will be swapped.
;
; USES TT
;
  (let (choice examples examples-str gist-clauses)

    ; Get example strings
    (setq choice (choose-result-for prior-gist-clause '*gist-prompt-examples-tree*))
    (when (eq (car choice) :prompt-examples)
      (setq examples (cdr choice)))
    (setq examples-str
      (mapcar (lambda (example) (mapcar #'words-to-str example)) examples))

    ; If no (non-nil) prior gist clause, create a generic one
    (when (or (null prior-gist-clause) (member 'NIL prior-gist-clause))
      (setq prior-gist-clause '(Hello \.)))

    ; If Eta is agent, swap pronouns in prior gist clause
    (when (equal agent '^me)
      (setq prior-gist-clause (swap-duals prior-gist-clause)))
    
    ; Get gist clauses
    (setq gist-clauses (get-gpt3-gist examples-str
      (words-to-str (untag-emotions words))
      (words-to-str prior-gist-clause)))

    ; If Eta is agent, swap pronouns in each resulting gist clause
    (when (equal agent '^you)
      (setq gist-clauses (mapcar #'swap-duals gist-clauses)))

    ;; (format t "~% gist-clauses = ~a" gist-clauses) ; DEBUGGING   

    gist-clauses
)) ; END form-gist-clauses-using-language-model





(defun form-gist-clauses-from-input (words prior-gist-clause) ; {@}
;``````````````````````````````````````````````````````````````
; Find a list of gist-clauses corresponding to the user's 'words',
; interpreted in the context of 'prior-gist-clause' (usually a
; question output by the system). Use hierarchically related 
; choice trees for extracting gist clauses.
;
; The gist clause extraction patterns will be similar to the
; ones in the choice packets for reacting to inputs, used in
; the previous version; whereas the choice packets for reacting
; will become simpler, based on the gist clauses extracted from
; the input.
;
; - look for a final question -- either yes-no, starting
;   with auxiliary + "you{r}", or wh-question, starting with
;   a wh-word and with "you{r}" coming within a few words.
;   "What about you" isa fairly common pattern. (Sometimes the
;   wh-word is not detected but "you"/"your" is quite reliable.)
;   The question, by default, is reciprocal to Eta's question.
;
; USES TT
;
  (let ((n (length words)) relevant-trees sentences
        specific-trees thematic-trees facts gist-clauses)

    ; Get the relevant pattern transduction tree given the gist clause of Eta's previous utterance.
    ;````````````````````````````````````````````````````````````````````````````````````````````````
    ;; (format t "~% prior-gist-clause = ~a" prior-gist-clause) ; DEBUGGING
    (setq relevant-trees (cdr
      (choose-result-for prior-gist-clause '*gist-clause-trees-for-input*)))
    ;; (format t "~% this is a clue == ~a" (choose-result-for prior-gist-clause
    ;;   '*gist-clause-trees-for-input*))
    ;; (format t "~% relevant trees = ~a" relevant-trees) ; DEBUGGING   

    ; A subtree is specific by default, or if in list with :specific keyword; if in a list with
    ; the :thematic keyword, it's treated as a thematic tree.
    (dolist (tree relevant-trees)
      (cond
        ((and (listp tree) (equal :thematic (first tree)))
          (push (second tree) thematic-trees))
        ((and (listp tree) (equal :specific (first tree)))
          (push (second tree) specific-trees))
        (t (push tree specific-trees))))

    ;; ; Get the list of gist clauses from the user's utterance, using the contextually
    ;; ; relevant pattern transduction tree.
    ;; ;```````````````````````````````````````````````````````````````````````````````````````````````````````
    ;; (setq facts (cdr (choose-result-for words relevant-tree)))
    ;; (format t "~% gist clauses = ~a" facts) ; DEBUGGING

    ; Split user's reply into sentences for extracting specific gist clauses
    ;`````````````````````````````````````````````````````````````````````````
    (setq sentences (split-sentences words))
    (dolist (specific-tree (reverse specific-trees))
      (dolist (sentence sentences)
        (setq clause (cdr (choose-result-for sentence specific-tree)))
        (when (atom (car clause)) (setq clause (list clause))) ; in case no topic-key
        (when clause
          (setq keys (second clause))
          (store-gist (car clause) keys (ds-gist-kb-user (get-ds)))
          (push (car clause) facts))))

    ; Form thematic answer from input (if no specific facts are extracted)
    ;``````````````````````````````````````````````````````````````````````
    (when (and thematic-trees (> (length sentences) 2) (null facts))
      (dolist (thematic-tree (reverse thematic-trees))
        (setq clause (cdr (choose-result-for words thematic-tree)))
        (when (atom (car clause)) (setq clause (list clause))) ; in case no topic-key
        (when clause
          (setq keys (second clause))
          (store-gist (car clause) keys (ds-gist-kb-user (get-ds)))
          (push (car clause) facts))))

    ; 'facts' should be a concatenation of the above results in the order in
    ; which they occur in the user's input; in reacting, Eta will
    ; pay particular attention to the first clause, and any final question.
    (setq gist-clauses (remove-duplicates (remove nil (reverse facts)) :test #'equal))

    ; If using GPT3 gist interpretation mode, use GPT3 to extract additional gist clause(s).
    (when (equal (get-interpretation-mode *config-agent*) 'GPT3)
      (setq gist-clauses (append gist-clauses
        (form-gist-clauses-using-language-model words prior-gist-clause '^you))))

    ; If no gist clause, return (NIL Gist) in order to allow processing.
    (when (or (null gist-clauses) (equal gist-clauses '(NIL)))
      (setq gist-clauses (list '(NIL Gist))))

    ;; (format t "~% extracted gist clauses: ~a" gist-clauses) ; DEBUGGING
	
	  gist-clauses
)) ; END form-gist-clauses-from-input





(defun form-semantics-from-gist-clause (clause) ; {@}
;``````````````````````````````````````````````````
; Find the ULF corresponding to the user's 'clause' (a gist clause).
;
; Use hierarchical choice trees for extracting the ULF, starting from
; the root *clause-semantics-tree*.
;
; If no ULF is obtained from hierarchical pattern transduction, and the
; BLLIP-based symbolic ULF parser is enabled as a dependency, then use
; the parser to obtain a ULF.
;
; USES TT
;
  (let (ulf)
    (setq ulf (choose-result-for clause '*clause-semantics-tree*))

    ; If using BLLIP parser mode and no ULF, use parser to obtain ULF.
    (when (equal (get-parser-mode *config-agent*) 'BLLIP)
      (setq ulf (parse-str-to-ulf-bllip (words-to-str clause))))

  ulf)
) ; END form-semantics-from-gist-clause





(defun form-pragmatics-from-gist-clause (clause) ; {@}
;``````````````````````````````````````````````````
; Interpret additional (secondary) pragmatic ULFs from the
; user's 'clause' (a gist clause). E.g., the direct semantic
; interpretation of the utterance "Goodbye" might simply be
; the ULF (goodbye.gr), but the pragmatic interpretation might
; be (^you say-bye-to.v ^me).
;
; Use hierarchical choice trees for extracting the ULFs, starting
; from the root *clause-pragmatics-tree*.
;
; USES TT
;
  (let (wff pragmatics)
    (setq wff (choose-result-for clause '*clause-pragmatics-tree*))
    ; If sentential-level conjunction, split into multiple WFFs
    (setq pragmatics (extract-set wff))
  pragmatics
)) ; END form-pragmatics-from-gist-clause





(defun eval-truth-value (wff) ; {@}
;```````````````````````````````
; Evaluates the truth of a conditional schema action.
; This assumes a CWA, i.e., if something is not found in
; context, it is assumed to be false.
;
  (cond
    ; (wff1 = wff2)
    ((equal-prop? wff)
      (setq wff (eval-functions wff))
      (equal (first wff) (third wff)))
    ; (not wff1)
    ((not-prop? wff)
      (not (eval-truth-value (second wff))))
    ; (wff1 and wff2)
    ((and-prop? wff)
      (and (eval-truth-value (first wff))
           (eval-truth-value (third wff))))
    ; (wff1 or wff2)
    ((or-prop? wff)
      (or  (eval-truth-value (first wff))
           (eval-truth-value (third wff))))
    ; (wff1 ** e) - check memory
    ((characterizes-prop? wff)
      (get-from-memory wff (get-ds)))
    ; Otherwise, check to see if wff is true in context
    (t (get-from-context wff (get-ds)))
)) ; END eval-truth-value





(defun choose-result-for (clause rule-node) ; {@}
;`````````````````````````````````````````````
; This is just the top-level call to 'choose-result-for', with
; no prior match providing a value of 'parts', i.e., 'parts' = nil;
; this is to enable tracing of just the top-level calls
  (choose-result-for1 clause nil rule-node nil)
) ; END choose-result-for





(defun choose-result-for1 (clause parts rule-node visited-subtrees) ; {@}
;``````````````````````````````````````````````````````````````````````````
; This is a generic choice-tree search program, used both for
; (i) finding gist clauses in user inputs (starting with selection
; of appropriate subtrees as a function of Eta's preceding
; question, simplified to a gist clause), and (ii) in selecting
; outputs in response to (the gist clauses extracted from) user 
; inputs. Outputs in the latter case may be verbal responses
; obtained with reassembly rules, or names (possibly with
; arguments) of other choice trees for response selection, or
; the names (possibly with arguments) of schemas for planning 
; an output. The program works in essentially the same way for
; purposes (i) and (ii), but returns
;      (cons <directive keyword> result)
; where the directive keyword (:out, :subtree, :subtree+clause,
; :schema, ...) is the one associated with the rule node that
; provided the final result to the calling program. (The calling
; program is presumed to ensure that the appropriate choice tree
; is supplied  as 'rule-node' argument, and that the result is
; interpreted and used as intended for that choice tree.)
;
; So, given an input clause 'clause', a list 
; 'parts' of matched parts from application of the superordiate
; decomposition rule (initially, nil), and the choice tree node 
; 'rule-node' in a tree of decomposition/result rules, we generate
; a verbal result or other specified result starting at that rule,
; prefixed with the directive keyword.
;
; Decomposition rules (as opposed to result rules) have no
; 'directive' property (i.e., it is NIL). Note that in general
; a decomposition rule will fail if the pattern it supplies fails
; to match 'clause', while a result rule will fail if its
; latency requirements prevent its (re)use until more system
; outputs have been generated. (This avoids repetitive outputs.)
;
; Note also that result rules can have siblings, but not children,
; since the "downward" direction in a choice tree corresponds to
; successive refinements of choices. Further, note that if the
; given rule node provides a decomposition rule (as indicated by
; a NIL 'directive' property), then it doesn't make any direct
; use of the 'parts' list supplied to it -- it creates its own
; 'newparts' list via a new pattern match. However, if this
; match fails (or succeeds but the recursion using the children 
; returns NIL), then the given 'parts' list needs to be passed
; to the siblings of the rule node -- which after all may be 
; result rules, in particular reassembly rules.
;
; Method:
;````````
; If the rule has a NIL 'directive' property, then its 'pattern'
; property supplies a decomposition rule. We match this pattern,
; and if successful, recursively seek a result from the children
; of the rule node (which may be result rules or further decomp-
; osition rules), returning the result if it is non-nil; in case
; of failure, we recursively return a result from the siblings
; of the rule node (via the 'next' property); these siblings
; represent alternatives to the current rule node, and as such
; may be either alternative decomposition rules, or result rules 
; (with a non-nil 'directive' property) -- perhaps intended as
; a last resort if the decomposition rules at the current level
; fail.
;
; In all cases of non-nil directives, if the latency requirement
; is not met, i.e., the rule cannot be reused yet, the recursive
; search for a result continues with the siblings of the rule.
;
; If the rule node has directive property :out, then its 'pattern'
; property supplies a reassembly rule. If the latency requirement 
; of the rule is met, the result based on the reassembly rule and
; the 'parts' list is returned (after updating time-last-used). 
; The latency criterion uses the 'latency' property of 'rule-node' 
; jointly with the (ds-time-last-used (get-ds)) hash table and the
; result count, (ds-count (get-ds)).
;
; If the rule node has directive property :subtree, then 'pattern'
; will just be the name of another choice tree. If the latency 
; requirement is met, a result is recursively computed using the
; named choice tree (with the same 'clause' as input).
; The latency will usually be 0 in this case, i.e., a particular
; choice subtree can usually be used again right away.
;
; If the rule node has directive property :subtree+clause, then
; 'pattern' supplies both the name of another choice tree and
; a reassembly pattern to be used to construct a clause serving
; as input in the continued search (whereas for :subtree the
; recursion continues with the original clause). Again the
; latency will usually be 0.
;
; (June 9/19) If the rule node has directive property :ulf-recur,
; then 'pattern' supplies two reassembly rules, the first of which,
; upon instantiation with 'parts', is a list such as
;  ((*be-ulf-tree* ((is be pres))) 
;   (*np-ulf-tree* (the det def) (Nvidia name corp-name) (block cube obj))
;   (*rel-ulf-tree* (to prep dir loc) (the det def) (left noun loc) (of prep))
;   (*np-ulf-tree* (a det indef) (red adj color) (block cube obj)) 
;   (*end-punc-ulf-tree* (? end-punc ques-punc))),
; ie., a list of sublists of words, with each sublist prefaced by
; the name of a rule tree to be used to produce a ulf for that sublist of
; words. The instantiated reassembly rule is then processed
; further, by successively trying to get a result for each of the rule
; trees named in the sublists; if all succeed, the individual results
; are assembled into an overall ULF, and this is the result returned
; (otherwise, the result is nil -- failure). The second reassembly rule
; provides the right bracketing structure for putting together the
; individual ULFs. Example: ((1 2 (3 4)) 5); result for the above:
;     (((pres be.v) (the.d (|NVidia| block.n)) 
;                   (to_the_left_of.p (a.d (red.a block.n)))) ?)
;
; Other directives (leading to direct return of a successful result
; rather than possible failure, leading to continuing search) are 
; - :subtrees (returning the names of multiple subtrees (e.g., 
;   for extracting different types of gist clauses from a 
;   potentially lengthy user input); 
; - :schema (returning the name of a schema to be instantiated, 
;   where this schema requires no arguments); 
; - :schemas (returning multiple schema names, perhaps as 
;   alternatives); 
; - :schema+args (a schema to be instantiated for the specified 
;   args derived from the given 'clause'); 
; - :gist (a gist clause extracted from the given 'clause,
;   plus possibly a list of topic keys for storage);
; - :ulf (June 9/19) (returning a ulf for a phrase simple enough
;   to be directly interpreted);
; - perhaps others will be added, such as :subtrees+clauses or
;   :schemas+args
;
; These cases are all treated uniformly -- a result is returned
; (with the directive) and it is the calling program's responsib-
; ility to use it appropriately. Specifically, if the latency
; requirement is met, the value supplied as 'pattern', instantiated
; with the supplied 'parts', is returned. (Thus integers appearing
; in the value pattern are interpreted as references to parts
; obtained from the prior match.) 
;
; The function maintains a list of visited subtrees
; (for a particular path) to avoid entering infinite recursion, as well
; as a list of matched nodes that are returned for debugging purposes.
; (NOTE: the latter is not yet implemented for ULF directives)
;

  ; First make sure we have the lexical code needed for ULF computation
  (if (not (fboundp 'eval-lexical-ulfs)) (load "eval-lexical-ulfs.lisp"))

  (let (directive pattern newparts newparts-option newclause ulf ulfs result)
    ; Don't use empty choice trees
    (if (null rule-node) (return-from choose-result-for1 nil))

    ; Get directive and pattern from rule node
    (setq directive (get rule-node 'directive))
    (setq pattern (get rule-node 'pattern))

    ; If latency is being enforced, skip rule if it was used too recently
    (when (and directive *use-latency*
            (< (ds-count (get-ds)) (+ (get-time-last-used (get-ds) rule-node)
                          (get rule-node 'latency))))
      (return-from choose-result-for1
        (choose-result-for1 clause parts (get rule-node 'next) visited-subtrees)))

    ;; (format t "~% ***1*** Clause = ~a ~%" clause) ; DEBUGGING
    ;; (format t "~% =====2==== Pattern/output to be matched in rule ~a = ~%  ~a and directive = ~a" rule-node pattern directive) ; DEBUGGING
  
    ; Big conditional statement for dealing with all possible directives.
    ; We first deal with cases requiring further tree-descent (with possible
    ; failure and thus recursive backtracking), no directive (i.e. decomposition
    ; rule), :subtree, :subtree+clause, and :ulf-recur
    (cond
      ;``````````````````
      ; No directive
      ;``````````````````
      ; Look depth-first for more specific match, otherwise try alternatives
      ((null directive)
        (cond
          ; If pattern is disjunctive, try to match any option within the disjunction
          ((equal (car pattern) :or)
            (dolist (pattern-option (cdr pattern))
              (setq newparts-option (match1 pattern-option clause))
              (when (and (null newparts) newparts-option)
                (setq newparts newparts-option))))
          ; If pattern is a subtree to match, try to match that subtree
          ((equal (car pattern) :subtree)
            (when (and (atom (second pattern)) (not (member (second pattern) visited-subtrees)))
              (setq newparts-option
                (choose-result-for1 clause parts (second pattern) (cons (second pattern) visited-subtrees)))
              (if newparts-option (setq newparts '(:seq)))))
          ; Otherwise, try to match pattern
          (t (setq newparts (match1 pattern clause))))

        ;; (format t "~% ----3---- new part = ~a ~%" newparts) ; DEBUGGING

        ; Pattern does not match 'clause', search siblings recursively
        (if (null newparts)
          (return-from choose-result-for1
            (choose-result-for1 clause parts (get rule-node 'next) visited-subtrees)))

        ; Pattern matched, try to obtain recursive result from children
        (setq result
          (choose-result-for1 clause newparts (get rule-node 'children) visited-subtrees))

        (if result (return-from choose-result-for1 result)
                   (return-from choose-result-for1
                      (choose-result-for1 clause parts (get rule-node 'next) visited-subtrees))))

      ;`````````````````````
      ; :subtree directive
      ;`````````````````````
      ; Recursively obtain a result from the choice tree specified via its
      ; root name, given as 'pattern'
      ((eq directive :subtree)
        (set-time-last-used (get-ds) rule-node (ds-count (get-ds)))
        (cond
          ; Pattern is wrong format
          ((not (atom pattern)) (return-from choose-result-for1 nil))
          ; If subtree was already visited, skip rule
          ((member pattern visited-subtrees)
            (return-from choose-result-for1
              (choose-result-for1 clause parts (get rule-node 'next) visited-subtrees)))
          ; Otherwise, go to subtree and add subtree to list of visited subtrees
          (t (return-from choose-result-for1
            (choose-result-for1 clause parts pattern (cons pattern visited-subtrees))))))

      ;````````````````````````````
      ; :subtree+clause directive
      ;````````````````````````````
      ; Similar to :subtree, except that 'pattern' is not simply the root
      ; name of a tree to be searched, but rather a pair of form
      ; (<root name of tree> <reassembly pattern>), indicating that the
      ; reassembly pattern should be used together with 'parts' to reassemble
      ; some portion of 'clause', whose results should then be used
      ; (after re-tagging) in the recursive search.
      ((eq directive :subtree+clause)
        (set-time-last-used (get-ds) rule-node (ds-count (get-ds)))
        (setq newclause (fill-template (second pattern) parts))
        (return-from choose-result-for1
          (choose-result-for1 newclause nil (car pattern)
            (remove-duplicates (cons (car pattern) visited-subtrees)))))

      ;````````````````````````
      ; :ulf-recur directive
      ;````````````````````````
      ; Find the instance of the rule pattern determined by 'parts',
      ; which will be a shallow analysis of a text segment, of the
      ; form described in the initial commentary; try to find results
      ; (ULFs) for the component phrases, and if successful assemble
      ; these into a complete ULF for the input. NB: (first pattern)
      ; supplies the top-level phrasal segments to be further analyzed
      ; (using the ulf rule trees heading each phrasal segment), while
      ; (second pattern) supplies the bracketing structure for the
      ; phrasal ULFs.
      ((eq directive :ulf-recur)
        ; Instantiate shallow analysis
        (setq newclause (fill-template (first pattern) parts))
        ; Interpret recursive phrases; the car of each nonatomic phrase
        ; either gives the name of the relevant rule tree to use, or it
        ; is 'lex-ulf@'; in the former case we proceed recursively, in
        ; the latter we keep the phrase as-is
        (dolist (phrase newclause)
          (if (or (atom phrase) (eq (car phrase) 'lex-ulf@))
            (setq ulf phrase) ; e.g., ulf of (next to) = next_to.p
            (setq ulf
              (choose-result-for (cdr phrase) (car phrase))))
          ; If failure, exit loop
          (when (null ulf)
            (setq ulfs nil)
            (return-from choose-result-for1 nil))
          (push ulf ulfs))
        ; Assemble the (initially reversed) list of phrasal ULFs into a
        ; ULF for the entire input, using the second reassembly rule
        (when ulfs
          (setq n (length ulfs)) ; number of phrasal ULFs
          (setq result (second pattern)) ; bracket structure with indices
          (dolist (ulf ulfs)
            (setq ulf (eval-lexical-ulfs ulf))
            (setq result (subst ulf n result))
            (decf n)))
        (return-from choose-result-for1 result))

      ; Now we deal with cases expected to directly return a result,
      ; not requiring allowance for failure-driven backtracking

      ;`````````````````
      ; :ulf directive
      ;`````````````````
      ; In the case of ULF computation we don't prefix the result with
      ; the directive symbol; this is in contrast with  cases like :out,
      ; :gist, :schema, etc., where the schema executor needs to know
      ; what it's getting back as result for an input, and hence what
      ; to do with it
      ((eq directive :ulf)
        (setq result (fill-template pattern parts))
        (setq result (eval-lexical-ulfs result))
        (return-from choose-result-for1 result))

      ;```````````````````````
      ; :ulf-coref directive
      ;```````````````````````
      ; Obtains a ulf result using the subtree & input specified in the pattern, and
      ; then resolves the coreferences in the resulting ulf
      ((eq directive :ulf-coref)
        (setq newclause (fill-template (second pattern) parts))
        (setq result (choose-result-for1 newclause nil (car pattern) visited-subtrees))
        (if (and result (not (equal (car result) :out)))
          (setq result (coref-ulf result (get-ds))))
        (return-from choose-result-for1 result))

      ;```````````````````````
      ; :gist-coref directive
      ;```````````````````````
      ; TODO: Implement coreference resolution (gist case)
      ((eq directive :gist-coref)
        (setq result (cons directive (fill-template pattern parts)))
        (set-time-last-used (get-ds) rule-node (ds-count (get-ds)))
        (setq result (coref-gist result))
        (return-from choose-result-for1 result))

      ;```````````````````````````````
      ; Misc non-recursive directives
      ;```````````````````````````````
      ((member directive '(:out :subtrees :schema :schemas
                           :schema+args :gist :prompt-examples))
        (setq result (fill-template pattern parts))
        ; If result is disjunctive, randomly choose one element
        (when (and (listp result) (equal (car result) :or))
          (setq result (choose-random-element (cdr result))))
        ; If schema directive using schema variable name (old format),
        ; attempt to replace with corresponding predicate
        (when (and (equal directive :schema) (global-var? result))
          (setq result (global-var-to-pred result)))
        (when (equal directive :schemas)
          (setq result (mapcar (lambda (s)
            (if (global-var? s) (global-var-to-pred s) s)) result)))
        (when (and (equal directive :schema+args) (global-var? (car result)))
          (setq result (cons (global-var-to-pred (car result)) (cdr result))))
        ; Update count and return result
        (setq result (cons directive result))
        (set-time-last-used (get-ds) rule-node (ds-count (get-ds)))
        (return-from choose-result-for1 result))

      ; A directive is not recognized
      (t
        (format t "~%*** UNRECOGNIZABLE DIRECTIVE ~s ENCOUNTERED FOR RULE ~s~%    FOR THE FOLLOWING PATTERN AND CLAUSE: ~%    ~s,  ~s" directive rule-node pattern clause))
    )
)) ; END choose-result-for1






