;; Dec 13/2022
;; ===========================================================
;;
;; Contains functions used for interfacing with GPT.
;;

(in-package :eta)

(defun generate-prompt-turn-start (name &key (short t) (newline t))
;````````````````````````````````````````````````````````````````````
; Generates a turn start prefix for the prompt.
;
  (if newline
    (format nil "[N]~a:" (if short (shortname name) name))
    (format nil "~a:" (if short (shortname name) name)))
) ; END generate-prompt-turn-start



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



(defun generate-prompt-preprocess-paraphrase-examples (examples ^me ^you)
;`````````````````````````````````````````````````````````````````````````
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
        (format nil "~a " (generate-prompt-turn-start (string ^you)))
        (first example)
        (format nil "~a " (generate-prompt-turn-start (string ^me)))
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



(defun generate-prompt-emotion (utterance history ^me ^you emotions)
;``````````````````````````````````````````````````````````````````````````````````
; Generates a GPT-3 prompt for classifying an utterance string as one of the emotions in
; emotions (a list of strings), given a dialogue history (a list of strings).
;
  (let (prompt)
    (setq prompt
      (format nil "From the following list, which emotional state most closely describes ~a's feelings?[N]"
        (shortname (string ^me))))
    (setq prompt (concatenate 'string prompt
      (str-join emotions ", ")
      "[N][N]"
      (generate-prompt-preprocess-history history)
      (generate-prompt-turn-start (string ^me))
      " "
      utterance
      "[N][N]Emotional state:"))
)) ; END generate-prompt-emotion



(defun generate-prompt-paraphrase (conds facts examples prev-utterance gist-clause incomplete-utterance ^me ^you mode)
;``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
; Generates a GPT-3 prompt for paraphrasing from facts, which is a list of strings,
; a list of examples (3-tuples of strings), a previous utterance string, and a
; gist-clause string.
; If a string is given for :incomplete-utterance, it is treated as a partially-generated
; response for GPT-3 to continue to fill in.
;
  (let (prompt)
    (setq prompt (format nil "~:(~a~) is having a conversation with ~:(~a~). " ^you ^me))
    (setq prompt (concatenate 'string prompt (str-join conds " ")))
    (setq prompt (concatenate 'string prompt
      (format nil "[N][N]Rewrite the following conversations as conversations between ~a and ~a:"
        (shortname (string ^you)) (shortname (string ^me)))))

    (when facts
      (setq prompt (concatenate 'string prompt
        "[N][N]"
        "Use the following facts in your rewritings:[N]"
        (str-join (mapcar (lambda (fact) (format nil "* ~a" fact)) facts) "[N]"))))

    (cond
      ((equal mode 'statement)
        (setq prompt (concatenate 'string prompt "[N][N]Do not ask a question in your rewritten responses.")))
      ((equal mode 'question)
        (setq prompt (concatenate 'string prompt "[N][N]Ask a question in your rewritten responses."))))

    (setq prompt (concatenate 'string prompt
      "[N][N]"
      ; Add examples to prompt
      (generate-prompt-preprocess-paraphrase-examples examples ^me ^you)
      "[N][N]"
      ; Add prev-utterance and gist-clause to prompt
      (format nil "\"~a " (generate-prompt-turn-start "Person A" :short nil :newline nil))
      prev-utterance
      (format nil "~a " (generate-prompt-turn-start "Person B" :short nil))
      gist-clause
      "\"[N]"
      (format nil "~a " (generate-prompt-turn-start (string ^you)))
      prev-utterance
      (generate-prompt-turn-start (string ^me))))
    (when incomplete-utterance
      (setq prompt (concatenate 'string prompt " " incomplete-utterance (generate-prompt-turn-start (string ^me)))))
    prompt
)) ; END generate-prompt-paraphrase



(defun generate-prompt-unconstrained (conds facts history ^me ^you mode)
;````````````````````````````````````````````````````````````````````````
; Generates a GPT-3 prompt for unconstrained generation from conds,
; which is a list of strings; facts, which is a list of strings;
; and history, which is a list of lists (agent turn) where agent and
; turn are both strings.
;
  (let (prompt)
    (setq prompt (format nil "Write a conversation between ~:(~a~) and ~:(~a~). " ^you ^me))
    (setq prompt (concatenate 'string prompt (str-join conds " ")))

    (when facts
      (setq prompt (concatenate 'string prompt
        "[N][N]"
        "Use the following facts in your response:[N]"
        (str-join (mapcar (lambda (fact) (format nil "* ~a" fact)) facts) "[N]"))))

    (cond
      ((equal mode 'statement)
        (setq prompt (concatenate 'string prompt "[N][N]Do not ask a question in your response.")))
      ((equal mode 'question)
        (setq prompt (concatenate 'string prompt "[N][N]Ask a question in your response."))))

    (setq prompt (concatenate 'string prompt "[N]"
      ; Add initial greeting from user to prompt to calibrate GPT-3
      (format nil "~a Hi, ~a." (generate-prompt-turn-start (string ^you)) (shortname (string ^me)))
      ; If the initial dialogue turn is not Eta's, add initial greeting from Eta to calibrate GPT-3
      (if (not (equal (first (car history)) (string ^me)))
        (format nil "~a Hi, ~a." (generate-prompt-turn-start (string ^me)) (shortname (string ^you)))
        "")
      (if history "[N]" "")
      ; TODO: only add the second line by Sophie if not detected in conversation log
      (generate-prompt-preprocess-history history)
      (generate-prompt-turn-start (string ^me))))
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



(defun get-gpt3-emotion (utterance history ^me ^you &key (emotions *emotions-list*))
;`````````````````````````````````````````````````````````````````````````````````````````````````````````
; Uses GPT-3 to classify the emotion of an utterance as one of the given emotions.
;
  (let (prompt stop-seq generated emotion)
    (setq emotions (mapcar (lambda (e)
        (format nil "~:(~a~)" (coerce (cdr (butlast (explode e))) 'string)))
      emotions))
    (setq prompt (generate-prompt-emotion utterance history ^me ^you emotions))
    ;; (format t "~%  gpt-3 prompt:~%-------------~%~a~%-------------~%" prompt) ; DEBUGGING
    (setq stop-seq (vector
      (generate-prompt-turn-start (string ^you))
      (generate-prompt-turn-start (string ^me))))
    ;; (format t "~%  gpt-3 stop-seq: ~s~%" stop-seq) ; DEBUGGING
    (setq generated (gpt3-generate (get-api-key "openai") prompt :stop-seq stop-seq))
    ;; (format t "~%  gpt-3 response:~%-------------~%~a~%-------------~%" generated) ; DEBUGGING
    (setq emotion (format nil "~:(~a~)" (string-trim " " (trim-all-newlines generated))))
    (if (member emotion emotions :test #'equal)
      (read-from-string (format nil "[~a]" emotion))
      '[NEUTRAL])
)) ; END get-gpt3-emotion



(defun get-gpt3-paraphrase (conds facts examples prev-utterance gist-clause ^me ^you &key incomplete-utterance mode)
;```````````````````````````````````````````````````````````````````````````````````````````````````````````````````
; Generates a GPT-3 paraphrase given a prompt containing conds, which is a list of strings to
; use in conditioning the LLM; facts, which is a list of strings that the LLM is prompted to
; use in response generation; examples, which is a list of 3-tuples of strings representing example
; paraphrases; prev-utterance, which is a string; and gist-clause, which is a string.
; Returns a list of words.

; If a string is given for :incomplete-utterance, it is treated as a partially-generated
; response for GPT-3 to continue to fill in.
;
; Optionally, :mode may be specified (either 'question or 'statement)
; in order to prompt GPT-3 to specifically generate a question or statement
; response type.
;
  (let (prompt stop-seq generated)
    (setq prompt (generate-prompt-paraphrase conds facts examples prev-utterance gist-clause incomplete-utterance ^me ^you mode))
    ;; (format t "~%  gpt-3 prompt:~%-------------~%~a~%-------------~%" prompt) ; DEBUGGING
    (setq stop-seq (vector
      (generate-prompt-turn-start ^you)
      (generate-prompt-turn-start ^me)
      "Person A"
      "Person B"))
    ;; (format t "~%  gpt-3 stop-seq: ~s~%" stop-seq) ; DEBUGGING
    (setq generated (gpt3-generate (get-api-key "openai") prompt :stop-seq stop-seq))
    ;; (format t "~%  gpt-3 response:~%-------------~%~a~%-------------~%" generated) ; DEBUGGING
    (postprocess-generation (parse-chars (coerce (trim-all-newlines generated) 'list)) mode)
)) ; END get-gpt3-paraphrase



(defun get-gpt3-response (conds facts history ^me ^you &key mode)
;`````````````````````````````````````````````````````````````````
; Generates a GPT-3 response from conds, which is a list
; of strings to use in conditioning the LLM; facts, which
; is a list of strings that the LLM is prompted to use in
; response generation; and history, which is a list of of
; lists (agent turn) where agent and turn are both strings.
; Returns a list of words.
;
; Optionally, :mode may be specified (either 'question or 'statement)
; in order to prompt GPT-3 to specifically generate a question or statement
; response type.
;
  (let (prompt stop-seq generated)
    (setq prompt (generate-prompt-unconstrained conds facts history ^me ^you mode))
    ;; (format t "~%  gpt-3 prompt:~%-------------~%~a~%-------------~%" prompt) ; DEBUGGING
    (setq stop-seq (vector
      (generate-prompt-turn-start ^you)
      (generate-prompt-turn-start ^me)))
    ;; (format t "~%  gpt-3 stop-seq: ~s~%" stop-seq) ; DEBUGGING
    (setq generated (gpt3-generate (get-api-key "openai") prompt :stop-seq stop-seq))
    ;; (format t "~%  gpt-3 response:~%-------------~%~a~%-------------~%" generated) ; DEBUGGING
    ; Hack to remove parentheticals that GPT-3 sometimes generates
    (setq generated
      (str-replace (str-replace (str-replace (str-replace generated "* " "] ") "*" "[") "(" "[") ")" "]"))
    (postprocess-generation (parse-chars (coerce (trim-all-newlines generated) 'list)) mode)
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



(defun postprocess-generation (generated mode)
;``````````````````````````````````````````````````````````
; Ensures that the generated utterance abides by the specified
; mode (i.e., removing a question if 'mode' is 'statement).
; 
  (let (parts)
    (setq parts (split-sentences generated))
    (cond
      ((equal mode 'statement)
        (setq parts (reverse (member t (reverse parts) :key #'statement?)))))
    (apply #'append parts)
)) ; END postprocess-generation