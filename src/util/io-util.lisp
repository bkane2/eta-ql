;; Aug 13/2020
;; ================================================
;;
;; Contains utility functions used for file IO
;;

(in-package :eta)

(defun read-from-system (system)
;``````````````````````````````````
; Reads input (as a list of propositions) from given subsystem.
;
  (case system
    (|Audio| (read-audio))
    (|Terminal| (read-terminal))
    (otherwise (read-subsystem system)))
) ; END read-from-system



(defun read-terminal ()
;```````````````````````
; Scans input from the terminal. If the user presses enter, read the
; input, create and return a (^you say-to.v ^me '(...)) proposition.
; NOTE: previously in eta.lisp, it would call detach-final-punctuation
; after reading input. However, I suspect we want punctuation since Google ASR is
; capable of it. The pattern-matching files therefore need to take punctuation into account.
;
  (when (listen)
    (let ((text (parse-chars (coerce (read-line) 'list))))
      (if text `((^you say-to.v ^me ',text)))))
) ; END read-terminal



(defun read-audio ()
;`````````````````````
; Reads input from |Audio| subsystem (i.e., (^you say-to.v ^me '(...)), or
; possibly (^you say-to.v ^me "...")) propositions from io/in/Audio.lisp.
; NOTE: previously in eta.lisp, it would call detach-final-punctuation
; after reading input. However, I suspect we want punctuation since Google ASR is
; capable of it. The pattern-matching files therefore need to take punctuation into account.
; 
  ; Write empty star line to output to prompt avatar to listen
  ; TODO: there has to be a better way of doing this...
  (when (= *output-listen-prompt* 1)
    (setq *output-count* (1+ *output-count*))
    (with-open-file (outfile (get-io-path "output.txt")
      :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~%*~D: dummy" *output-count*))
    (setq *output-listen-prompt* 2))

  ; Read from Audio input
  (setq *input* nil)
  (load (get-io-path "in/Audio.lisp"))
  (if *input*
    (with-open-file (outfile (get-io-path "in/Audio.lisp")
      :direction :output :if-exists :supersede :if-does-not-exist :create)))
  (mapcar (lambda (wff)
      ; If say-to.v argument given in string form, parse it into list form
      (if (and (equal (butlast wff) '(^you say-to.v ^me)) (stringp (car (last wff))))
        (append (butlast wff) `(',(parse-chars (coerce (car (last wff)) 'list))))
        wff))
    *input*)
) ; END read-audio



(defun read-subsystem (system &key block)
;``````````````````````````````````````````
; Reads input ULF propositions from io/in/<system>.lisp.
; If :block t is given, loop until a non-nil value is set for *input*.
; NOTE: 'None is reserved as a special value, which causes the function
; to return nil (nil itself cannot be used, since otherwise the program
; would be unable to distinguish from cases where no input is given yet)
;
  (let ((fname (concatenate 'string (get-io-path "in/") (string system) ".lisp")))
  (setq *input* nil)
  (cond
    ; If :block t is given, loop until non-nil input
    (block
      (loop while (and block (null *input*)) do
        (load fname)))
    ; Otherwise, load file once
    (t (load fname)))
  (if *input*
    (with-open-file (outfile fname
      :direction :output :if-exists :supersede :if-does-not-exist :create)))
  (if (equal *input* 'None) nil *input*)
)) ; END read-subsystem



(defun valid-rewind-state-p (offset)
;`````````````````````````````````````
; Check whether a signal to rewind the dialogue state (relative offset) is valid.
;
  (and (integerp offset) (> offset 0) (< offset (length *ds-stack*)))
) ; END valid-rewind-state-p



(defun check-for-rewind-signal ()
;````````````````````````````````````
; Return t if a valid signal to rewind the dialogue state (i.e., a relative offset within
; the range of previous dialogue states) is detected; nil otherwise.
;
  (load (get-io-path "rewindState.lisp"))
  (cond
    ((and *rewind-state* (valid-rewind-state-p *rewind-state*))
      (with-open-file (outfile (get-io-path "rewindState.lisp")
        :direction :output :if-exists :supersede :if-does-not-exist :create))
      t)
    (t nil))
) ; END check-for-rewind-signal



(defun write-subsystem (output system)
;`````````````````````````````````````````
; Writes output/"query" ULF propositions to io/out/<system>.lisp.
; output should be a list of propositions.
;
  (let ((fname (concatenate 'string (get-io-path "out/") (string system) ".lisp")))
    (with-open-file (outfile fname
      :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format outfile "(setq *output* '~s)" output))
)) ; END write-subsystem



(defun user-log (logfile content)
;`````````````````````````````````````
; Logs some user data in the corresponding log file (i.e., text, gist, semantic, or pragmatic).
; Temporarily disable pretty-printing so each line in the log file corresponds to a single turn.
;
  (let ((fname (concatenate 'string (get-io-path "user-log/") (string-downcase (string logfile)) ".txt")))
    (setq *print-pretty* nil)
    (with-open-file (outfile fname :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~a~%" content))
    (setq *print-pretty* t)
)) ; END user-log



(defun log-turn-write-all ()
;`````````````````````````````
; Logs all of the turns in the current conversation log.
;
  (mapcar #'log-turn-write (reverse (ds-conversation-log *ds*)))
) ; END log-turn-write-all



(defun log-turn-write (turn)
;````````````````````````````
; Logs some a turn in the conversation-log directory, as well as the appropriate log directories
; when in read-log mode.
; Temporarily disable pretty-printing so each line in the log file corresponds to a single turn.
;
  (let* ((instance-dir (format nil "~a/" *dialogue-instance*))
         (log-dir (get-io-path "conversation-log/"))
         (fname-text (concatenate 'string log-dir instance-dir "text.txt"))
         (fname-gist (concatenate 'string log-dir instance-dir "gist.txt"))
         (fname-sem  (concatenate 'string log-dir instance-dir "semantic.txt"))
         (fname-prag (concatenate 'string log-dir instance-dir "pragmatic.txt"))
         (fname-oblg (concatenate 'string log-dir instance-dir "obligations.txt"))
         (agent (dialogue-turn-agent turn)))
    (setq *print-pretty* nil)
    (with-open-file (outfile fname-text :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~a : ~s~%" agent (dialogue-turn-utterance turn)))
    (with-open-file (outfile fname-gist :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~a : ~s~%" agent (remove nil (remove-if #'nil-gist-clause? (dialogue-turn-gists turn)))))
    (with-open-file (outfile fname-sem  :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~a : ~s~%" agent (remove nil (dialogue-turn-semantics turn))))
    (with-open-file (outfile fname-prag :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~s~%" (remove nil (dialogue-turn-pragmatics turn))))
    (with-open-file (outfile fname-oblg :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~s~%" (remove nil (dialogue-turn-obligations turn))))
    (setq *print-pretty* t))
  ; Additionally write conversation log to appropriate log directories when in read-log mode.
  (when *read-log*
    (let ((fname-text (concatenate 'string "logs/logs_out/text/" (pathname-name *read-log*) ".txt"))
          (fname-gist (concatenate 'string "logs/logs_out/gist/" (pathname-name *read-log*) ".txt"))
          (fname-sem  (concatenate 'string "logs/logs_out/semantic/" (pathname-name *read-log*) ".txt"))
          (fname-prag (concatenate 'string "logs/logs_out/pragmatic/" (pathname-name *read-log*) ".txt"))
          (fname-oblg (concatenate 'string "logs/logs_out/obligations/" (pathname-name *read-log*) ".txt"))
          (agent (if (equal (dialogue-turn-agent turn) *^me*)
            (if (boundp '*agent-id*) (string-upcase (string *agent-id*)) "ETA")
            "USER")))
      (setq *print-pretty* nil)
      (with-open-file (outfile fname-text :direction :output :if-exists :append :if-does-not-exist :create)
        (format outfile "~a : ~s~%" agent (dialogue-turn-utterance turn)))
      (with-open-file (outfile fname-gist :direction :output :if-exists :append :if-does-not-exist :create)
        (format outfile "~a : ~s~%" agent (remove nil (remove-if #'nil-gist-clause? (dialogue-turn-gists turn)))))
      (with-open-file (outfile fname-sem  :direction :output :if-exists :append :if-does-not-exist :create)
        (format outfile "~a : ~s~%" agent (remove nil (dialogue-turn-semantics turn))))
      (with-open-file (outfile fname-prag  :direction :output :if-exists :append :if-does-not-exist :create)
        (format outfile "~s~%" (remove nil (dialogue-turn-pragmatics turn))))
      (with-open-file (outfile fname-oblg  :direction :output :if-exists :append :if-does-not-exist :create)
        (format outfile "~s~%" (remove nil (dialogue-turn-obligations turn))))
      (setq *print-pretty* t)))
) ; END log-turn-write



(defun print-words (wordlist)
;``````````````````````````````
; This is intended for the keyboard-based mode of interaction,
; i.e., with *live* = nil.
;
  (format t "~%...")
  (dolist (word wordlist)
    (princ " ")
    (princ word)
    (if (or (member word '(? ! \.))
            (member (car (last (explode word))) '(#\? #\! #\.)))
      (format t "~%")))
) ; END print-words



(defun say-words (wordlist)
;````````````````````````````
; This is intended for th *live* = T mode of operation, i.e., I/O
; is via the virtual agent; (but the output is printed as well).
; For terminal mode only, we use 'print-words'.
;
  (let (wordstring)
    ; Write ETA's words to "./io/output.txt" as a continuous string
    ; (preceded by the output count and a colon)
    (dolist (word wordlist)
      (if (numberp word)
        (push (write-to-string word) wordstring)
        (push (string word) wordstring))
      (push " " wordstring))
    (setq wordstring (reverse (cdr wordstring)))
    (setq wordstring (eval (cons 'concatenate (cons ''string wordstring))))

    ; Increment output number
    (setq *output-count* (1+ *output-count*))
	  
    ; Output words
    (with-open-file (outfile (get-io-path "output.txt")
      :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~%#~D: ~a" *output-count* wordstring))

    ; Add words to output buffer
    (push wordlist *output-buffer*)

    ; Also write ETA's words to standard output:
    (format t "~% ... ")
    (dolist (word wordlist)
      (format t "~a " word)
      (if (or (member word '(? ! \.))
              (member (car (last (explode word))) '(#\? #\! #\.)))
        (format t "~%")))
    (format t "~%")
)) ; END say-words



(defun write-output-buffer ()
;````````````````````````````````
; Writes buffered output to turn-output.txt, as well as any emotion
; tags to turn-emotion.txt.
;
  (let ((buffer (reverse *output-buffer*)) (output "") (emotion "neutral"))
    (setq output (str-join (mapcar (lambda (wordlist)
        (words-to-str (untag-emotions wordlist)))
      buffer) " "))
    (dolist (wordlist buffer)
      (if (equal emotion "neutral")
        (setq emotion (get-emotion wordlist))))
    (with-open-file (outfile (get-io-path "turn-output.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format outfile "~a" output))
    (with-open-file (outfile (get-io-path "turn-emotion.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format outfile "~a" emotion))
    (setq *output-buffer* nil)
)) ; END write-output-buffer



(defun get-api-key (api)
;``````````````````````````
; Reads an API key from either the "<api-name>_key" environment variable, or
; a corresponding text file in config/keys.
; Returns nil and prints a warning if the API key does not exist.
  (let ((fname (concatenate 'string "config/keys/" api ".txt")) (kname (concatenate 'string api "_key")) in key)
    (cond
      ((posix-getenv kname)
        (posix-getenv kname))
      (t
        (ensure-directories-exist "config/keys")
        (cond
          ((probe-file fname)
            (setq in (open fname))
            (setq key (read-line in))
            (close in)
            key)
          (t
            (format t "~% --- Warning: API key for ~a not found in ~a.~%" api fname)
            nil))))
)) ; END get-api-key






;; --------------------------------------------------
;; The functions after this point need to be vetted.
;; --------------------------------------------------





(defun read-input-timeout (n)
;``````````````````````````````
; Reads terminal input for n seconds and returns accumulated string.
;
  (finish-output)
  (let ((i 0) result)
    (loop while (< i n) do
      (sleep 1)
      (if (listen) (setq result (cons " " (cons (read-line) result))))
      (setq i (+ 1 i)))
    (if (listen) (setq result (cons (read-line) result)))
    (eval (append '(concatenate 'string) (reverse result))))
) ; END read-input-timeout



(defun read-log-contents (log)
;```````````````````````````````
; Reads the contents of a given log file and converts to list.
;
  (let (result)
    (with-open-file (logfile log :if-does-not-exist :create)
      (do ((l (read-line logfile) (read-line logfile nil 'eof)))
          ((eq l 'eof) "Reached end of file.")
        (setq result (concatenate 'string result " " l))))
    (read-from-string (concatenate 'string "(" result ")")))
) ; END read-log-contents



(defun update-block-coordinates (moves)
;````````````````````````````````````````
; Given a list of moves (in sequential order), update *block-coordinates*. Return a list of
; perceptions, i.e. the given moves combined with the current block coordinates.
;
  (mapcar (lambda (move)
    (setq *block-coordinates* (mapcar (lambda (coordinate)
        (if (equal (car move) (car coordinate))
          (list (car coordinate) 'at-loc.p (cadar (cddadr move)))
          coordinate))
      *block-coordinates*))) moves)
  (append *block-coordinates* moves)
) ; END update-block-coordinates



(defun process-and-increment-log ()
;`````````````````````````````````````
; Verifies the correctness of the current tuple of log (when applicable),
; increments the log pointer, and stores a say-to.v episode containing the
; user input in the next log entry.
; TODO: any perceptions apart from the say-to.v act should also be stored in
; context here (currently only select move.v observations are stored by the
; respective blocks-world QA actions).
;
  ; Verify current tuple
  (when (and (>= *log-ptr* 0) (> (length (nth *log-ptr* *log-contents*)) 1))
    (if (not *log-answer*) (setq *log-answer* '(PARSE FAILURE \.)))
    (verify-log *log-answer* (nth *log-ptr* *log-contents*) *read-log*)
    (setq *log-answer* nil))
  ; Increment log pointer
  (setq *log-ptr* (1+ *log-ptr*))
  ; Read off input of next tuple and store in context
  (let (ep-name-new (log-input (if (>= *log-ptr* (length *log-contents*))
                      (parse-chars (coerce "bye" 'list))
                      (parse-chars (coerce (first (nth *log-ptr* *log-contents*)) 'list)))))
    (setq log-input `(^you say-to.v ^me ',log-input))
    (when log-input
      (setq ep-name-new (store-new-contextual-facts (list log-input)))
      (enqueue-in-buffer (list ep-name-new log-input) (buffers-perceptions (ds-buffers *ds*)))))
) ; END process-and-increment-log



(defun verify-log (answer-new turn-tuple filename)
;```````````````````````````````````````````````````
; Given Eta's answer for a turn, allow the user to compare to the answer in the log
; and amend the correctness judgment for that turn. Output to the corresponding
; filename in log_out/ directory.
;
  (let ((filename-out (concatenate 'string "logs/logs_out/" (pathname-name filename)))
        (answer-old (parse-chars (coerce (third turn-tuple) 'list)))
        (feedback-old (fourth turn-tuple)) feedback-new)
    ;; (format t "/~a~%\\~a~%" answer-old answer-new)
    (with-open-file (outfile filename-out :direction :output :if-exists :append :if-does-not-exist :create)
      (cond
        ; If answer is the same, just output without modification
        ((equal answer-old answer-new)
          (format outfile "(\"~a\" ~S \"~a\" ~a)~%" (first turn-tuple) (second turn-tuple) (third turn-tuple) (fourth turn-tuple)))
        ; If question was marked as non-historical, also skip
        ((member (fourth turn-tuple) '(XC XI XP XE))
          (format outfile "(\"~a\" ~S \"~a\" ~a)~%" (first turn-tuple) (second turn-tuple) (third turn-tuple) (fourth turn-tuple)))
        ; If "when" question with specific time, also skip
        ((and (equal "when" (string-downcase (subseq (first turn-tuple) 0 4)))
              (find-if (lambda (x) (member x '(zero one two three four five six seven eight nine ten eleven twelve thirteen
                                               fourteen fifteen sixteen seventeen eighteen nineteen twenty thirty forty
                                               fifty sixty seventy eighty ninety hundred))) answer-old))
          (format outfile "(\"~a\" ~S \"~a\" ~a)~%" (first turn-tuple) (second turn-tuple) (third turn-tuple) (fourth turn-tuple)))
        ; Otherwise, check the new output with the user and prompt them to change feedback

        (t
          (format t " ----------------------------------------------------------~%")
          (format t "| A CHANGE WAS DETECTED IN LOG '~a':~%" (pathname-name filename))
          (format t "| * question: ~a~%" (first turn-tuple))
          (format t "| * old answer: ~a~%" answer-old)
          (format t "| * old feedback: ~a~%" (fourth turn-tuple))
          (format t "| * new answer: ~a~%" answer-new)
          (format t "| > new feedback: ")
          (finish-output) (setq feedback-new (read-from-string (read-line)))
          (format t " ----------------------------------------------------------~%")
          (if (not (member feedback-new '(C I P F E))) (setq feedback-new 'E))
          (format outfile "(\"~a\" ~S \"~a\" ~a)~%"
            (first turn-tuple) (second turn-tuple) (format nil "~{~a~^ ~}" answer-new) feedback-new)))))
) ; END verify-log



(defun error-message (str mode)
;````````````````````````````````
; Print error message to the console, and if in live mode, to output.txt
;
  (format t "~a~%" str)
  (if mode
    (with-open-file (outfile (get-io-path "output.txt")
      :direction :output :if-exists :append :if-does-not-exist :create)
      (format outfile "~%#: ~a" str)))
) ; END error-message