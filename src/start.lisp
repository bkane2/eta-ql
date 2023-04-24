;; January 6/2020
;; ================================================
;;
;; Starts Eta using the configuration specified in config.lisp
;;

(in-package :eta)


(defun get-io-path (fname)
;``````````````````````````
; Yields IO path for avatar instance.
;
  (concatenate 'string *io-path* fname)
) ; END get-io-path


(defun ensure-log-files-exist (&key (instance 0))
;``````````````````````````````````````````````````
; Ensure that empty conversation log files exist for the given avatar configuration and current dialogue instance.
;
  (let ((instance-dir (format nil "~a/" instance)))
    (ensure-directories-exist (concatenate 'string (get-io-path "conversation-log/") instance-dir))
    (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") instance-dir "text.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") instance-dir "text-readable.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") instance-dir "gist.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") instance-dir "semantic.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") instance-dir "pragmatic.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
    (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") instance-dir "obligations.txt")
      :direction :output :if-exists :supersede :if-does-not-exist :create))
)) ; END ensure-log-files-exist


(defun clean-io-files ()
;``````````````````````````
; Overwrites all io files used by Eta with blank files.
;
  (ensure-directories-exist "./io/")
  (ensure-directories-exist *io-path*)
  (ensure-directories-exist (get-io-path "in/"))
  (ensure-directories-exist (get-io-path "out/"))
  ; Delete and recreate the directory to remove all dialogue instance subdirectories
  (ensure-directories-exist (get-io-path "conversation-log/"))
  (delete-directory (get-io-path "conversation-log/") :recursive t)
  (ensure-directories-exist (get-io-path "conversation-log/"))
  (when *read-log-mode*
    (ensure-directories-exist "./logs/")
    (ensure-directories-exist "./logs/logs/")
    (ensure-directories-exist "./logs/logs_out/")
    (ensure-directories-exist "./logs/logs_out/text/")
    (ensure-directories-exist "./logs/logs_out/gist/")
    (ensure-directories-exist "./logs/logs_out/semantic/")
    (ensure-directories-exist "./logs/logs_out/pragmatic/"))

  ; Ensure all standard input & output files for registered subsystems exist and are empty
  ; Note: input files only created for non-terminal systems,
  ;       output files are only created for non-terminal and non-audio systems
  (mapcar (lambda (system)
  (let ((fname-in (if (not (member system '(|Terminal|)))
                  (concatenate 'string (get-io-path "in/") (string system) ".lisp")))
        (fname-out (if (not (member system '(|Terminal| |Audio|)))
                  (concatenate 'string (get-io-path "out/") (string system) ".lisp"))))
    (if fname-in
    (with-open-file (outfile fname-in :direction :output :if-exists
                                      :supersede :if-does-not-exist :create)))
    (if fname-out
    (with-open-file (outfile fname-out :direction :output :if-exists
                                      :supersede :if-does-not-exist :create)))))
  (append *subsystems-perception* *subsystems-specialist*))

  ; Ensure that empty conversation log files exist
  (ensure-log-files-exist)

  ; Delete the content of output.txt, if it exists, otherwise create
  (with-open-file (outfile (get-io-path "output.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  ; Delete the content of turn-output.txt and turn-emotion.txt, otherwise create
  (with-open-file (outfile (get-io-path "turn-output.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (outfile (get-io-path "turn-emotion.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  ; Delete the content of rewindState.lisp, if it exists, otherwise create
  (with-open-file (outfile (get-io-path "rewindState.lisp")
    :direction :output :if-exists :supersede :if-does-not-exist :create))                                             
) ; END clean-io-files





(defun load-avatar-files (avatar-name)
;``````````````````````````````````````
; Loads all schema and rule files used by a particular avatar
;
  (labels ((load-files-recur (directory)
      (mapcar (lambda (d)
          (mapcar (lambda (f) (load f))
            (directory (concatenate 'string (namestring d) "/*.lisp")))
          (load-files-recur (concatenate 'string
            (coerce (butlast (explode (namestring d))) 'string) "/*")))
        (remove nil (mapcar (lambda (p)
            ; This is pretty awkward, but has to be done to handle differences btwn ACL and SBCL
            (if (fboundp 'probe-directory)
              (if (probe-directory p) p)
              (if (not (pathname-name p)) p)))
          (directory directory))))))
    ; Load all shared rules, schemas, and knowledge files
    (load-files-recur (concatenate 'string "./avatars/" avatar-name "/schemas"))
    (load-files-recur (concatenate 'string "./avatars/" avatar-name "/rules"))
    (load-files-recur (concatenate 'string "./avatars/" avatar-name "/knowledge"))
    (load-files-recur (concatenate 'string "./avatars/" avatar-name "/obj-schemas"))
    ; If a multi-session avatar, load all files specific to that day
    (when (and (boundp '*session-number*) (integerp *session-number*))
      (load-files-recur (concatenate 'string "./avatars/" avatar-name "/" (format nil "day~a" *session-number*)))))
) ; END load-avatar-files





(defun start (&key agent-id)
;````````````````````````````
; Starts the dialogue manager by loading config, loading the selected avatar files,
; and calling the top-level eta funtion.
;

  ; Load the config file corresponding to the session's agent-id.
  ; TODO: could modify this to auto-generate a default config file if one doesn't exist for the agent-id.
  (when (and agent-id (or (stringp agent-id) (numberp agent-id)) (not (boundp '*agent-id*)))
    (setq *agent-id* agent-id))
  (load
    (if (and (boundp '*agent-id*) *agent-id* (or (stringp *agent-id*) (numberp *agent-id*))
            (probe-file (format nil "config/~a.lisp" *agent-id*)))
      (format nil "config/~a.lisp" *agent-id*)
      (format nil "config/config.lisp")))


  ; Set IO path based on agent ID (using basic path if no ID is defined)
  (defparameter *io-path*
    (if (and (boundp '*agent-id*) *agent-id* (or (stringp *agent-id*) (numberp *agent-id*)))
      (format nil "./io/~a/" *agent-id*)
      (format nil "./io/")))
  (format t ">>~a~%" *io-path*)


  ; If live mode, load *user-id* and *user-name* from sessionInfo file (if it exists).
  ; Otherwise, manually set (or prompt user for input).
  ;```````````````````````````````````````````````````````````````````````
  (defparameter *user-id* nil)
  (defparameter *user-name* nil)
  (if (probe-file (get-io-path "sessionInfo.lisp"))
    (load (get-io-path "sessionInfo.lisp")))
  (when (not *user-id*)
    (defparameter *user-id* "_test")
    ;; (format t "~%~%Enter user-id ~%")
    ;; (princ "user id: ") (finish-output)
    ;; (setq *user-id* (write-to-string (read))))
  )
  (when (and (not *user-name*) (member '|Terminal| *subsystems-perception*))
    (format t "~%~%Enter user name ~%")
    (princ "user name: ") (finish-output)
    (setq *user-name* (read-line)))
  (when (not *user-name*)
    (setq *user-name* "Test User"))


  ; Validate dependency settings
  ;````````````````````````````````
  ; If GPT3 generation/interpretation mode and GPT3-shell not provided as a dependency, print warning and change mode to RULE.
  (when (and (or (equal *generation-mode* 'GPT3) (equal *interpretation-mode* 'GPT3))
            (not (member "gpt3-shell" *dependencies* :test #'equal)))
      (format t "~% --- Warning: GPT3 generation/interpretation mode requires gpt3-shell to be listed as a dependency in the config file.")
      (format t "~%              Changing generation/interpretation mode to RULE.~%")
      (setq *generation-mode* 'RULE)
      (setq *interpretation-mode* 'RULE))

  ; If BLLIP parser mode and lenulf + standardize-ulf are not provided as dependencies, print warning and change mode to RULE.
  (when (and (equal *parser-mode* 'BLLIP) (not (member "lenulf" *dependencies* :test #'equal))
                                          (not (member "standardize-ulf" *dependencies* :test #'equal)))
      (format t "~% --- Warning: BLLIP parser mode requires lenulf and standardize-ulf to be listed as dependencies in the config file.")
      (format t "~%              Changing parser mode to RULE.~%")
      (setq *parser-mode* 'RULE))


  ; Clean IO files and load avatar-specific files
  ;``````````````````````````````````````````````````````````
  (clean-io-files)
  (load-avatar-files *avatar*)


  (cond

    ; Run Eta (safe mode)
    ;`````````````````````````
    (*safe-mode*
      (handler-case (eta :subsystems-perception *subsystems-perception* :subsystems-specialist *subsystems-specialist*
                        :emotions *emotion-tags* :dependencies *dependencies* :model-names *model-names* :response-generator *generation-mode*
                        :gist-interpreter *interpretation-mode* :parser *parser-mode*)
        (error (c)
          (error-message "Execution of Eta failed due to an internal error.")
          (values 0 c))))

    ; Run Eta (read-log mode)
    ;`````````````````````````
    (*read-log-mode*
      (let ((logs (if (stringp *read-log-mode*)
                    (directory (concatenate 'string "logs/logs/" *read-log-mode*))
                    (directory "logs/logs/*"))))
        ; Create empty log_out file
        (mapcar (lambda (log)
          (with-open-file (outfile (pathname (concatenate 'string "logs/logs_out/" (pathname-name log)))
            :direction :output :if-exists :supersede :if-does-not-exist :create))) logs)
        ; Start eta using log
        (mapcar (lambda (log)
          (format t "==:: READING LOG ~a ::==~%" log)
          (load-avatar-files *avatar*)
          (eta :read-log log :subsystems-perception *subsystems-perception* :subsystems-specialist *subsystems-specialist*
              :emotions *emotion-tags* :dependencies *dependencies* :model-names *model-names* :response-generator *generation-mode*
              :gist-interpreter *interpretation-mode* :parser *parser-mode*)) logs)))

    ; Run Eta
    ;`````````````````````````
    (t (eta :subsystems-perception *subsystems-perception* :subsystems-specialist *subsystems-specialist*
            :emotions *emotion-tags* :dependencies *dependencies* :model-names *model-names* :response-generator *generation-mode*
            :gist-interpreter *interpretation-mode* :parser *parser-mode*)))


  ; Write user gist clauses to file
  ;````````````````````````````````````
  (print-gist-kb :filename
    (ensure-directories-exist (concatenate 'string "./gist-kb/" *user-id* ".txt")))

) ; END start