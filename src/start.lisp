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


(defun ensure-log-files-exist ()
;```````````````````````````````````
; Ensure that empty conversation log files exist for the given avatar configuration and current dialogue instance.
;
  (ensure-directories-exist (concatenate 'string (get-io-path "conversation-log/")))
  (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") "text.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") "text-readable.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") "gist.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") "semantic.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") "pragmatic.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
  (with-open-file (outfile (concatenate 'string (get-io-path "conversation-log/") "obligations.txt")
    :direction :output :if-exists :supersede :if-does-not-exist :create))
) ; END ensure-log-files-exist


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
  (ensure-directories-exist (get-io-path "embeddings/"))
  (ensure-directories-exist (get-io-path "embeddings/schemas/"))

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





(defun start (&key session-id safe-mode)
;`````````````````````````````````````````
; Starts the dialogue manager by loading config, loading the selected avatar files,
; and calling the top-level eta funtion.
;

  ; Load the config file corresponding to the session's session-id.
  ; TODO: could modify this to auto-generate a default config file if one doesn't exist for the session-id.
  (when (and session-id (or (stringp session-id) (numberp session-id)) (not (boundp '*session-id*)))
    (setq *session-id* session-id))
  (load
    (if (and (boundp '*session-id*) *session-id* (or (stringp *session-id*) (numberp *session-id*))
            (probe-file (format nil "config/~a.lisp" *session-id*)))
      (format nil "config/~a.lisp" *session-id*)
      (format nil "config/config.lisp")))


  ; Set IO path based on agent ID (using basic path if no ID is defined)
  (defparameter *io-path*
    (if (and (boundp '*session-id*) *session-id* (or (stringp *session-id*) (numberp *session-id*)))
      (format nil "./io/~a/" *session-id*)
      (format nil "./io/")))


  ; If live mode, load *user-id* and *user-name* from sessionInfo file (if it exists).
  ; Otherwise, manually set (or prompt user for input).
  ;```````````````````````````````````````````````````````````````````````
  (when (or (not (boundp '*user-id*)) (not *user-id*))
    (defparameter *user-id* "_test")
    ;; (format t "~%~%Enter user-id ~%")
    ;; (princ "user id: ") (finish-output)
    ;; (setq *user-id* (write-to-string (read))))
  )
  (when (and (or (not (boundp '*user-name*)) (not *user-name*)) (member '|Terminal| *subsystems-perception*))
    (format t "~%~%Enter user name ~%")
    (princ "user name: ") (finish-output)
    (setq *user-name* (read-line)))
  (when (or (not (boundp '*user-name*)) (not *user-name*))
    (setq *user-name* "Test User"))


  ; Clean IO files and load avatar-specific files
  ;``````````````````````````````````````````````````````````
  (clean-io-files)
  (load-avatar-files *avatar*)


  (cond

    ; Run Eta (safe mode)
    ;`````````````````````````
    (safe-mode
      (handler-case (eta :subsystems-perception *subsystems-perception* :subsystems-specialist *subsystems-specialist*
                        :emotions *emotion-tags* :model-names *model-names* :response-generator *generation-mode*
                        :gist-interpreter *interpretation-mode* :parser *parser-mode*)
        (error (c)
          (error-message "Execution of Eta failed due to an internal error.")
          (values 0 c))))

    ; Run Eta
    ;`````````````````````````
    (t (eta :subsystems-perception *subsystems-perception* :subsystems-specialist *subsystems-specialist*
            :emotions *emotion-tags* :model-names *model-names* :response-generator *generation-mode*
            :gist-interpreter *interpretation-mode* :parser *parser-mode*)))

) ; END start