;; January 6/2020
;; ================================================
;;
;; Starts Eta using the configuration specified in config.lisp
;;

(in-package :eta)

(defun load-avatar-files (avatar-name session-number)
;``````````````````````````````````````````````````````
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
    (when (and session-number (integerp session-number))
      (load-files-recur (concatenate 'string "./avatars/" avatar-name "/" (format nil "day~a" session-number)))))
) ; END load-avatar-files





(defun start (&optional agent-config-fname &key safe-mode)
;```````````````````````````````````````````````````````````
; Starts the dialogue manager by loading config, loading the selected avatar files,
; and calling the top-level eta funtion.
;
  (let (config-agent)
    (if (or (null agent-config-fname) (not (probe-file agent-config-fname)))
      (cond
        ((probe-file "config/agent/config.lisp")
          (setq agent-config-fname "config/agent/config.lisp"))
        (t
          (format t "~%~~~~ error: could not find agent config file to load.")
          (return-from start nil))))

    (setq config-agent (apply #'make-agent-config (read-config agent-config-fname)))


    ; Load avatar-specific files
    ;``````````````````````````````
    (load-avatar-files
      (agent-config-avatar config-agent)
      (agent-config-session-number config-agent))


    (cond

      ; Run Eta (safe mode)
      ;`````````````````````````
      (safe-mode
        (handler-case (eta config-agent)
          (error (c)
            (error-message "Execution of Eta failed due to an internal error.")
            (values 0 c))))

      ; Run Eta
      ;`````````````````````````
      (t (eta config-agent)))

)) ; END start