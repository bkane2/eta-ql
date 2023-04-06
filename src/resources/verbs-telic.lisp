;; Records telic verbs which are assumed to be "instantaneously" true, and thus need
;; to be removed from context after a certain period of time has passed.   (Jan 7/21)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :eta)

(defvar *verbs-telic* '(say-to.v reply-to.v react-to.v paraphrase-to.v articulate2-to.v move.v))