(asdf:defsystem :eta
    :name "Eta dialogue manager"
    :serial t
    :version "1.0.0"
    :description "A schema-based dialogue manager for virtual conversational agents."
    :author "Benjamin Kane <bkane2@ur.rochester.edu>"
    :license "GPLv3"
    :depends-on ("priority-queue" "ttt" "ulf-lib" "gpt3-shell" "information-retrieval" "ulf2english"
                 "ulf-pragmatics" "timegraph" "lenulf" "standardize-ulf")

    :components ((:static-file "eta.asd")
        (:file "package")
        (:module "src" :depends-on ("package") :components
          ((:module "util" :serial t :components
               ((:file "util")
                (:file "ulf-util")
                (:file "ttt-util")
                (:file "timegraph-util")
                (:file "io-util")
                (:file "schema-util")
                (:file "plan-util")))
            (:module "tt" :depends-on ("util") :serial t :components
               ((:file "tt")
                (:file "tt-match-predicates")))
            (:module "response" :depends-on ("util") :serial t :components
               ((:file "historical-questions")
                (:file "response-generator")))
            (:module "interpretation" :depends-on ("util") :serial t :components
               ((:file "stem")
                (:file "eval-lexical-ulfs")))
            (:module "coref" :depends-on ("util") :serial t :components
               ((:file "constraints")
                (:file "type-util")
                (:file "ulf-coref")))
            (:module "resources" :depends-on ("util") :serial t :components
               ((:file "functions")
                (:file "general-word-data")
                (:file "names-female")
                (:file "names-male")
                (:file "names-non-personal")
                (:file "noun-hierarchy")
                (:file "specialist-spatial")
                (:file "specialist-temporal")
                (:file "timegraph-bfs")
                (:file "verbs-telic")))
            (:file "eta" :depends-on ("util" "tt" "response" "interpretation" "coref" "resources"))
            (:file "start" :depends-on ("eta")))))

    :around-compile (lambda (next)
                      ; For development use (debug 3) (safety 3) (space 1) (speed 1)
                      (proclaim '(optimize (debug 0) (safety 2) (space 1) (speed 3)))
                      (funcall next)))

(asdf:defsystem :eta/run
  :serial t
  :description "Shell to run Eta dialogue manager"
  :author "Benjamin Kane <bkane2@ur.rochester.edu>"
  :license "GPLv3"
  :depends-on (:eta :alexandria :hunchentoot)
  :components ((:file "run")))