;; *ask-about-will-chemotherapy-help*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'ask-about-will-chemotherapy-help.v

'(event-schema :header (((set-of ^me ^you) ask-about-will-chemotherapy-help.v) ** ?e)
;````````````````````````````````````````````````````````````````````````````````

:types (
  !t1 (^me person.n)
  !t2 (^you person.n)
)

:rigid-conds (
  ; Sophie is a woman
  !r1 (^me ((pres be.v) (= (a.d woman.n))))
  ; Sophie is 65 years old
  !r2 (^me ((pres be.v) ((mod-a (65.a (plur year.n))) old.a)))
)

:static-conds (
  ; Sophie has lung cancer
  ?s1 (^me ((pres have.v) (k (lung.n cancer.n))))
  ; Sophie wants to be able to watch her grandson graduate
  ?s2 (^me ((pres want.v) (to (be.v able.a (to (watch.v ((^me 's) grandson.n) (ke graduate.v)))))))
)

:preconds (
  ; Sophie is unsure about chemotherapy
  ?p1 (^me ((pres be.v) unsure.a (about.p-arg (k chemotherapy.n))))
)

:goals (
  ; Sophie wants to know whether chemotherapy will help her
  ?g1 (^me ((pres want.v) (to (know.v (whether
        ((k chemotherapy.n) ((pres will.aux-s) (help.v ^me))))))))
)

:episodes (

?e1 (^me paraphrase-to.v ^you '(Do you think chemotherapy will help ?))
 
?e2 (^you reply-to.v ?e1)

)

)) ; END ask-about-will-chemotherapy-help.v