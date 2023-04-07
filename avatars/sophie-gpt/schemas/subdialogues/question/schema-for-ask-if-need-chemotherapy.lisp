;; *ask-if-need-chemotherapy*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'ask-if-need-chemotherapy.v

'(event-schema :header (((set-of ^me ^you) ask-if-need-chemotherapy.v) ** ?e)
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
)

:preconds (
  ; Sophie does not know whether she needs chemotherapy
  ?p1 (^me ((pres do.aux-s) not (know.v (whether (^me ((pres need.v) (k chemotherapy.n)))))))
)

:goals (
  ; Sophie wants to know whether she needs chemotherapy
  ?g1 (^me ((pres want.v) (to (know.v (whether (^me ((pres need.v) (k chemotherapy.n))))))))
)

:episodes (

?e1 (^me paraphrase-to.v ^you '(Do I need chemotherapy ?))
 
?e2 (^you reply-to.v ?e1)

)

:obligations (
  !o1 (?e1 obligates ((^you be.v explicit.a) and (^you tell.v ^me (about.p-arg (k chemotherapy.n)))))
)

)) ; END ask-if-need-chemotherapy.v