;; *ask-how-chemotherapy-works*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'ask-how-chemotherapy-works.v

'(event-schema :header (((set-of ^me ^you) ask-how-chemotherapy-works.v) ** ?e)
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
  ; Sophie does not understand how chemotherapy works
  ?p1 (^me ((pres do.aux-s) not (understand.v (ans-to
        (sub how.pq ((k chemotherapy.n) ((pres work.v) *h)))))))
)

:goals (
  ; Sophie wants to know how chemotherapy works
  ?g1 (^me ((pres want.v) (to (know.v (ans-to
        (sub how.pq ((k chemotherapy.n) ((pres work.v) *h))))))))
)

:episodes (

?e1 (^me paraphrase-to.v ^you '(How does chemotherapy work ?))
 
?e2 (^you reply-to.v ?e1)

)

:obligations (
  !o1 (?e1 obligates ((^you be.v explicit.a) and (^you tell.v ^me (about.p-arg (k chemotherapy.n)))))
)

)) ; END ask-how-chemotherapy-works.v