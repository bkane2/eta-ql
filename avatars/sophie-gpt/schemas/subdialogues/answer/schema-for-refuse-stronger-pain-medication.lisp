;; *refuse-stronger-pain-medication*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'refuse-stronger-pain-medication.v

'(event-schema :header (((set-of ^me ^you) refuse-stronger-pain-medication.v) ** ?e)
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
  ; Sophie has pain in her chest
  ?s2 (^me ((pres have.v) (k (n+preds pain.n (in.p ((^me 's) chest.n))))))
  ; Sophie is taking Lortab for her pain
  ?s3 (^me ((pres take.v) (k |Lortab|.n) (adv-a (for.p ((^me 's) pain.n)))))
)

:preconds (
  ; Sophie is concerned about the side effects of stronger pain medication
  ?p1 (^me ((pres be.v) concerned.a (about.p-arg (the.d (n+preds (side.a (plur effect.n))
          (of.p (k (stronger.a (pain.n medication.n)))))))))
)

:goals (
  ; Sophie wants to not take stronger pain medication at the moment
  ?g1 (^me ((pres want.v) (to (not (take.v (k (stronger.a (pain.n medication.n))) (adv-e (at.p (the.d moment.n))))))))
)

:episodes (

?e1 (^me paraphrase-to.v ^you '(I think I should hold off on stronger pain medication and think about it more \.))
 
?e2 (^you reply-to.v ?e1)

)

)) ; END refuse-stronger-pain-medication.v