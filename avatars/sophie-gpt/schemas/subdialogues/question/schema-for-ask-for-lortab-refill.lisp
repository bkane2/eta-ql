;; *ask-for-lortab-refill*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'ask-for-lortab-refill.v

'(event-schema :header (((set-of ^me ^you) ask-for-lortab-refill.v) ** ?e)
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
  ; Sophie is out of her Lortab
  ?p1 (^me ((pres be.v) (out.a (of.p-arg ((^me 's) |Lortab|.n)))))
)

:goals (
  ; Sophie wants a refill of her Lortab
  ?g1 (^me ((pres want.v) (a.d (n+preds refill.n (of.p ((^me 's) |Lortab|.n))))))
)

:episodes (

?e1 (^me paraphrase-to.v ^you '(I would like a refill of medicine \.))
 
?e2 (^you reply-to.v ?e1)

)

)) ; END ask-for-lortab-refill.v