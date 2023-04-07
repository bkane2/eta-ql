;; *confirm-name*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'confirm-name.v

'(event-schema :header (((set-of ^me ^you) confirm-name.v) ** ?e)
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
  ; Sophie is named "Sophie Hallman"
  ?p1 (^me ((pres (pasv name.v)) |Sophie Hallman|))
)

:goals (
  ; The user wants to know Sophie's name
  ?g1 (^you ((pres want.v) (to (know.v ((^me 's) name.n)))))
)

:episodes (

?e1 (^me paraphrase-to.v ^you '(You can call me Sophie \.))
 
?e2 (^you reply-to.v ?e1)

)

)) ; END confirm-name.v