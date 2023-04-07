;; *deny-prognosis*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'deny-prognosis.v

'(event-schema :header (((set-of ^me ^you) deny-prognosis.v) ** ?e)
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
  ; Sophie doesn't accept her prognosis
  ?p1 (^me ((pres do.aux-s) not (accept.v ((^me 's) prognosis.n))))
)

:goals (
  ; Sophie wants to know more about her prognosis
  ?g1 (^me ((pres want.v) (to (know.v (more.d
        (n+preds {information}.n (about.p ((^me 's) prognosis.n))))))))
)

:episodes (

  ?e1 (^me paraphrase-to.v ^you '(There\'s no way your prognosis is true !))
  ?e2 (^you reply-to.v ?e1)
  
  ; If user doesn't address denial in explicit way, escalate emotions
  ?e3 (:if (not (((^you be.v explicit.a) ** ?e2) and ((^you tell.v ^me (about.p-arg ((^me 's) prognosis.n))) ** ?e2)))
  
    ?e4 (^me paraphrase-to.v ^you '([ANGRY] I don\'t believe your prognosis \. I\'m feeling fine \.))
    ?e5 (^you reply-to.v ?e4)

    ; If not explicit again, start to distrust doctor
    ?e6 (:if (not (((^you be.v explicit.a) ** ?e5) and ((^you tell.v ^me (about.p-arg ((^me 's) prognosis.n))) ** ?e5)))

      ?e7 (^me paraphrase-to.v ^you '([ANGRY] That\'s ridiculous \. I know I have more time than that \.))
      ?e8 (^you reply-to.v ?e7)

      ; If not explicit a third time, escalate again (presumably, the conversation would be paused here and rewound)
      ?e9 (:if (not (((^you be.v explicit.a) ** ?e8) and ((^you tell.v ^me (about.p-arg ((^me 's) prognosis.n))) ** ?e8)))
      
        ?e10 (^me paraphrase-to.v ^you '([ANGRY] I\'m going to find a different doctor \.))
        ?e11 (^me say-to.v ^you '([NEUTRAL] Let\'s pause here for feedback on this conversation \.))
        ?e12 (^me say-bye.v))))


  ; Once the user is explicit, proceed
  ?e13 ((set-of ^me ^you) bargain-about-prognosis.v)

)

:obligations (
  !o1 (?e1 obligates ((^you be.v explicit.a) and (^you tell.v ^me (about.p-arg ((^me 's) prognosis.n)))))
  !o2 (?e4 obligates ((^you be.v explicit.a) and (^you tell.v ^me (about.p-arg ((^me 's) prognosis.n)))))
  !o3 (?e7 obligates ((^you be.v explicit.a) and (^you tell.v ^me (about.p-arg ((^me 's) prognosis.n)))))
  !o4 (?e10 obligates ((^you be.v explicit.a) and (^you tell.v ^me (about.p-arg ((^me 's) prognosis.n)))))
)

)) ; END deny-prognosis.v