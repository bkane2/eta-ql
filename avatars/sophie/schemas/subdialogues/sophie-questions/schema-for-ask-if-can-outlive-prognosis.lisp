;; *ask-if-can-outlive-prognosis*: development version 6
;;
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(store-schema 'ask-if-can-outlive-prognosis.v

'(event-schema :header (((set-of ^me ^you) ask-if-can-outlive-prognosis.v) ** ?e)
;````````````````````````````````````````````````````````````````````````````````


:goals (
  ?g1 (^me want.v (that (^me know.v (ans-to '(Can I outlive your prognosis ?)))))
)

:episodes (

?e1 (^me paraphrase-to.v ^you '(Can I outlive your prognosis ?))
 
?e2 (^you reply-to.v ?e1)

?e3 (^me react-to.v ?e2)

)

)) ; END ask-if-can-outlive-prognosis.v