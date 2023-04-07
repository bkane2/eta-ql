;;	what are things you want to change about yourself ?
;;	(want-change-about-you)
;; 	from-want-change-about-you-input
;;	(0 thing 3 change about myself is 0) 	(0 nothing 3 change about myself 0) 
;;		gist-question: (1 what 2 things 3 change about yourself 1) 
;; MEETING WITH KIM NOTES (8/4/2017)
;; Be more outgoing
;; Excersize more
;; Eat healthy
;; Spend more time with my grandchildren/spouse/kids
;; They isolate and want to get out of their house more
;; Negative outlook; being pessimistic
;; Lose weight
;; Manage money better
;; Might get confused and interpret as "what you would want to be different about your life"
;; hear better, better health, not need a wheelchare, have better hearing, not live in a nursing home, be able to drive, see better, etc.
(MAPC 'ATTACHFEAT
'(
  (SYN-MORE more better higher good gain)
  (SYN-LESS less lose)
  (SYN-DOING DO BE doing being)
  (QUALITY-TYPES-COMPARATIVE funnier kinder smarter friendlier happier wiser stronger taller humbler bolder healthier)
  (CHANGE-FROM-ISOLATED isolate isolated introverted introvert lonely alone)
  (CHANGE-FROM-PROCRASTINATOR procrastinate procrastinating procrastination distracted)
  (CHANGE-FROM-UNHEALTHY weight overweight underweight unhealthy fat heavy chubby skinny thin)
  (CHANGE-FROM-COMM-SKILLS conversation speaking communicating communication interacting interaction talking)
  (SKILL skills ability)
  (NEGATIVE negatively pessimistic pessimist pessimism)
  ;; BADSTATE
  ;; BADHEALTH
  (CHANGE-TO-INDEPENDENT independent independently alone myself)
  (CHANGE-TO-TWO-LEARNING learn learning improve improving experiences EXPERIENCE SKILL skills)
  (EXPERIENCE experiences)
))


(READRULES '*want-change-about-you-input*
'(
  ; Reciprocal questions
  1 (0 what 2 you 0 ?)
    2 (What are things I want to change about myself ?) (0 :gist)
  1 (0 how 2 you 0 ?)
    2 (What are things I want to change about myself ?) (0 :gist)
  ; Specific answers
  ;; Two word
  1 (0 .SYN-MORE 2 .QUALITY-TWO-TYPES-ARCHETYPES .QUALITY-TWO-TYPES-ARCHETYPES 0)
    2 ((The thing that you would change about yourself is being a 4 5 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-MORE 2 .QUALITY-TWO-TYPES-PARTICIPLE .QUALITY-TWO-TYPES-PARTICIPLE 0)
    2 ((The thing that you would change about yourself is 4 5 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .QUALITY-TWO-TYPES-VERB .QUALITY-TWO-TYPES-VERB 2 .SYN-MORE 0)
    2 ((The thing that you would change about yourself is trying to 2 3 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-MORE 2 .QUALITY-TWO-TYPES-NOUN .QUALITY-TWO-TYPES-NOUN 0)
    2 ((The thing that you would change about yourself is having 4 5 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-MORE 2 .QUALITY-TWO-TYPES-ADJ .QUALITY-TWO-TYPES-ADJ 0)
    2 ((The thing that you would change about yourself is being 4 5 \.) (Want-change-about-you)) (0 :gist)
  ;; One word
  1 (0 .SYN-MORE 2 .QUALITY-TYPES-ARCHETYPES 0)
    2 ((The thing that you would change about yourself is being a 4 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-MORE 2 .QUALITY-TYPES-PARTICIPLE 0)
    2 ((The thing that you would change about yourself is 4 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .QUALITY-TYPES-VERB 2 .SYN-MORE 0)
    2 ((The thing that you would change about yourself is trying to 2 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-MORE 2 .QUALITY-TYPES-NOUN 0)
    2 ((The thing that you would change about yourself is having 4 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-MORE 2 .QUALITY-TYPES-ADJ 0)
    2 ((The thing that you would change about yourself is being 4 \.) (Want-change-about-you)) (0 :gist)
  1 (0 .QUALITY-TYPES-COMPARATIVE 0)
    2 ((The thing that you would change about yourself is being 2 \.) (Want-change-about-you)) (0 :gist)
  ;; Specific changes
  1 (0 .SYN-LESS 2 .CHANGE-FROM-ISOLATED 0)
    2 ((The thing that you would change about yourself is being less alone \.) (Want-change-about-you)) (0 :gist)
  1 (0 .NEG 2 .BE 2 .CHANGE-FROM-ISOLATED 0)
    2 ((The thing that you would change about yourself is being less alone \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-LESS 2 .CHANGE-FROM-PROCRASTINATOR 0)
    2 ((The thing that you would change about yourself is procrastinating less \.) (Want-change-about-you)) (0 :gist)
  1 (0 .NEG 2 .BE .NEG .CHANGE-FROM-PROCRASTINATOR 0)
    2 ((The thing that you would change about yourself is procrastinating less \.) (Want-change-about-you)) (0 :gist)
  1 (0 .CHANGE-FROM-UNHEALTHY 0)
    2 ((The thing that you would change about yourself is being more healthy \.) (Want-change-about-you)) (0 :gist)
  1 (0 .BADHEALTH 0)
    2 ((The thing that you would change about yourself is being more healthy \.) (Want-change-about-you)) (0 :gist)
  1 (0 .BADSTATE 0)
    2 ((The thing that you would change about yourself is being more happy \.) (Want-change-about-you)) (0 :gist)
  1 (0 .NEGATIVE 0)
    2 ((The thing that you would change about yourself is being less negative \.) (Want-change-about-you)) (0 :gist)
  1 (0 .SYN-DOING 3 .CHANGE-TO-INDEPENDENT 0)
    2 ((The thing that you would change about yourself is being more independent \.) (Want-change-about-you)) (0 :gist)
  1 (0 .CHANGE-TO-TWO-LEARNING 2 .CHANGE-TO-TWO-LEARNING 0)
    2 ((The thing that you would change about yourself is learning new skills \.) (Want-change-about-you)) (0 :gist)
  1 (0 new .CHANGE-TO-TWO-LEARNING 0)
    2 (0 .EXPERIENCE 0)
      3 ((The thing that you would change about yourself is having new experiences \.) (Want-change-about-you)) (0 :gist)
    2 ((The thing that you would change about yourself is learning new skills \.) (Want-change-about-you)) (0 :gist)
  1 (0 .CHANGE-FROM-COMM-SKILLS 0)
    2 ((The thing that you would change about yourself is improving your communication skills \.) (Want-change-about-you)) (0 :gist)
  ;; 1 (0 do things alone 0)
  ;;   2 ((The thing that you would change about yourself is to be able to do things alone \.) ()) (0 :gist)
  ;; 1 (0 live 1 alone 0)
  ;;   2 ((The thing that you would change about yourself is to be able to live alone \.) ()) (0 :gist)
  1 (0 can not 1 think 0)
    2 ((There is nothing that you would change about yourself \.) (Want-change-about-you)) (0 :gist)
  1 (0 not 1 sure 0)
    2 ((There is nothing that you would change about yourself \.) (Want-change-about-you)) (0 :gist)
  1 (0 not really 0)
    2 ((There is nothing that you would change about yourself \.) (Want-change-about-you)) (0 :gist)
  1 (0)
    2 ((NIL Gist \: nothing found for the thing change about yourself is \.) (Want-change-about-you)) (0 :gist)
))


(READRULES '*reaction-to-want-change-about-you-input*
'(
  ;; Two word
  1 (0 .QUALITY-TWO-TYPES-ARCHETYPES .QUALITY-TWO-TYPES-ARCHETYPES 0)
    2 (Being a 2 3 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TWO-TYPES-PARTICIPLE .QUALITY-TWO-TYPES-PARTICIPLE 0)
    2 (2 3 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TWO-TYPES-VERB .QUALITY-TWO-TYPES-VERB 0)
    2 (Knowing how to 2 3 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TWO-TYPES-NOUN .QUALITY-TWO-TYPES-NOUN 0)
    2 (0 hard .WORK 0)
      3 (Being a hard worker is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
    2 (0 open mind 0)
      3 (Having an open mind is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
    2 (Having 2 3 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TWO-TYPES-ADJ .QUALITY-TWO-TYPES-ADJ 0)
    2 (Being more 2 3 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  ;; One word
  1 (0 .QUALITY-TYPES-ARCHETYPES 0)
    2 (Being a 2 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TYPES-PARTICIPLE 0)
    2 (2 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TYPES-VERB 0)
    2 (Knowing how to 2 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TYPES-NOUN 0)
    2 (0 humor 0)
      3 (Having a good sense of humor is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
    2 (0 smile 0)
      3 (Having a nice smile is a good thing to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
    2 (0 relationship 0)
      3 (Having better relationships is a good thing to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
    2 (0 health 0)
      3 (Having good health is pretty important \! I hope you stay healthy in the future \.) (100 :out)
    2 (Having 2 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  1 (0 .QUALITY-TYPES-ADJ 0)
    2 (Being more 2 is a good trait to work on \! Personally \, I am trying to work on being a better listener \.) (100 :out)
  ;; Specific changes
  1 (0 less alone 0)
    2 (It\'s always good to have friends to keep you company \. I hope you are successful with that \!) (100 :out)
  1 (0 procrastinating less 0)
    2 (I know what it\'s like to get distracted too easily \. I hope you can improve \.) (100 :out)
  1 (0 more healthy 0)
    2 (Having good health is pretty important \! I hope you stay healthy in the future \.) (100 :out)
  1 (0 more happy 0)
    2 (I hope you can find something to keep you happy in the future \.) (100 :out)
  1 (0 less .NEGATIVE 0)
    2 (It is important to be able to stay positive and optimistic about you and your life \.) (100 :out)
  1 (0 more independent 0)
    2 (Being more independent is always good \, I hope you have success with that \.) (100 :out)
  1 (0 learning new skills 0)
    2 (I think there\'s possibilities to learn new skills every day \. I hope you are successful with that \!) (100 :out)
  1 (0 having new .EXPERIENCE 0)
    2 (I think there\'s possibilities to have new experiences every day \. I hope you are successful with that \!) (100 :out)
  1 (0 .CHANGE-FROM-COMM-SKILLS 0)
    2 (I think improving communication and social skills would be great \, since it helps you have more effective relations with other people \.) (100 :out)
  ;; Nothing/not sure
  1 (0 there is nothing 3 change about yourself 0)
    2 (It\'s great to be comfortable with yourself \! I hope you can continue to do so in the future \.) (100 :out)
  1 (0 NIL Gist 0)
    2 (We all have traits we could improve on \. Something to keep in mind for the future \!) (100 :out)
))