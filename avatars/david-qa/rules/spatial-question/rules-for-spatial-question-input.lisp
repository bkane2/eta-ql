;; Gist clause extraction
;; simple nested rules + features + wildcards
;;
;; "How did you get here today?"
(MAPC 'ATTACHFEAT
'(
  (SPECIAL-REQUEST bye goodbye pause stop exit MOMENT MINUTE that that\'s SECOND bit quit resume david now today FINISHED-WORD)
  (FINISHED-WORD it all everything)
  (SPATIAL-BEGINNING-PAIR2 SPATIAL-BEGINNING there)
  (SPATIAL-BEGINNING-PAIR1 SPATIAL-BEGINNING)
  (SPATIAL-BEGINNING SPATIAL-VERB between PREP conj-prep)
  (SPATIAL-VERB BE MODAL WH_ DO)
  (SPATIAL-ENDING NOUN ADJ there DIRECTIONS ANA-PRON PREP conj-prep facing ADV-HIST-WORD PREP-HISTORY-ADJ VERB-REL that this those CORP)
  (SPATIAL-WORD NOUN-OBJ ANA-PRON supporting CORP ADJ uppermost UNDER CLOSE TOUCHING FARTHEST ROTATED VERB-REL)
  (SPATIAL-WORD-POTENTIAL SPATIAL-WORD BE WH_ PREP conj-prep)
  (KINDS types sorts kind type sort formats format)
  (QUESTION questions)
  (ANSWER UNDERSTAND hear interpret parse)
  (SPECIFICALLY exactly precisely)
  (HERE here\'s heres)
  (DIRECTIONS left right top bottom back front)
))
; This is the top level choice tree for processing spatial question inputs.
; Currently, redirect input to *asr-fix-tree* as the very first step.
(READRULES '*spatial-question-input*
'(
  1 (0)
    2 (*asr-fix-tree* (1)) (0 :subtree+clause)
))
; After fixing asr mistakes, we want to check whether the response _is_ a spatial question, which
; we can do pretty generally by checking if it has any spatial keyword in it. If so,
; we sent it to further subtrees to do some simple preprocessing. Otherwise, we can check
; for any number of "small talk" patterns, which we should also be able to handle.
(READRULES '*detect-smalltalk-tree*
'(
  ;; ----------------------------------------
  ;; For now, preempt any references/pronouns
  ;; ----------------------------------------
  ;; 1 (0 spatial-word-potential 0 ANA-PRON 0)
  ;;   2 ((Can I answer your question referring to a past question ?)) (0 :gist)
  ;; 1 (0 spatial-word-potential 0 that block 0)
  ;;   2 ((Can I answer your question referring to a past question ?)) (0 :gist)
  ;; 1 (0 spatial-word-potential 0 that one 0)
  ;;   2 ((Can I answer your question referring to a past question ?)) (0 :gist)
  ;; ----------------------------------------
  ;; If spatial question, start preprocessing
  ;; ----------------------------------------
  ; Avoid matching "that is finished"
  1 (0 that is .FINISHED-WORD 0)
    2 (*request-input* (1 2 3 4 5)) (0 :subtree+clause)
  ; Match any spatial word feature
  1 (0 .SPATIAL-WORD 0)
    2 (*multi-token-word-tree* (1 2 3)) (0 :subtree+clause)
  ;; -----------------
  ;; Special requests
  ;; -----------------
  1 (0 .SPECIAL-REQUEST 0)
    2 (*request-input* (1 2 3)) (0 :subtree+clause)
  ;; ----------------------------------
  ;; 'Indexical' explanation requests 
  ;; The following are interpreted as the slightly awkward gist clause
  ;; "why [not] my previous question ?", assuming that the "my previous question"
  ;; is replaced with the actual previous question ULF by the reference resolution module.
  ;; ----------------------------------
  1 (0 .AUX you .VERB-EXPLAIN 5)
    2 (0 .VERB-EXPLAIN 1 .WH-EXPLANATION .NEG 2)
      3 (*multi-token-word-tree* (why not my previous question ?)) (0 :subtree+clause)
    2 (*multi-token-word-tree* (why my previous question ?)) (0 :subtree+clause)
  1 (1 .VERB-EXPLAIN 1 .WH-EXPLANATION .NEG 2)
    2 (*multi-token-word-tree* (why not my previous question ?)) (0 :subtree+clause)
  1 (1 .VERB-EXPLAIN 3)
    2 (*multi-token-word-tree* (why my previous question ?)) (0 :subtree+clause)
  1 (1 .WH-EXPLANATION .DO you 5)
    2 (0 you .NEG 4)
      3 (*multi-token-word-tree* (why not my previous question ?)) (0 :subtree+clause)
    2 (*multi-token-word-tree* (why my previous question ?)) (0 :subtree+clause)
  1 (1 .WH-EXPLANATION .NEG 2)
    2 (*multi-token-word-tree* (why not my previous question ?)) (0 :subtree+clause)
  1 (1 .WH-EXPLANATION 2)
    2 (*multi-token-word-tree* (why my previous question ?)) (0 :subtree+clause)
  1 (1 what .BE 1 reasons 5)
    2 (0 reasons 0 .NEG 2)
      3 (*multi-token-word-tree* (why not my previous question ?)) (0 :subtree+clause)
    2 (*multi-token-word-tree* (why my previous question ?)) (0 :subtree+clause)
  ;; "Small talk" patterns
  ;; ---------------------
  1 (0 .WH_ 1 your name 0)
    2 ((What is my name ?)) (0 :gist)
  1 (0 .AUX you 1 .ANSWER 3 .QUESTION 0)
    2 ((Can I answer your question ?)) (0 :gist)
  1 (0 .AUX you 0)
    2 ((Can I do something ?)) (0 :gist)
  1 (0 .WH_ 1 .KINDS 2 .QUESTION 1 .AUX you 1 .ANSWER 0)
    2 ((What questions can I answer ?)) (0 :gist)
  1 (0)
    2 ((NIL Gist \: Eta could not understand your question \.)) (0 :gist)
))
; The first stage of preprocessing. Here we combine any words that have multiple tokens,
; e.g. "burger king" into a single word, joined by an underscore.
(READRULES '*multi-token-word-tree*
'(
  1 (0 how many 0)
    2 (*multi-token-word-tree* (1 how_many 4)) (0 :subtree+clause)
  1 (0 relative to 0)
    2 (*multi-token-word-tree* (1 relative_to 4)) (0 :subtree+clause)
  1 (0 with .RESPECT to 0)
    2 (*multi-token-word-tree* (1 with_respect_to 5)) (0 :subtree+clause)
  1 (0 burger king 0)
    2 (*multi-token-word-tree* (1 burger_king 4)) (0 :subtree+clause)
  1 (0 sri international 0)
    2 (*multi-token-word-tree* (1 sri_international 4)) (0 :subtree+clause)
  1 (0 next to 0)
    2 (*multi-token-word-tree* (1 next_to 4)) (0 :subtree+clause)
  1 (0 on top of 0)
    2 (*multi-token-word-tree* (1 on_top_of 5)) (0 :subtree+clause)
  1 (0 to the left of 0)
    2 (*multi-token-word-tree* (1 to_the_left_of 6)) (0 :subtree+clause)
  1 (0 to the right of 0)
    2 (*multi-token-word-tree* (1 to_the_right_of 6)) (0 :subtree+clause)
  1 (0 left of 0)
    2 (*multi-token-word-tree* (1 to_the_left_of 4)) (0 :subtree+clause)
  1 (0 right of 0)
    2 (*multi-token-word-tree* (1 to_the_right_of 4)) (0 :subtree+clause)
  1 (0 left to 0)
    2 (*multi-token-word-tree* (1 to_the_left_of 4)) (0 :subtree+clause)
  1 (0 right to 0)
    2 (*multi-token-word-tree* (1 to_the_right_of 4)) (0 :subtree+clause)
  1 (0 prior to 0)
    2 (*multi-token-word-tree* (1 prior_to 4)) (0 :subtree+clause)
  1 (0 near to 0)
    2 (*multi-token-word-tree* (1 near 4)) (0 :subtree+clause)
  1 (0 .CLOSE to 0)
    2 (*multi-token-word-tree* (1 near 4)) (0 :subtree+clause)
  1 (0 on to 0)
    2 (*multi-token-word-tree* (1 on_to 4)) (0 :subtree+clause)
  1 (0 onto 0)
    2 (*multi-token-word-tree* (1 on_to 3)) (0 :subtree+clause)
  1 (0 in front of 0)
    2 (*multi-token-word-tree* (1 in_front_of 5)) (0 :subtree+clause)
  1 (0 adjacent to 0)
    2 (*multi-token-word-tree* (1 adjacent_to 4)) (0 :subtree+clause)
  1 (0 flush with 0)
    2 (*multi-token-word-tree* (1 flush_with 4)) (0 :subtree+clause)
  1 (0 pick up 0)
    2 (*multi-token-word-tree* (1 pick_up 4)) (0 :subtree+clause)
  1 (0 picks up 0)
    2 (*multi-token-word-tree* (1 picks_up 4)) (0 :subtree+clause)
  1 (0 picked up 0)
    2 (*multi-token-word-tree* (1 picked_up 4)) (0 :subtree+clause)
  1 (0 consist of 0)
    2 (*multi-token-word-tree* (1 consist_of 4)) (0 :subtree+clause)
  1 (0 consists of 0)
    2 (*multi-token-word-tree* (1 consists_of 4)) (0 :subtree+clause)
  1 (0 consisted of 0)
    2 (*multi-token-word-tree* (1 consisted_of 4)) (0 :subtree+clause)
  ; room world compound nouns + plurals
  1 (0 .BOOK shelves 0)
    2 (*multi-token-word-tree* (1 book_shelves 4)) (0 :subtree+clause)
  1 (0 .BOOK shelf 0)
    2 (*multi-token-word-tree* (1 book_shelf 4)) (0 :subtree+clause)
  1 (0 coffee tables 0)
    2 (*multi-token-word-tree* (1 coffee_tables 4)) (0 :subtree+clause)
  1 (0 coffee .TABLE 0)
    2 (*multi-token-word-tree* (1 coffee_table 4)) (0 :subtree+clause)
  1 (0 .PINE trees 0)
    2 (*multi-token-word-tree* (1 pine_trees 4)) (0 :subtree+clause)
  1 (0 .PINE .TREE 0)
    2 (*multi-token-word-tree* (1 pine_tree 4)) (0 :subtree+clause)
  1 (0 .ANALOG clocks 0)
    2 (*multi-token-word-tree* (1 analog_clocks 4)) (0 :subtree+clause)
  1 (0 .ANALOG .CLOCK 0)
    2 (*multi-token-word-tree* (1 analog_clock 4)) (0 :subtree+clause)
  1 (0 .TOOTH brushes 0)
    2 (*multi-token-word-tree* (1 tooth_brushes 4)) (0 :subtree+clause)
  1 (0 .TOOTH .BRUSH 0)
    2 (*multi-token-word-tree* (1 tooth_brush 4)) (0 :subtree+clause)
  1 (0 recycling bins 0)
    2 (*multi-token-word-tree* (1 recycle_bins 4)) (0 :subtree+clause)
  1 (0 recycling .BIN 0)
    2 (*multi-token-word-tree* (1 recycle_bin 4)) (0 :subtree+clause)
  1 (0 recycle bins 0)
    2 (*multi-token-word-tree* (1 recycle_bins 4)) (0 :subtree+clause)
  1 (0 recycle .BIN 0)
    2 (*multi-token-word-tree* (1 recycle_bin 4)) (0 :subtree+clause)
  1 (0 .CEILING fans 0)
    2 (*multi-token-word-tree* (1 ceiling_fans 4)) (0 :subtree+clause)
  1 (0 .CEILING .FAN 0)
    2 (*multi-token-word-tree* (1 ceiling_fan 4)) (0 :subtree+clause)
  1 (0 .CEILING lights 0)
    2 (*multi-token-word-tree* (1 ceiling_lights 4)) (0 :subtree+clause)
  1 (0 .CEILING .LIGHT 0)
    2 (*multi-token-word-tree* (1 ceiling_light 4)) (0 :subtree+clause)
  1 (0 .CARD boxes 0)
    2 (*multi-token-word-tree* (1 card_boxes 4)) (0 :subtree+clause)
  1 (0 .CARD .BOX 0)
    2 (*multi-token-word-tree* (1 card_box 4)) (0 :subtree+clause)
  1 (0 .FOOT rests 0)
    2 (*multi-token-word-tree* (1 foot_rests 4)) (0 :subtree+clause)
  1 (0 .FOOT rest 0)
    2 (*multi-token-word-tree* (1 foot_rest 4)) (0 :subtree+clause)
  1 (0 .PENCIL holders 0)
    2 (*multi-token-word-tree* (1 pencil_holders 4)) (0 :subtree+clause)
  1 (0 .PENCIL .HOLDER 0)
    2 (*multi-token-word-tree* (1 pencil_holder 4)) (0 :subtree+clause)
  1 (0 .FLOOR lamps 0)
    2 (*multi-token-word-tree* (1 floor_lamps 4)) (0 :subtree+clause)
  1 (0 .FLOOR .LAMP 0)
    2 (*multi-token-word-tree* (1 floor_lamp 4)) (0 :subtree+clause)
  1 (0 rocking chairs 0)
    2 (*multi-token-word-tree* (1 rocking_chairs 4)) (0 :subtree+clause)
  1 (0 rocking .CHAIR 0)
    2 (*multi-token-word-tree* (1 rocking_chair 4)) (0 :subtree+clause)
  1 (0)
    2 (*trim-suffix-tree* (1)) (0 :subtree+clause)
))
; The second stage of preprocessing. We want to remove any "suffix" that the user might
; throw after the query, such as tag questions. We do this by trimming off everything at
; the end that isn't a spatial-ending (noun or adj). Right now this is being done in a rather
; unwieldy way, due to the problem of recursion (i.e. an input can theoretically have any number
; of spatial-ending words).
(READRULES '*trim-suffix-tree*
'(
  1 (3 where 0 need to .BE ?)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 ?)) (0 :subtree+clause)
  1 (3 where 1 should 0 .BE ?)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING ?)
    2 (*trim-prefix-tree* (1 2 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING)
    2 (*trim-prefix-tree* (1 2 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 7 8 9 10 11 12 13 14 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 7 8 9 10 11 12 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 7 8 9 10 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 7 8 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 3 4 5 6 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 3 4 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-ENDING 0)
    2 (*trim-prefix-tree* (1 2 ?)) (0 :subtree+clause)
  1 (0 .SPATIAL-WORD 0)
    2 (*trim-prefix-tree* (1 2 ?)) (0 :subtree+clause)
  1 (0 explain 0 .NOUN-EXPLAIN 0)
    2 (*trim-prefix-tree* (1 2 3 4 ?)) (0 :subtree+clause)
))
; The third stage of preprocessing. We want to remove any "prefix" that the user might
; use as an opening, e.g. "my question is ...".
(READRULES '*trim-prefix-tree*
'(
  1 (where .SPECIFICALLY 0)
    2 (*trim-prefix-tree* (1 3)) (0 :subtree+clause)
  1 (yes I .DO \, 0)
    2 (*trim-prefix-tree* (5)) (0 :subtree+clause)
  1 (yes I .DO 0)
    2 (*trim-prefix-tree* (4)) (0 :subtree+clause)
  1 (yes \, 0)
    2 (*trim-prefix-tree* (3)) (0 :subtree+clause)
  1 (yes 0)
    2 (*trim-prefix-tree* (2)) (0 :subtree+clause)
  1 (0 .HERE 1 my 1 .QUESTION 0)
    2 (*trim-prefix-tree* (7)) (0 :subtree+clause)
  1 (0 my 1 .QUESTION 2 .BE this 0)
    2 (*trim-prefix-tree* (8)) (0 :subtree+clause)
  1 (0 my 1 .QUESTION 2 .BE 0)
    2 (*trim-prefix-tree* (7)) (0 :subtree+clause)
  ; Detect and simplify 'explanation' prefixes
  1 (what .BE .DET .REASON that 0)
    2 (what .BE .DET .REASON that 0 .BE 0)
      3 (*swap-pronoun-tree* (spatial-question why is 6 8)) (0 :subtree+clause)
    2 (*swap-pronoun-tree* (spatial-question why do 6)) (0 :subtree+clause)
  1 (.WH-EXPLANATION .BE 1 true that 0)
    2 (.WH-EXPLANATION .BE 1 true that 0 .BE 0)
      3 (*swap-pronoun-tree* (spatial-question why is 6 8)) (0 :subtree+clause)
    2 (*swap-pronoun-tree* (spatial-question why do 6)) (0 :subtree+clause)
  1 (.VERB-EXPLAIN 1 .WH-EXPLANATION you .VERB-ANSWER that .NP_ 0)
    2 (.VERB-EXPLAIN 1 .WH-EXPLANATION you .VERB-ANSWER that .NP_ 0 .BE 0)
      3 (*swap-pronoun-tree* (spatial-question why is 7 8 10)) (0 :subtree+clause)
    2 (*swap-pronoun-tree* (spatial-question why do 7 8)) (0 :subtree+clause)
  1 (.VERB-EXPLAIN 1 .WH-EXPLANATION you .VERB-ANSWER .NP_ 0)
    2 (.VERB-EXPLAIN 1 .WH-EXPLANATION you .VERB-ANSWER .NP_ 0 .BE 0)
      3 (*swap-pronoun-tree* (spatial-question why is 6 7 9)) (0 :subtree+clause)
    2 (*swap-pronoun-tree* (spatial-question why do 6 7)) (0 :subtree+clause)
  1 (.VERB-EXPLAIN 1 .WH-EXPLANATION 0)
    2 (.VERB-EXPLAIN 1 .WH-EXPLANATION 0 .BE 0)
      3 (*swap-pronoun-tree* (spatial-question why is 4 6)) (0 :subtree+clause)
    2 (*swap-pronoun-tree* (spatial-question why do 4)) (0 :subtree+clause)
  1 (1 .WH-EXPLANATION .AUX you .VERB-ANSWER that .NP_ 0)
    2 (1 .WH-EXPLANATION .AUX you .VERB-ANSWER that .NP_ 0 .BE 0)
      3 (*swap-pronoun-tree* (spatial-question why is 7 8 10)) (0 :subtree+clause)
    2 (*swap-pronoun-tree* (spatial-question why do 7 8)) (0 :subtree+clause)
  1 (1 .WH-EXPLANATION .AUX you .VERB-ANSWER .NP_ 0)
    2 (1 .WH-EXPLANATION .AUX you .VERB-ANSWER .NP_ 0 .BE 0)
      3 (*swap-pronoun-tree* (spatial-question why is 6 7 9)) (0 :subtree+clause)
    2 (*swap-pronoun-tree* (spatial-question why do 6 7)) (0 :subtree+clause)
  ; Other one-off prefixes
  1 (0 .AUX you 1 know if 0)
    2 (*swap-pronoun-tree* (spatial-question 7)) (0 :subtree+clause)
  1 (0 .AUX you 1 know 0)
    2 (*swap-pronoun-tree* (spatial-question 6)) (0 :subtree+clause)
  1 (0 .AUX you 1 see if 0)
    2 (*swap-pronoun-tree* (spatial-question 7)) (0 :subtree+clause)
  1 (0 .AUX you 1 see 0)
    2 (*swap-pronoun-tree* (spatial-question 6)) (0 :subtree+clause)
  1 (0 .AUX you 1 tell 1 if 0)
    2 (*swap-pronoun-tree* (spatial-question 8)) (0 :subtree+clause)
  1 (0 .AUX you 1 tell me 0)
    2 (*swap-pronoun-tree* (spatial-question 7)) (0 :subtree+clause)
  1 (0 .AUX you 1 tell 0)
    2 (*swap-pronoun-tree* (spatial-question 6)) (0 :subtree+clause)
  1 (0 .SPATIAL-BEGINNING-PAIR1 .SPATIAL-BEGINNING-PAIR2 .SPATIAL-BEGINNING-PAIR1 (:c | meant to match something|) .SPATIAL-BEGINNING-PAIR2 0) ; like "is there...what is next
    ; to the red block?"
    2 (*swap-pronoun-tree* (spatial-question 4 5 6)) (0 :subtree+clause)
  1 (between .SPATIAL-BEGINNING 0)
    2 (*swap-pronoun-tree* (spatial-question 1 2 3)) (0 :subtree+clause)
  1 (.WH_ .SPATIAL-BEGINNING 0)
    2 (*swap-pronoun-tree* (spatial-question 1 2 3)) (0 :subtree+clause)
  1 (.PREP .SPATIAL-BEGINNING 0)
    2 (*swap-pronoun-tree* (spatial-question 1 2 3)) (0 :subtree+clause)
  1 (NIL so .SPATIAL-BEGINNING 0)
    2 (*swap-pronoun-tree* (spatial-question 3 4)) (0 :subtree+clause)
  1 (NIL \, .SPATIAL-BEGINNING 0)
    2 (*swap-pronoun-tree* (spatial-question 3 4)) (0 :subtree+clause)
  1 (NIL .SPATIAL-BEGINNING 0)
    2 (*swap-pronoun-tree* (spatial-question 2 3)) (0 :subtree+clause)
  1 (0)
    2 (*swap-pronoun-tree* (spatial-question 1)) (0 :subtree+clause)
))
; The final stage of preprocessing; we want to swap me/you pronouns so gist-clauses reference
; a system-centric viewpoint. Note that this has to be done in stages using a
; 'placeholder' set of pronouns, or else it would get stuck in an infinite cycle.
; Also, this might be unreliable in the case of 'you' -> 'I', since sometimes it should map to 'me' instead.
; However, in this case we expect that spatial questions will generally involve the user referring to themselves,
; and thus use 'I'/'me' -> 'you'.
(READRULES '*swap-pronoun-tree*
'(
  1 (0 you are 0)
    2 (*swap-pronoun-tree* (1 xou are 4)) (0 :subtree+clause)
  1 (0 you 0)
    2 (*swap-pronoun-tree* (1 xou 3)) (0 :subtree+clause)
  1 (0 your 0)
    2 (*swap-pronoun-tree* (1 xour 3)) (0 :subtree+clause)
  1 (0)
    2 (*swap-pronoun-tree1* (1)) (0 :subtree+clause)
))


(READRULES '*swap-pronoun-tree1*
'(
  1 (0 I am 0)
    2 (*swap-pronoun-tree1* (1 you are 4)) (0 :subtree+clause)
  1 (0 I 0)
    2 (*swap-pronoun-tree1* (1 you 3)) (0 :subtree+clause)
  1 (0 me 0)
    2 (*swap-pronoun-tree1* (1 you 3)) (0 :subtree+clause)
  1 (0 my 0)
    2 (*swap-pronoun-tree1* (1 your 3)) (0 :subtree+clause)
  1 (0)
    2 (*swap-pronoun-tree2* (1)) (0 :subtree+clause)
))


(READRULES '*swap-pronoun-tree2*
'(
  1 (0 xou are 0)
    2 (*swap-pronoun-tree2* (1 I am 4)) (0 :subtree+clause)
  1 (0 xou 0)
    2 (*swap-pronoun-tree2* (1 I 3)) (0 :subtree+clause)
  1 (0 xour 0)
    2 (*swap-pronoun-tree2* (1 my 3)) (0 :subtree+clause)
  1 (0)
    2 ((1)) (0 :gist)
))
; We want to check for common ASR mistakes, and map those to the (most plausibly)
; correct input.
; NOTE: Moved closer to end of file since the code was getting pretty long.
(READRULES '*asr-fix-tree*
'(
  1 (0 .DO not .PRON 0)
    2 (*asr-fix-tree* (1 2 4 3 5)) (0 :subtree+clause)
  1 (0 anything 0) ; not a mistake per se, but want to split into two words for parsing
    2 (*asr-fix-tree* (1 any thing 3)) (0 :subtree+clause)
  1 (0 mcdonald\'s 0)
    2 (*asr-fix-tree* (1 mcdonalds 3)) (0 :subtree+clause)
  1 (0 mcdonalds black 0)
    2 (*asr-fix-tree* (1 mcdonalds block 4)) (0 :subtree+clause)
  1 (0 show 0)
    2 (*asr-fix-tree* (1 shell 3)) (0 :subtree+clause)
  1 (0 sra 0)
    2 (*asr-fix-tree* (1 sri 3)) (0 :subtree+clause)
  1 (0 s or I 0)
    2 (*asr-fix-tree* (1 sri 5)) (0 :subtree+clause)
  1 (0 meats are I 0)
    2 (*asr-fix-tree* (1 sri 5)) (0 :subtree+clause)
  1 (0 meats \? are I 0)
    2 (*asr-fix-tree* (1 sri 5)) (0 :subtree+clause)
  1 (0 meats ? are I 0)
    2 (*asr-fix-tree* (1 sri 6)) (0 :subtree+clause)
  1 (0 bsri 0)
    2 (*asr-fix-tree* (1 the sri 3)) (0 :subtree+clause)
  1 (0 bsr I 0)
    2 (*asr-fix-tree* (1 the sri 4)) (0 :subtree+clause)
  1 (0 psri 0)
    2 (*asr-fix-tree* (1 the sri 3)) (0 :subtree+clause)
  1 (0 psr I 0)
    2 (*asr-fix-tree* (1 the sri 4)) (0 :subtree+clause)
  1 (0 dsri 0)
    2 (*asr-fix-tree* (1 the sri 3)) (0 :subtree+clause)
  1 (0 dsr I 0)
    2 (*asr-fix-tree* (1 the sri 4)) (0 :subtree+clause)
  1 (0 esri 0)
    2 (*asr-fix-tree* (1 the sri 3)) (0 :subtree+clause)
  1 (0 esr I 0)
    2 (*asr-fix-tree* (1 the sri 4)) (0 :subtree+clause)
  1 (0 ssri .BLOCK 0)
    2 (*asr-fix-tree* (1 sri block 4)) (0 :subtree+clause)
  1 (0 sr \. I .BLOCK 0)
    2 (*asr-fix-tree* (1 sri block 5)) (0 :subtree+clause)
  1 (0 sr \. I .BLOCK 0)
    2 (*asr-fix-tree* (1 sri block 6)) (0 :subtree+clause)
  1 (0 psr I .BLOCK 0)
    2 (*asr-fix-tree* (1 the sri block 5)) (0 :subtree+clause)
  1 (0 they survived look 0)
    2 (*asr-fix-tree* (1 the sri block 5)) (0 :subtree+clause)
  1 (0 in their survival 0)
    2 (*asr-fix-tree* (1 and the sri block 5)) (0 :subtree+clause)
  1 (0 novita 0)
    2 (*asr-fix-tree* (1 nvidia 3)) (0 :subtree+clause)
  1 (0 univita 0)
    2 (*asr-fix-tree* (1 nvidia 3)) (0 :subtree+clause)
  1 (0 aveda 0)
    2 (*asr-fix-tree* (1 nvidia 3)) (0 :subtree+clause)
  1 (0 play media 0)
    2 (*asr-fix-tree* (1 the nvidia 4)) (0 :subtree+clause)
  1 (0 play video 0)
    2 (*asr-fix-tree* (1 the nvidia 4)) (0 :subtree+clause)
  1 (0 visiting aveda .BLOCK 0)
    2 (*asr-fix-tree* (1 is the nvidia block 5)) (0 :subtree+clause)
  1 (0 ultra 0)
    2 (*asr-fix-tree* (1 toyota 3)) (0 :subtree+clause)
  1 (0 yoda 0)
    2 (*asr-fix-tree* (1 toyota 3)) (0 :subtree+clause)
  1 (0 traffic 0)
    2 (*asr-fix-tree* (1 target 3)) (0 :subtree+clause)
  1 (0 chopping 0)
    2 (*asr-fix-tree* (1 target 3)) (0 :subtree+clause)
  1 (0 pocket 0)
    2 (*asr-fix-tree* (1 target 3)) (0 :subtree+clause)
  1 (0 charger 0)
    2 (*asr-fix-tree* (1 target 3)) (0 :subtree+clause)
  1 (0 talking .BLOCK 0)
    2 (*asr-fix-tree* (1 target block 4)) (0 :subtree+clause)
  1 (0 texican 0)
    2 (*asr-fix-tree* (1 texaco 3)) (0 :subtree+clause)
  1 (0 mexico 0)
    2 (*asr-fix-tree* (1 texaco 3)) (0 :subtree+clause)
  1 (0 texas call 0)
    2 (*asr-fix-tree* (1 texaco 4)) (0 :subtree+clause)
  1 (0 critter 0)
    2 (*asr-fix-tree* (1 twitter 3)) (0 :subtree+clause)
  1 (0 butcher 0)
    2 (*asr-fix-tree* (1 twitter 3)) (0 :subtree+clause)
  1 (0 mass of the 0)
    2 (*asr-fix-tree* (1 mercedes 5)) (0 :subtree+clause)
  1 (0 merced us 0)
    2 (*asr-fix-tree* (1 mercedes 4)) (0 :subtree+clause)
  1 (0 merced is 0)
    2 (*asr-fix-tree* (1 mercedes 4)) (0 :subtree+clause)
  1 (0 mercer does 0)
    2 (*asr-fix-tree* (1 mercedes 4)) (0 :subtree+clause)
  1 (0 messages 0)
    2 (*asr-fix-tree* (1 mercedes 3)) (0 :subtree+clause)
  1 (0 merciless 0)
    2 (*asr-fix-tree* (1 mercedes 3)) (0 :subtree+clause)
  1 (0 varsity sports 0)
    2 (*asr-fix-tree* (1 mercedes 4)) (0 :subtree+clause)
  1 (0 in the .TABLE 0)
    2 (*asr-fix-tree* (1 on the table 5)) (0 :subtree+clause)
  1 (0 in a cup 0)
    2 (*asr-fix-tree* (1 on top 5)) (0 :subtree+clause)
  1 (0 involved 0)
    2 (*asr-fix-tree* (1 above 3)) (0 :subtree+clause)
  1 (0 about 0)
    2 (*asr-fix-tree* (1 above 3)) (0 :subtree+clause)
  1 (0 above to 0)
    2 (*asr-fix-tree* (1 above the 4)) (0 :subtree+clause)
  1 (0 after the right 0)
    2 (*asr-fix-tree* (1 are to the right 5)) (0 :subtree+clause)
  1 (0 a mirror to 0)
    2 (*asr-fix-tree* (1 nearer to 5)) (0 :subtree+clause)
  1 (0 hyatt 0)
    2 (*asr-fix-tree* (1 highest 3)) (0 :subtree+clause)
  1 (0 louis 0)
    2 (*asr-fix-tree* (1 lowest 3)) (0 :subtree+clause)
  1 (0 lymph nodes look 0)
    2 (*asr-fix-tree* (1 leftmost block 5)) (0 :subtree+clause)
  1 (0 rifles 0)
    2 (*asr-fix-tree* (1 rightmost 3)) (0 :subtree+clause)
  1 (0 right most 0)
    2 (*asr-fix-tree* (1 rightmost 4)) (0 :subtree+clause)
  1 (0 left most 0)
    2 (*asr-fix-tree* (1 leftmost 4)) (0 :subtree+clause)
  1 (0 front most 0)
    2 (*asr-fix-tree* (1 frontmost 4)) (0 :subtree+clause)
  1 (0 top most 0)
    2 (*asr-fix-tree* (1 topmost 4)) (0 :subtree+clause)
  1 (0 right-most 0)
    2 (*asr-fix-tree* (1 rightmost 3)) (0 :subtree+clause)
  1 (0 left-most 0)
    2 (*asr-fix-tree* (1 leftmost 3)) (0 :subtree+clause)
  1 (0 front-most 0)
    2 (*asr-fix-tree* (1 frontmost 3)) (0 :subtree+clause)
  1 (0 top-most 0)
    2 (*asr-fix-tree* (1 topmost 3)) (0 :subtree+clause)
  1 (0 metal 0)
    2 (*asr-fix-tree* (1 middle 3)) (0 :subtree+clause)
  1 (0 patch 0)
    2 (*asr-fix-tree* (1 touch 3)) (0 :subtree+clause)
  1 (0 punches 0)
    2 (*asr-fix-tree* (1 touches 3)) (0 :subtree+clause)
  1 (0 punching 0)
    2 (*asr-fix-tree* (1 touching 3)) (0 :subtree+clause)
  1 (0 patching 0)
    2 (*asr-fix-tree* (1 touching 3)) (0 :subtree+clause)
  1 (0 catching 0)
    2 (*asr-fix-tree* (1 touching 3)) (0 :subtree+clause)
  1 (0 cashing 0)
    2 (*asr-fix-tree* (1 touching 3)) (0 :subtree+clause)
  1 (0 flashing 0)
    2 (*asr-fix-tree* (1 touching 3)) (0 :subtree+clause)
  1 (0 flushing 0)
    2 (*asr-fix-tree* (1 touching 3)) (0 :subtree+clause)
  1 (0 fashion 0)
    2 (*asr-fix-tree* (1 touching 3)) (0 :subtree+clause)
  1 (0 stock 0)
    2 (*asr-fix-tree* (1 stack 3)) (0 :subtree+clause)
  1 (0 .HOUR 0)
    2 (*asr-fix-tree* (1 tower 3)) (0 :subtree+clause)
  1 (0 power 0)
    2 (*asr-fix-tree* (1 tower 3)) (0 :subtree+clause)
  1 (0 boxer 0)
    2 (*asr-fix-tree* (1 blocks are 3)) (0 :subtree+clause)
  1 (0 gridlock 0)
    2 (*asr-fix-tree* (1 green block 3)) (0 :subtree+clause)
  1 (0 cube 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 .BOOK 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 black 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 glock 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 blog 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 bach 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 blood 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 glass 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 .BOX 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 look 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 walk 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 wok 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 lock 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 vlog 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 blocked 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 talk 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 cook 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 .CLOCK 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 plug 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 blonde 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 .LOVER 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 boardwalk 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 blow 0)
    2 (*asr-fix-tree* (1 block 3)) (0 :subtree+clause)
  1 (0 blockus 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 blokus 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 cubes 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 books 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 blacks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 glocks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 blogs 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 bach\'s 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 bloods 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 looks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 walks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 woks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 locks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 vlogs 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 talks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 cooks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 clocks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 plugs 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 blondes 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 lovers 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 boardwalks 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 blows 0)
    2 (*asr-fix-tree* (1 blocks 3)) (0 :subtree+clause)
  1 (0 .ROSE 0)
    2 (*asr-fix-tree* (1 rows 3)) (0 :subtree+clause)
  1 (0 brett 0)
    2 (*asr-fix-tree* (1 red 3)) (0 :subtree+clause)
  1 (0 rim 0)
    2 (*asr-fix-tree* (1 green 3)) (0 :subtree+clause)
  1 (0 song 0)
    2 (*asr-fix-tree* (1 some 3)) (0 :subtree+clause)
  1 (0 sound 0)
    2 (*asr-fix-tree* (1 some 3)) (0 :subtree+clause)
  1 (0 sun 0)
    2 (*asr-fix-tree* (1 some 3)) (0 :subtree+clause)
  1 (0)
    2 (*grammar-fix-tree* (1)) (0 :subtree+clause)
))
; Preprocess certain common but ungrammatical queries.
(READRULES '*grammar-fix-tree*
'(
  1 (0 not ever 0) ; not ever => never
    2 (*grammar-fix-tree* (1 never 4)) (0 :subtree+clause)
  1 (.WH-DET .NOUN .PRON .VERB-REL 0) ; e.g., what block I moved first ?
    2 (*grammar-fix-tree* (1 2 did 3 4 5)) (0 :subtree+clause)
  1 (.WH-DET .NOUN-HISTORY 0 .PREP 0) ; e.g., what turn was the Twitter block on the Texaco block ?
    2 (*grammar-fix-tree* (at 1 2 3 4 5)) (0 :subtree+clause)
  1 (.WH-DET .NOUN-HISTORY 0 .VERB-REL 0) ; e.g., what turn did I move the Twitter block ?
    2 (*grammar-fix-tree* (at 1 2 3 4 5)) (0 :subtree+clause)
  1 (0 .HAVE not I 0) ; have not I => have I not
    2 (*grammar-fix-tree* (1 have I not 5)) (0 :subtree+clause)
  1 (0)
    2 (*detect-smalltalk-tree* (1)) (0 :subtree+clause)
))