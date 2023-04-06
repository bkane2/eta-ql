; June 8/19
;=================================================================
; These are the general word data used by a ETA1 (initial version
; derived as a variant of doolittle). [This file is derived from
; data.lisp, but excluding the doolittle rule tree.] They consist of
;
; 1. A list of contractions and their corresponding expansions; 
;    e.g., (SHE'S SHE IS) (DON'T DO NOT), etc. The expanded versions 
;    are inserted on the property lists of the contractions, under 
;    indicator TWOWORDS. The TWOWORDS property is used in an obvious 
;    way to preprocess user inputs, replacing contractions by their 
;    expanded versions.
;
; 2. The next set of items supplies contracted negations of auxiliary 
;    words, under indicator NEG. E.g., (IS ISN'T), (DO DON'T), etc. 
;    These are used in an obvious way to condense outputs just 
;    before they are printed. E.g., WHY DO NOT YOU ... is condensed 
;    to WHY DON'T YOU ... by noticing that DO has a non-NIL value 
;    for NEG, and is followed by NOT.
;
; 3. Next is a list of substitutions to be made in computing the dual
;    of an output, before actual printing. The substition is stored 
;    under the SUBST property of the word. The DUALS function makes 
;    each of the two given words the SUBST property of the other.
;
; 4. The next set provides a very extensive set of features (tags) 
;    that seem useful in formulating decomposition rules. (Not all 
;    of them are as yet used.)
;

(in-package :eta)

(MAPC (LAMBDA (TRIPLE) (SETF (GET (CAR TRIPLE) 'TWOWORDS) (CDR TRIPLE)))
'(
  (DON\'T DO NOT)
  (DONT DO NOT)
  (DOESN\'T DOES NOT)
  (DIDN\'T DID NOT)
  (WON\'T WILL NOT)
  (WONT WILL NOT)
  (CAN\'T CAN NOT)
  (CANT CAN NOT)
  (CANNOT CAN NOT)
  (COULDN\'T COULD NOT)
  (WOULDN\'T WOULD NOT)
  (SHOULDN\'T SHOULD NOT)
  (AREN\'T ARE NOT)
  (ISN\'T IS NOT)
  (WASN\'T WAS NOT)
  (WEREN\'T WERE NOT)
  (MIGHTN\'T MIGHT NOT)
  (HAVEN\'T HAVE NOT)
  (HASN\'T HAS NOT)
  (HADN\'T HAD NOT)
  (I\'M I AM)
  (I\'LL I WILL)
  (I\'D I WOULD)
  (YOU\'RE YOU ARE)
  (YOU\'LL YOU WILL)
  (YOU\'D YOU WOULD)
  (HE\'S HE IS)
  (HE\'LL HE WILL)
  (HE\'D HE WOULD)
  (SHE\'S SHE IS)
  (SHE\'LL SHE WILL)
  (SHE\'D SHE WOULD)
  (IT\'S IT IS)
  (IT\'LL IT WILL)
  (WE\'RE WE ARE)
  (WE\'LL WE WILL)
  (WE\'D WE WOULD)
  (THEY\'RE THEY ARE)
  (THEY\'LL THEY WILL)
  (THEY\'D THEY WOULD)
  (I\'VE I HAVE)
  (YOU\'VE YOU HAVE)
  (WE\'VE WE HAVE)
  (THEY\'VE THEY HAVE)
  (GONNA GOING TO)
  (WANNA WANT TO)
  (THAT\'S THAT IS)
  (HOW\'S HOW IS)
))

(MAPC (LAMBDA (PAIR) (SETF (GET (CAR PAIR) 'NEG) (CADR PAIR)))
'(
  (DO DON\'T)
  (DID DIDN\'T)
  (DOES DOESN\'T)
  (WILL WON\'T)
  (CAN CAN\'T)
  (COULD COULDN\'T)
  (WOULD WOULDN\'T)
  (SHOULD SHOULDN\'T)
  (ARE AREN\'T)
  (IS ISN\'T)
  (WAS WASN\'T)
  (WERE WEREN\'T)
  (HAVE HAVEN\'T)
  (HAS HASN\'T)
  (HAD HADN\'T)
))


(DUALS 'I 'YOU)
(SETF (GET 'ME 'SUBST) 'YOU)
(SETF (GET 'YOU1 'SUBST) 'I)
(SETF (GET 'YOU2 'SUBST) 'ME) ; objective case!
(DUALS 'MY 'YOUR)
(DUALS 'MINE 'YOURS)
(DUALS 'MYSELF 'YOURSELF)
(SETF (GET 'AM 'SUBST) 'ARE)
(SETF (GET 'ARE2 'SUBST) 'AM) ; second person! (after YOU)
(SETF (GET 'WAS2 'SUBST) 'WERE) ; after I?
(SETF (GET 'WERE2 'SUBST) 'WAS) ; after YOU?


; Features:
(MAPC 'ATTACHFEAT
'(
  (FINISH finished done quit stop terminate)
  (FOREIGN francais deutsch italiano espanol francais \? deutsch \? italiano \? espanol \?)
  (END-PUNC - ? ! \. \: \;)
  (CLAUSE-PUNC END-PUNC \,)
  (THEME-KEY pet-key chat1-opening chat1-rochester chat1-movies
    ; we could add other keys, as a flag for
    ; adhering to a particular theme (i.e., capture
  ) ; by a particular cluster of rules)

  ; Numbers
  (ONE |1|)
  (TWO |2|)
  (THREE |3|)
  (FOUR |4|)
  (FIVE |5|)
  (SIX |6|)
  (SEVEN |7|)
  (EIGHT |8|)
  (NINE |9|)
  (TEN |10|)
  (ELEVEN |11|)
  (TWELVE |12|)
  (THIRTEEN |13|)
  (FOURTEEN |14|)
  (FIFTEEN |15|)
  (SIXTEEN |16|)
  (SEVENTEEN |17|)
  (EIGHTEEN |18|)
  (NINETEEN |19|)
  (TWENTY |20|)
  (THIRTY |30|)
  (FORTY |40|)
  (FIFTY |50|)
  (SIXTY |60|)
  (SEVENTY |70|)
  (EIGHTY |80|)
  (NINETY |90|)

  ; Pronouns
  (INDEX-PRON I you me us mine yours ours)
  (QUANT-PRON someone everyone anyone something everything
			everybody nobody somebody anybody anything nothing)
  (REFL-PRON oneself myself yourself himself herself itself
			ourselves yourselves themselves)
  (ANA-PRON he she it they him her them hers theirs)
  (ANAPHOR ANA-PRON his her its their)
  (REL-PRON that which when who whom)
  (PRON INDEX-PRON QUANT-PRON REFL-PRON ANA-PRON WH-PRON REL-PRON)
  (WH-DET which what whose how_many)
  (WH-PRON who whom what which)
  (CLEFT-PRON it that there)

  (CLEFT it\'s that\'s there\'s)
  (INDEX-DET that those these this)
  (DET the a an my your his her its our their all every each any INDEX-DET
			some many ONE TWO THREE another other WH-DET TWO THREE FOUR FIVE SIX
			SEVEN EIGHT NINE TEN ELEVEN TWELVE)
  (NP_ PRON DET) ; the beginning of a noun phrase
  (MODAL can will shall could would should might may ought)
  (CAN could)
  (CAN\'T couldn\'t)
  (INTEROG that whether if)
  (NECESSITY need HAVE)
  (HAVE has had)
  (BE am are is was were)
  (DO does did doing)
  (AUX-BASE HAVE BE DO) ; "basic", i.e. non-modal auxiliary verbs
  (AUX MODAL HAVE BE DO)
  (FREQ often frequently many few lots)
  (TIMEADV today yesterday tomorrow often sometimes seldom never rarely
			always constantly now)
  (DEG-ADV not just VERY only exactly precisely 	  ; name changed from 'WH-ADV' 6/17/19
  		immediately right slightly directly flush up) ; "directly on", "flush against"
  (INITADV TIMEADV DEG-ADV)
  (CONJ but and or)
  (WH_ WH-DET WH-PRON why how when where) ; begin'g of wh-ques
  (QUANT POSQUANT NEGQUANT)
  (POSQUANT always all everyone everything every everybody constantly)
  (NEGQUANT never nothing noone no-one nobody)
  (ADV POSADV NEGADV INITADV S-ADV)
  (POSADV always sometimes certainly course absolutely sure ok okay occasionally
			mostly yesterday tomorrow frequently usually constantly probably
			really truly obviously naturally ok \, also so)
  (NEGADV seldom rarely never almost)
  (S-ADV PERHAPS)
  (PERHAPS maybe possibly)
  (SUPPOSE guess imagine hope) ; aimed at things like "I suppose so"
  (TENTATIVE PERHAPS SUPPOSE)
  (DOUBT unlikely hardly not) ; aimed at things like "I doubt it",
  												    ; "I don't think so", "That's unlikely"
  (RECOMMEND suggest endorse advocate approve)
  (COMMUNICATIVE say tell answer)
  (GENERAL-PREP for with)

  (SELF I my myself me)
  (FATHER dad)
  (MOTHER mom mommy)
  (CHILD children kid kids)
  (SON sons)
  (DAUGHTER daughters)
  (SPOUSE husband wife)
  (GRANDFATHER granddad grandpa)
  (GRANDMOTHER granny grandma)
  (FAMILY1 FATHER MOTHER SON DAUGHTER SPOUSE CHILD SON DAUGHTER)
  (FAMILY2 GRANDMOTHER GRANDFATHER cousin niece nephew uncle aunt mother-in-law father-in-law)
  (FAMILY FAMILY1 FAMILY2)

  (MONEY cash assets financial)
  (NOMONEY debt broke)
  (MONEYTHEME MONEY NOMONEY bank account mortgage payments ends fortune)
  (TROUBLE troubles DIFFICULT difficulty difficulties tough hard hardship challenging
			problem problems struggle struggling)
  (NOMORE rid lost gone left away out)

  (DENY no nah nope)
  (UNTRUTH-ADV incorrect inaccurate untrue)
  (DENY-ADV not never zero hardly little barely scarcely incorrect inaccurate untrue)
  (DISAGREE disagreed doubt)
  (NEG DENY DENY-ADV DISAGREE)
  (NEG-MOD really quite very exactly entirely all completely)
  (UNCERTAINTY-ADV unsure uncertain unsure unknown undetermined unforeseeable unpredictable debatable)
  (AFFIRM yes yeah yup indeed)
  (CERTAINTY-ADV certainly certain sure)
  (TRUTH-ADV correct accurate true)
  (AFFIRM-ADV certainly certain absolutely really quite completely exactly entirely sure definitely correct accurate true)
  (AGREE agreed)
  (POS AFFIRM AFFIRM-ADV AGREE)
  (AFFIRM-BELIEF know think believe feel understand understanding belief beliefs thought feeling)

  (MARRIAGETHEME marriage SPOUSE DIVORCE)
  (DIVORCE divorced separation separated)
  (LOVELIFE LOVE LOVER sex)
  (LOVER man woman girlfriend boyfriend affair mistress)
  (SCHOOLTHEME SCHOOLWORK TEACHER)
  (SCHOOLWORK school class classes course courses assignment essay essays homework
			assignments exam exams test tests grades marks study studies)
  (TEACHER teachers instructor instructors professor professors lecturer
			lecturers colleague colleagues)
  (WORKTHEME WORK-BOSS SALARY office co-workers)
  (WORK-BOSS WORK BOSS)
  (WORK working worked job jobs)
  (BOSS employer supervisor chairman)
  (SALARY raise pay)
  (SOCIALLIFE FRIEND FUN)
  (FRIEND friends buddy buddies pal pals acquaintance acquaintances
			friendship friendships)
  (FUN social party parties dance dances dancing movie concert visit
			visiting invite invited) ;SHOW
  (PET-TYPE dog dogs cat cats canary budgy budgie budgerigar parrot
			goldfish gerbil hamster guinea-pig guineapig guinea pig iguana turtle horse pony)
  (PETTHEME pet-key PET-TYPE pets pet)

  (BELIEVE think SUPPOSE suspect presume guess)
  (REMEMBER recall)
  (PERCEIVE notice note see infer conclude realize)
  (CONC BELIEVE REMEMBER PERCEIVE know)
  (BADSTATE unhappy sad worried TIRED depressed appalled terrible awful
			lonely disgusted upset bored dismayed distressed shambles wrecked
			rocks dead falling apart disintegrating pieces lousy)
  (BADHEALTH sick wreck cancer ill illness illnesses insane SICKNESS pain
			pains ache aches crazy hurt hurts injury injured smoke smoking
			alcoholic alcoholism TIRED fatigued)
  (SICKNESS sicknesses cold flu headache headaches toothache cancer
			diabetes diabetic diabetics rabies disease)
  (GOODHEALTH healthy well better health)
  (HEALTH-THEME GOODHEALTH BADHEALTH)
  (BADQUALITY stupid mean nasty silly idiotic vicious disgusting dorky
			boring ugly smelly weird dumb old poor DIFFICULT lazy yucky DIFFICULT evil
			wicked malicious dense insane crazy nuts phoney terrible awful dreadful
			miserable hellish stink stinks suck sucks gross greasy)
  (DIFFICULT tough impossible hard challenging demanding stressful)
  (MUCH too many)
  (HATE hates hated)
  (DISLIKE dislikes)
  (BADATTITUDE HATE DISLIKE afraid despise)
  (YELL yelling yelled yells scream screaming screamed screams)
  (BOTHER bothers bothering bug bugs bugging annoys harrass harrassing
			harasses nag nags nagging pester pesters pestering)
  (FIGHT fights fighting fought argue arguing argues quarrel quarreling quarreled)
  (BADREL BADATTITUDE BOTHER HATE YELL FIGHT)
  (KILL killed)
  (MURDER murdered)
  (BEAT beats beating)
  (SHOOT shot)
  (STAB stabbed)
  (BATTER battered batters)

  (CHANCE chances)
  (ODDS CHANCE prospects likelihood probability possibility possibilities potential)

  (BADOCCUR died disappeared burned crashed broke collapsed)
  (BADEVENT death disappearance accident)

  (INJURE BEAT SHOOT STAB BATTER)
  (VIOLENCE KILL MURDER INJURE)
  (CHEAT cheating cheats cheated)
  (LIE lying lied)
  (FOOL fools fooled fooling)
  (DECEIVE deceived deceiving CHEAT LIE FOOL)
  (ERR goof goofed blew blown mistake mistakes blunder blunders)
  (STUPIDTHING idiot moron jerk FOOL ass weirdo pervert dork dummy
			dum dummie dum-dum nerd bozo yoyo peabrain dingbat ding-dong
			ding-ding freak slob goober numbskull dimwit nitwit loser
			sleazebag cretin airhead turkey twit dweeb)
  (SCARYTHING killer monster exam old death SICKNESS age snakes)
  (BADTHING STUPIDTHING SCARYTHING CRIME)
  (CRIME VIOLENCE rob robbed robbery mugging theft thief burglar
			burglary steal stole rape raped extortion pimp extorting
			extorted extorts rip-off mafia mob mobster)
  (LIQUOR booze beer drink drinks drinking drank drunk alcohol alcoholic)
  (DRUGS drug)
  (DRUGS-ADDICTIVE LIQUOR addict junkie hooked addicted heroin meth
			cocain cocaine mainline dope ecstasy crack opiates opioids)
  (DRUGS-RECREATIONAL DRUGS-ADDICTIVE marijuana weed lsd shrooms)
  (BADPROP BADSTATE BADQUALITY STUPIDTHING)
  (BADPRED BADPROP BADREL BADEVENT VIOLENCE DECEIVE BADTHING)
  (GOODSTATE happy well fine pleased delighted cheerful glad satisfied contented joy joyful)
  (GOODQUALITY smart clever bright nice good friendly pretty cool sexy
			handsome good-looking kind intelligent HELPFUL lovely enjoyable
			delightful entertaining FUN funny wonderful normal sane beautiful
			sharp perfect delicious knowledgeable skilled)
  (LOVE loves)
  (LIKE likes)
  (ADMIRE admires)
  (UNDERSTAND understands)
  (LISTEN listens hear hears)
  (RESPECT respects)
  (APPRECIATE appreciates)
  (CARE cares)
  (ENJOY enjoys enjoying enjoyed)
  (GOODATTITUDE LOVE LIKE ADMIRE UNDERSTAND RESPECT stand APPRECIATE CARE ENJOY)
  (HELP helps helped assist assists assisted)
  (RELAXATION relax relaxes relaxing relaxed calm calms calming sooth
			soothes soothingtherapy therapeutic)
  (VERY quite extremely exceedingly exceptionally extraordinary tremendously
			immensely hugely uncommonly particularly highly remarkably truly really)
  (HELPFUL helping HELP obliging accommodating sympathetic useful)
  (TIRED sleepy exhausted weary fatigued)
  (SUPPORT supports supported)
  (QUICK efficient fast)
  (GOODTHING genius beauty sex FUN pleasure)
  (GOODACTION HELP SUPPORT)
  (GOODPROP GOODSTATE GOODQUALITY)
  (GOODREL GOODATTITUDE GOODACTION)
  (GOODPRED GOODPROP GOODREL GOODTHING)
  (INFORM informs informed tell tells told telling assure assures assures assuring)
  (ASSERT asserts asserted asserting say says said saying claim claims claimed states stated stating)
  (COMMUN INFORM ASSERT)
  (WANT need needs wish require requires)
  (V AUX CONC BADREL VIOLENCE GOODREL COMMUN WANT)
))


(SETQ *TRACERULES* NIL)