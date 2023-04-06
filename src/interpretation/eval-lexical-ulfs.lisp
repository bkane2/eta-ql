;; Code for deriving ULFs for lexical words whose category is specified   (June 10/19)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :eta)

(defun eval-lexical-ulfs (ulf)
;`````````````````````````````
; 'ulf' is an arbitrary ULF that may contain items of form 
;      (lex-ulf@ <lex-cat> <part number>),
; to be replaced by atomic ULFs.
;
 (cond ((atom ulf) ulf)
       ((eq (car ulf) 'lex-ulf@)
        (eval (list (car ulf) 
                    (list 'quote (cadr ulf)) (list 'quote (caddr ulf)))))
       (t (cons (eval-lexical-ulfs (car ulf)) (eval-lexical-ulfs (cdr ulf))))
 )); end of eval-lexical-ulfs


(defun lex-ulf@ (cat word-sym)
;`````````````````````````````
; Construct a ulf of the appropriate type out of 'word-sym' (a symbol such
; as MERCEDES, ARE, ON-TOP-OF), given the lexical category (such as NAME,
; V, or P); **this version is intended for very limited experimental use;
;
; Make sure 'stem' program is available:
 (if (not (fboundp 'stem)) (load "stem.lisp"))

 (let ((word-str (string word-sym)) stem)
      (case cat
         (name (case word-sym
                  (ADIDAS '|Adidas|)
                  (BURGER_KING '|Burger King|)
                  (ESSO '|Esso|)
                  (HEINEKEN '|Heineken|)
                  (HP '|HP |)
                  (MCDONALDS '|McDonald's|)
                  (MERCEDES '|Mercedes|)
                  (NVIDIA '|NVidia|)
                  (PEPSI '|Pepsi|)
                  (SRI '|SRI |)
                  (STARBUCKS '|Starbucks|)
                  (TEXACO '|Texaco|)
                  (TARGET '|Target|)
                  (TOYOTA '|Toyota|)
                  (SHELL  '|Shell|)
                  (TWITTER '|Twitter|)
                  (t (if (< (length word-str) 4)
                         (intern (format nil "~a " word-sym)); NB: final blank
                         (intern (capitalize (string-downcase word-sym)))))))
         (pro (intern (format nil "~a.PRO" word-sym)))
         (punc word-sym)
         (nn ; known to be singular 
             (intern (format nil "~a.N" word-sym)))
         (nns ; known to be plural
             (setq stem (stem (list 'nns word-sym)));
             (list 'plur (intern (format nil "~a.N" stem))))
         ((n noun) (case word-sym
                     (block 'block.n) (blocks '(plur block.n))
                     (cube 'block.n)  (cubes '(plur block.n))
                     (book 'block.n)  (books '(plur block.n))
                     (blog 'block.n)  (blogs '(plur block.n))
                     (black 'block.n) (blacks '(plur block.n))
                     (glock 'block.n) (glocks '(plur block.n))
                     (bach 'block.n)  (bachs '(plur block.n))
                     (blood 'block.n) (bloods '(plur block.n))
                     (look 'block.n)  (looks '(plur block.n))
                     (glass 'block.n) (box '(plur block.n))
                     (book_shelf '(book.n shelf.n)) (book_shelves '(plur (book.n shelf.n)))
                     (coffee_table '(coffee.n table.n)) (coffee_tables '(plur (coffee.n table.n)))
                     (pine_tree '(pine.n tree.n)) (pine_trees '(plur (pine.n tree.n)))
                     (analog_clock '(analog.n clock.n)) (analog_clocks '(plur (analog.n clock.n)))
                     (tooth_brush '(tooth.n brush.n)) (tooth_brushes '(plur (tooth.n brush.n)))
                     (recycle_bin '(recycle.n bin.n)) (recycle_bins '(plur (recycle.n bin.n)))
                     (ceiling_fan '(ceiling.n fan.n)) (ceiling_fans '(plur (ceiling.n fan.n)))
                     (ceiling_light '(ceiling.n light.n)) (ceiling_lights '(plur (ceiling.n light.n)))
                     (card_box '(card.n box.n)) (card_boxes '(plur (card.n box.n)))
                     (foot_rest '(foot.n rest.n)) (foot_rests '(plur (foot.n rest.n)))
                     (pencil_holder '(pencil.n holder.n)) (pencil_holders '(plur (pencil.n holder.n)))
                     (floor_lamp '(floor.n lamp.n)) (floor_lamps '(plur (floor.n lamp.n)))
                     (rocking_chair 'rocking_chair.n) (rocking_chairs '(plur rocking_chair.n))
                     (t (setq stem (stem (list 'n word-sym))); tries to depluralize
                        (if (eq stem word-sym); assume singular if unchanged
                                              ; (somewhat error-prone)
                            (intern (format nil "~a.N" word-sym))
                            (list 'plur (intern (format nil "~a.N" stem)))))))
         ((p prep) (intern (format nil "~a.P" word-sym)))
         ((ps) (intern (format nil "~a.PS" word-sym)))
         ((d det) (intern (format nil "~a.D" word-sym)))
         ; ** Here we should insert cases where the verb is known to be of
         ;    type (POS) VBZ, VBD, VB, VBG, VBN, VBEN; this is a generic stub:
         ((v verb) (case word-sym
                     (can '(pres can.aux-s)) (could '(past can.aux-s))
                     ((do does) '(pres do.aux-s)) (did '(past do.aux-s))
                     ((should shall) '(pres should.aux-s))
                     ((need needs) '(pres need.aux-v)) (needed '(past need.aux-v))
                     ((is are) '(pres be.v)) (be 'be.v) ((was were) '(past be.v))
                     ((support supports) '(pres support.v)) (supported '(past support.v))
                     ((sit sits) '(pres sit.v)) (sat '(past sit.v))
                     ((touch touches) '(pres touch.v)) (touched '(past touch.v))
                     ((connect connects) '(pres connect.v)) (connected '(past connect.v))
                     ((consist_of consists_of) '(pres consist_of.v)) (consisted_of '(past consist_of.v))
                     ((adjoin adjoins) '(pres adjoin.v)) (adjoined '(past adjoin.v))
                     ((flank flanks) '(pres flank.v)) (flanked '(past flank.v))
                     ((face faces) '(pres face.v)) (faced '(past face.v))
                     ((move moves) '(pres move.v)) (moved '(past move.v))
                     ((put puts) '(pres put.v))
                     ((change changes) '(pres change.v)) (changed '(past change.v))
                     ((pick_up picks_up) '(pres pick_up.v)) (picked_up '(past pick_up.v))
                     ((rotate rotates) '(pres rotate.v)) (rotated '(past rotate.v))
                     ((place places) '(pres place.v)) (placed '(past place.v))
                     (t (read-from-string (format nil "(PRES ~a.V)" word-sym))))) ; by default we assume a verb is present tensed
         ; Note: the way passives are dealt with is currently inadequate. It currently assumes that any passive is past tensed
         ((v-pasv verb-passive) (case word-sym
                     (moved '(past (pasv move.v)))
                     (put '(past (pasv put.v)))
                     (changed '(past (pasv change.v)))
                     (picked_up '(past (pasv pick_up.v)))
                     (rotated '(past (pasv rotate.v)))
                     (placed '(past (pasv place.v)))
                     (t (read-from-string (format nil "(PAST (PASV ~a.V))" word-sym)))))
         ; Untensed passive
         ((v-pasv- verb-passive-) (case word-sym
                     (moved '(pasv move.v))
                     (put '(pasv put.v))
                     (changed '(pasv change.v))
                     (picked_up '(pasv pick_up.v))
                     (rotated '(pasv rotate.v))
                     (placed '(pasv place.v))
                     (t (read-from-string (format nil "(PASV ~a.V)" word-sym)))))
         ; Untensed verb
         ((v- verb-untensed)
            ; This is a bit hacky...
            (if (atom (lex-ulf@ 'v word-sym)) (lex-ulf@ 'v word-sym)
                (if (equal (second (lex-ulf@ 'v word-sym)) (intern (format nil "~a.V" word-sym)))
                    (intern (format nil "~a.V" word-sym))
                    (second (lex-ulf@ 'v word-sym)))))
         (wh-pred (case word-sym
                     (where '(at.p (what.d place.n)))
                     (when '(at.p (what.d time.n)))
                     (t (intern (format nil "~a.ADV" word-sym)))))
         ((a adj) (intern (format nil "~a.A" word-sym)))
         (sup-adj (case word-sym
                    (leftmost 'left.a) (rightmost 'right.a)
                    ((furthest farthest) 'far.a) (nearest 'near.a)
                    (closest 'close.a) (highest 'high.a) (tallest 'tall.a)
                    (nearest 'near.a) (topmost 'top.a) (top 'top.a) (uppermost 'upper.a)
                    (smallest 'small.a) (lowest 'low.a) (largest 'large.a)
                    (centermost 'centered.a) (shortest 'short.a) (backmost 'back.a)
                    (longest 'long.a) (fewest 'few.a) (frontmost 'front.a)
                    (t (intern (format nil "~a.A" (stem-superlative word-sym))))))
         (adv-adj (case word-sym
                    (just 'just.a) (previously 'previous.a)
                    (before 'before.a) (originally 'original.a)
                    (initially 'initial.a) (currently 'current.a)
                    (now 'now.a) (recently 'recent.a) (ever 'ever.a)
                    (since 'since.a) (first 'first.a) (second 'second.a)
                    (third 'third.a) (last 'last.a) (always 'always.a)
                    (never 'never.a) (once 'once.a) (twice 'twice.a) (thrice 'thrice.a)
                    (t (intern (format nil "~a.A" word-sym)))))
         (adv-a (intern (format nil "~a.ADV-A" word-sym)))
         (adv-e (intern (format nil "~a.ADV-E" word-sym)))
         (adv-f (intern (format nil "~a.ADV-F" word-sym)))
         (mod-a (intern (format nil "~a.MOD-A" word-sym)))
         (cc (intern (format nil "~a.CC" word-sym))) )
 )); end of lex-ulf@)
                   

(defun stem-superlative (sup-adj); tested
;```````````````````````````````
; Find stem of superlative adjective; this is a stub, intended as default
; when word-specific stems have not been found in the calling program;
; only a simple check whether the stem probably shouldn't have a doubled
; up final consonant is made.
;
; sup-adj: an upper-case symbolic atom such as 'biggest', "smartest', etc.
; 
 (let ((str (string-upcase (string sup-adj))) n)
      (setq n (length str))
      (setq str (subseq str 0 (- n 3)))
      (if (member (subseq str (- n 5) (- n 3)) 
                   '("BB" "DD" "GG" "NN" "PP" "TT") :test 'string=)
          (setq str (subseq str 0 (- n 4))))
      (intern str)
 )); end of stem-superlative


(defun capitalize (str); tested
;``````````````````````
; Return variant of str with first letter upper case; 
; assumes 1st character of str is alphabetic;
;
 (format nil "~:(~a~)" str))

               

