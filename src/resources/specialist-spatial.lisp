;; Dec 9/19
;; ================================================
;;
;; Functions to answer spatial questions in a "rough and ready" way
;; NOTE: We currently assume, for convenience, that each block is of unit size,
;; so that the centroid coordinates of two blocks which are touching will differ by 1
;;

(in-package :eta)

; List of supported spatial prepositions (potentially aliased in the case of prepositions
; with multiple senses or ones that are essentially synonyms)
; TODO: on_to in facing abutting visible flush_with towards)
(defvar *spatial-prep-list*
  '(touching.p to_the_left_of.p to_the_right_of.p below.p (under.p below.p) (beneath.p below.p) above.p behind.p
    in_front_of.p on.p (on_top_of.p on.p) near.p (near_to.p near.p) (close_to.p near.p) next_to.p (adjacent_to.p next_to.p)
    between.p))
(defun ternary-spatial-prep-p (x) (member x '(between.p)))

; List of supported spatial adjectives
; TODO few.a, fewest.a, visible.a, nearby.a, facing.a
(defvar *spatial-adj-list*
  '(left.a (leftmost.a (most-n left.a))
    right.a (rightmost.a (most-n right.a))
    far.a (furthest.a (most-n far.a)) (farthest.a (most-n far.a)) (back.a far.a)
          (backmost.a (most-n far.a))
    near.a (nearest.a (most-n near.a)) (close.a near.a) (closest.a (most-n near.a))
           (front.a near.a) (frontmost.a (most-n near.a))
    high.a (highest.a (most-n high.a)) (topmost.a (most-n high.a)) (top.a high.a)
           (uppermost.a (most-n high.a))
    low.a (lowest.a (most-n low.a))
    center.a (centermost.a (most-n center.a)) (central.a center.a)
    small.a (smallest.a (most-n small.a))
    large.a (largest.a (most-n large.a))
    short.a (shortest.a (most-n short.a))
    tall.a (tallest.a (most-n tall.a))
    long.a (longest.a (most-n long.a))
    adjacent.a (flush.a adjacent.a)
    clear.a
    purple.a blue.a green.a yellow.a orange.a red.a pink.a gray.a (grey.a gray.a) black.a white.a brown.a))
(defvar *color-adj-list*
  '(red.a orange.a yellow.a green.a blue.a indigo.a violet.a purple.a pink.a black.a gray.a grey.a white.a))
    
; Block sizes and expected standard error of coordinate measurements
; TODO: needs to be generalized
(defvar *blocksize* 1)
(defvar *std-error* (/ *blocksize* 10))

; Plural noun without any sort of modifier resolves to 3 by default
(defvar *spatial-plur-value* 3)

; Threshold for distance from center blocks can be to be considered "central"
(defvar *central-threshold* 3)

(defun block-name? (ulf)
  (member ulf '(|Adidas| |Burger King| |Esso| |Heineken| |HP | |HP| |McDonalds| |McDonald's| |Mercedes|
                |NVidia| |Pepsi| |SRI | |SRI| |Starbucks| |Texaco| |Target| |Toyota| |Shell| |Twitter|)))

(defun spatial-deg-adv? (ulf)
  (member ulf '(just.adv-a very.adv-a only.adv-a exactly.adv-a precisely.adv-a immediately.adv-a right.adv-a slightly.adv-a
    directly.adv-a flush.adv-a up.adv-a)))


(defun ~= (x y)
; ``````````````````````
; Approximately equal to
;
  (let ((eps *std-error*))
    (< (abs (- x y)) eps))
) ; END ~=


(defun ~equal (l1 l2)
; `````````````````````
; List approximately equal to
;
  (cond
    ((and (numberp l1) (numberp l2))
      (~= l1 l2))
    ((and (symbolp l1) (symbolp l2))
      (equal l1 l2))
    ((and (listp l1) (listp l2))
      (let* ((padded (pad l1 l2)) (l1p (first padded)) (l2p (second padded)))
        (every (lambda (x y) (~equal x y)) l1p l2p))))
) ; END ~equal


(defun sigmoid (x &key (a 0.1))
; ```````````````````````````````
; Sigmoid function with coefficient a
;
  (/ 1 (+ 1 (exp (- (* a x)))))
) ; END sigmoid


(defun exp-decay (x &key (a 0.1))
; `````````````````````````````````
; Exponential decay with a coefficient a
;
  (exp (- (* a x)))
) ; END exp-decay


(defun same-height (z1 z2)
; `````````````````````````
; Check whether two objects are at the same height
;
  (exp-decay (abs (- z1 z2)) :a 2)
) ; END same-height


(defun dist (x1 y1 z1 x2 y2 z2)
; ``````````````````````````````
; Euclidean distance
;
  (let ((d1 (- x1 x2))
        (d2 (- y1 y2))
        (d3 (- z1 z2)))
      (sqrt (+ (+ (* d1 d1) (* d2 d2)) (* d3 d3))))
) ; END dist


(defun scaled-dist (x1 y1 z1 x2 y2 z2 size1 size2)
; ``````````````````````````````````````````````````
; Distance scaled by the sizes of the two objects
; 
  (/ (dist x1 y1 z1 x2 y2 z2) (max (+ size1 0.001) (+ size2 0.001)))
) ; END scaled-dist


(defun sort-by-nth (coords-list n)
; `````````````````````````````````
; Sorts a list of coordinates by position n in the list (decreasing order).
;
  (sort (copy-seq coords-list) (lambda (c1 c2) (> (nth n c1) (nth n c2))))
) ; END sort-by-nth


(defun get-most-nth (coords-list n &optional neg)
; `````````````````````````````````````````````````
; Gets the coordinate(s) with the highest value of position n in the list (or lowest if neg is given).
;
  (let ((sorted (sort-by-nth coords-list n)))
    (when neg (setq sorted (reverse sorted)))
    (remove-if-not (lambda (c) (~= (nth n c) (nth n (car sorted)))) sorted))
) ; END get-most-nth


(defun get-pos-nth (coords-list n &optional neg)
; ````````````````````````````````````````````````
; Gets the coordinate(s) with a positive value of position n in the list (or negative if neg is given).
;
  (remove-if-not (lambda (c) (if neg (< (nth n c) 0) (> (nth n c) 0))) coords-list)
) ; END get-pos-nth


(defun sort-by-central (coords-list)
; ````````````````````````````````````
; Sorts a list of coordinates from x,y distance from origin.
;
  (sort (copy-seq coords-list) (lambda (c1 c2) (<
    (apply 'dist (append (cdr c1) (list 0 0 0)))
    (apply 'dist (append (cdr c2) (list 0 0 0))))))
) ; END sort-by-central


(defun get-most-central (coords-list &optional neg)
; `````````````````````````````````````````````````
; Gets the coordinate(s) with the highest value of position n in the list (or lowest if neg is given).
;
  (let ((sorted (sort-by-central coords-list)))
    (when neg (setq sorted (reverse sorted)))
    (remove-if-not (lambda (c) (~=
        (apply 'dist (append (cdr c) (list 0 0 0)))
        (apply 'dist (append (cdr (car sorted)) (list 0 0 0)))))
      sorted))
) ; END get-most-central


(defun get-central (coords-list)
; ````````````````````````````````
; Gets the coordinate(s) which fall within some threshold for being "central".
;
  (remove-if-not (lambda (c)
      (<= (apply 'dist (append (cdr c) (list 0 0 0))) *central-threshold*))
    coords-list)
) ; END get-central


(defun filter-adjacent-pairs (coords-list)
; ``````````````````````````````````````````
; Gets the coordinate(s) which are touching some other coordinate in coords-list.
;
  (remove-if-not (lambda (c) (some (lambda (c1)
        (eval-spatial-relation-bool 'touching.p c c1 nil))
      coords-list))
    coords-list)
) ; END filter-adjacent-pairs


(defun filter-clear (coords-list)
; `````````````````````````````````
; Gets the coordinate(s) which have nothing on top of them.
; TODO: currently this only checks the other things in coords-list, but it probably
; should range over all known items, including ones not in coords-list.
;
  (remove-if (lambda (c) (some (lambda (c1)
        (eval-spatial-relation-bool 'on.p c1 c nil))
      coords-list))
    coords-list)
) ; END filter-clear


(defun filter-color (coords-list color)
; ```````````````````````````````````````
; Gets the coordinate(s) of blocks with the given color in coords-list.
; TODO: using *temp-ds* here may be broken; revise this later.
;
  (remove-if-not (lambda (c) (equal color (get-color (first c) *temp-ds*))) coords-list)
) ; END filter-color


(defun filter-names (coords-list names)
; ``````````````````````````````````````
; Gets the coordinate(s) of blocks with a name in names.
;
  (remove-if-not (lambda (c) (member (first c) names)) coords-list)
) ; END filter-names


(defun eval-spatial-relation-bool (rel coords1 coords2 coords3 &optional deg-adv)
; ``````````````````````````````````````````````````````````````````````````````````
; Evaluate whether relation rel holds between an object with centroid at coords1, and
; an object with centroid at coords2 (plus an additional object at coords3 for between.p)
; Returns boolean value.
; If deg-adv is given as true (e.g. "directly"), also check if the blocks are touching.
; NOTE: coords is a list of the form e.g. (|Texaco| 2 3 0)
;
  (>= (eval-spatial-relation rel coords1 coords2 coords3 deg-adv) *certainty-threshold*)
) ; END eval-spatial-relation-bool


(defun eval-spatial-relation (rel coords1 coords2 coords3 &optional deg-adv)
; `````````````````````````````````````````````````````````````````````````````
; Returns a certainty value for the relation rel between coords1 and coords2 (and
; coords3 in the case of between).
; If deg-adv is given as true (e.g. "directly"), also check if the blocks are touching.
; NOTE: coords is a list of the form e.g. (|Texaco| 2 3 0)
;
  (let* ((prep-lookup (find-car rel *spatial-prep-list*))
         (prep (if (atom prep-lookup) prep-lookup (second prep-lookup))))
    (if (fboundp prep)
      (let ((v (apply prep (append (cdr coords1) (cdr coords2) (cdr coords3)))))
        (if (not (numberp v)) (setq v 0))
        (if deg-adv
          (+ (* 0.5 v) (* 0.5 (eval-spatial-relation 'touching.p coords1 coords2 nil)))
          v))))
) ; END eval-spatial-relation


(defun touching.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (if (or
      (and (~= 0 diffx)           (>  *blocksize* diffy) (~= *blocksize* diffz))
      (and (>  *blocksize* diffx) (~= 0 diffy)           (~= *blocksize* diffz))
      (and (~= 0 diffx)           (~= *blocksize* diffy) (>  *blocksize* diffz))
      (and (>  *blocksize* diffx) (~= *blocksize* diffy) (~= 0 diffz))
      (and (~= *blocksize* diffx) (~= 0 diffy)           (>  *blocksize* diffz))
      (and (~= *blocksize* diffx) (>  *blocksize* diffy) (~= 0 diffz)))
    1.0 0.0))
) ; END touching.p


(defun to_the_left_of.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (if (<= diffx (* -1 *blocksize*))
      (exp-decay (dist x1 y1 z1 x2 y2 z2) :a 0.02) 0))
) ; END to_the_left_of.p


(defun to_the_right_of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (- x1 x2)) (diffy (abs (- y1 y2))) (diffz (abs (- z1 z2))))
    (if (>= diffx *blocksize*)
      (exp-decay (dist x1 y1 z1 x2 y2 z2) :a 0.02) 0))
) ; END to_the_right_of.p


(defun below.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (if (<= diffz (* -1 *blocksize*))
      (exp-decay (dist x1 y1 z1 x2 y2 z2) :a 0.02) 0))
) ; END below.p


(defun above.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (abs (- y1 y2))) (diffz (- z1 z2)))
    (if (>= diffz *blocksize*)
      (exp-decay (dist x1 y1 z1 x2 y2 z2) :a 0.02) 0))
) ; END above.p


(defun behind.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (if (>= diffy *blocksize*)
      (exp-decay (dist x1 y1 z1 x2 y2 z2) :a 0.02) 0))
) ; END behind.p


(defun in_front_of.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````
  (let ((diffx (abs (- x1 x2))) (diffy (- y1 y2)) (diffz (abs (- z1 z2))))
    (if (<= diffy (* -1 *blocksize*))
      (exp-decay (dist x1 y1 z1 x2 y2 z2) :a 0.02) 0))
) ; END in_front_of.p


(defun on.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````
  ;; (and (above.p x1 y1 z1 x2 y2 z2) (touching.p x1 y1 z1 x2 y2 z2))
  (+ (* 0.5 (above.p x1 y1 z1 x2 y2 z2)) (* 0.5 (touching.p x1 y1 z1 x2 y2 z2)))
) ; END on.p


;; (defun near.p (x1 y1 z1 x2 y2 z2 size1 size2 context)
(defun near.p (x1 y1 z1 x2 y2 z2)
; `````````````````````````````````````````````````````
  ;; (exp (- (* 0.1 (scaled-dist x1 y1 z1 x2 y2 z2 size1 size2))))
  (exp-decay (scaled-dist x1 y1 z1 x2 y2 z2 *blocksize* *blocksize*))
) ; END near.p


;; (defun next_to.p (x1 y1 z1 x2 y2 z2 size1 size2)
(defun next_to.p (x1 y1 z1 x2 y2 z2)
; ````````````````````````````````````````````````
  ;; (+ (* 0.5 (near.p x1 y1 z1 x2 y2 z2 size1 size2)) (* 0.5 (same-height z1 z2)))
  (+ (* 0.7 (near.p x1 y1 z1 x2 y2 z2)) (* 0.3 (same-height z1 z2)))
) ; END next_to.p


(defun between.p (x1 y1 z1 x2 y2 z2 x3 y3 z3)
; ````````````````````````````````````````````````
  (max
    (+ (* 0.5 (to_the_left_of.p x1 y1 z1 x2 y2 z2)) (* 0.5 (to_the_right_of.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (to_the_right_of.p x1 y1 z1 x2 y2 z2)) (* 0.5 (to_the_left_of.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (in_front_of.p x1 y1 z1 x2 y2 z2)) (* 0.5 (behind.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (behind.p x1 y1 z1 x2 y2 z2)) (* 0.5 (in_front_of.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (above.p x1 y1 z1 x2 y2 z2)) (* 0.5 (below.p x1 y1 z1 x3 y3 z3)))
    (+ (* 0.5 (below.p x1 y1 z1 x2 y2 z2)) (* 0.5 (above.p x1 y1 z1 x3 y3 z3))))
) ; END between.p


(defun eval-spatial-modifier (adj coords-list &optional mod-a)
; ``````````````````````````````````````````````````````````````
; Evaluates a spatial modifier (e.g. left.a), possibly with some modifier (e.g. most-n)
; to return a subset of the block+coordinates in coords-list.
;
  (let* ((mod-lookup (find-car adj *spatial-adj-list*))
         (mod (if (atom mod-lookup) mod-lookup (second mod-lookup))))
    ; Alias contains mod-a
    (when (listp mod)
      (setq mod-a (first mod))
      (setq mod (second mod)))
    (cond
      ((or (null coords-list) (not (listp coords-list))) nil)
      ((and (symbolp mod) (fboundp mod))
        (funcall mod coords-list mod-a))
      (t coords-list)))
) ; END eval-spatial-modifier


(defun left.a (coords-list mod-a)
; `````````````````````````````````
; Select the left blocks, applying any mod-a as appropriate.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      (get-most-nth coords-list 1 nil))
    ; no/unknown mod-a
    (t
      (get-pos-nth coords-list 1 t)))
) ; END left.a


(defun right.a (coords-list mod-a)
; `````````````````````````````````
; Select the right blocks, applying any mod-a as appropriate.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      (get-most-nth coords-list 1 t))
    ; no/unknown mod-a
    (t
      (get-pos-nth coords-list 1 nil)))
) ; END right.a


(defun far.a (coords-list mod-a)
; `````````````````````````````````
; Select the far blocks, applying any mod-a as appropriate.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      (get-most-nth coords-list 2 t))
    ; no/unknown mod-a
    (t
      (get-pos-nth coords-list 2 nil)))
) ; END far.a


(defun near.a (coords-list mod-a)
; `````````````````````````````````
; Select the near blocks, applying any mod-a as appropriate.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      (get-most-nth coords-list 2 nil))
    ; no/unknown mod-a
    (t
      (get-pos-nth coords-list 2 t)))
) ; END near.a


(defun high.a (coords-list mod-a)
; `````````````````````````````````
; Select the high blocks, applying any mod-a as appropriate.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      (get-most-nth coords-list 3 t))
    ; no/unknown mod-a
    (t
      (get-pos-nth coords-list 3 nil)))
) ; END high.a


(defun low.a (coords-list mod-a)
; `````````````````````````````````
; Select the low blocks, applying any mod-a as appropriate.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      (get-most-nth coords-list 3 nil))
    ; no/unknown mod-a
    (t
      (get-pos-nth coords-list 3 t)))
) ; END low.a


(defun center.a (coords-list mod-a)
; `````````````````````````````````
; Select the central blocks, applying any mod-a as appropriate.
; TODO: all blocks are the same size right now, so this doesn't do anything.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      (get-most-central coords-list))
    ; no/unknown mod-a
    (t 
      (get-central coords-list)))
) ; END center.a


(defun small.a (coords-list mod-a)
; `````````````````````````````````
; Select the small blocks, applying any mod-a as appropriate.
; TODO: all blocks are the same size right now, so this doesn't do anything.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      coords-list)
    ; no/unknown mod-a
    (t coords-list))
) ; END small.a


(defun large.a (coords-list mod-a)
; `````````````````````````````````
; Select the large blocks, applying any mod-a as appropriate.
; TODO: all blocks are the same size right now, so this doesn't do anything.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      coords-list)
    ; no/unknown mod-a
    (t coords-list))
) ; END large.a


(defun short.a (coords-list mod-a)
; `````````````````````````````````
; Select the short blocks, applying any mod-a as appropriate.
; TODO: all blocks are the same size right now, so this doesn't do anything.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      coords-list)
    ; no/unknown mod-a
    (t coords-list))
) ; END short.a


(defun tall.a (coords-list mod-a)
; `````````````````````````````````
; Select the tall blocks, applying any mod-a as appropriate.
; TODO: all blocks are the same size right now, so this doesn't do anything.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      coords-list)
    ; no/unknown mod-a
    (t coords-list))
) ; END tall.a


(defun long.a (coords-list mod-a)
; `````````````````````````````````
; Select the long blocks, applying any mod-a as appropriate.
; TODO: all blocks are the same size right now, so this doesn't do anything.
;
  (cond
    ; most-n
    ((ttt:match-expr 'most-n mod-a)
      coords-list)
    ; no/unknown mod-a
    (t coords-list))
) ; END long.a


(defun adjacent.a (coords-list mod-a)
; ````````````````````````````````````
; Select any blocks which are adjacent to some other block in coords-list.
;
  (cond
    ; no/unknown mod-a
    (t (filter-adjacent-pairs coords-list)))
) ; END adjacent.a


(defun clear.a (coords-list mod-a)
; ````````````````````````````````````
; Select any blocks in coords-list which are clear (i.e. have nothing on them).
;
  (cond
    ; no/unknown mod-a
    (t (filter-clear coords-list)))
) ; END clear.a


(defun purple.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'purple.a)
) ; END purple.a


(defun blue.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'blue.a)
) ; END blue.a


(defun green.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'green.a)
) ; END green.a


(defun yellow.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'yellow.a)
) ; END yellow.a


(defun orange.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'orange.a)
) ; END orange.a


(defun red.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'red.a)
) ; END red.a


(defun pink.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'pink.a)
) ; END pink.a


(defun gray.a (coords-list mod-a)
; ```````````````````````````````````
  (union
    (filter-color coords-list 'gray.a)
    (filter-color coords-list 'grey.a))
) ; END gray.a


(defun black.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'black.a)
) ; END black.a


(defun white.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'white.a)
) ; END white.a


(defun brown.a (coords-list mod-a)
; ```````````````````````````````````
  (filter-color coords-list 'brown.a)
) ; END brown.a


