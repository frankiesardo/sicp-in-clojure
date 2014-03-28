(ns sicp.ch2)

;; Exercise 2.1.
;; Define a better version of make-rat that handles both positive and negative arguments.
;; Make-rat should normalize the sign so that if the rational number is positive, both the numerator and denominator are positive, and if the rational number is negative, only the numerator is negative.

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defn make-rat [n d]
  [n d])

(defn numer [r] (first r))

(defn denom [r] (second r))

(defn print-rat [r]
  (println-str (numer r) "/" (denom r)))

(def one-half (make-rat 1 2))

(print-rat one-half)

(def one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(defn gcd [a b]
  (if (= b 0)
      a
      (gcd b (rem a b))))

(defn make-rat [n d]
  (let [g (gcd n d)]
    [(/ n g) (/ d g)]))

(print-rat (add-rat one-third one-third))

(defn make-rat-better [n d]
  (if (< d 0) (make-rat (* -1 n) (* -1 d))
    (make-rat n d)))

(make-rat-better 6 -7)

;; Exercise 2.2

(defn make-segment [a b] [a b])

(defn start-segment [s] (first s))

(defn end-segment [s] (second s))

(defn make-point [x y] [x y])

(defn x-point [p] (first p))

(defn y-point [p] (second p))

(defn print-point [p] (println-str "(" (x-point p) "," (y-point p) ")"))

(defn midpoint-segment [s]
  (make-point
   (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2)
   (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)
   ))

(def segment (make-segment (make-point -3 4) (make-point 1 2)))

(print-point (midpoint-segment segment))


;; Exercise 2.3

(defn make-rect [left-right-diagonal] left-right-diagonal)

(defn rec-width [r] (Math/abs (- (x-point (start-segment r)) (x-point (end-segment r)))))

(defn rec-height [r] (Math/abs (- (y-point (end-segment r)) (y-point (start-segment r)))))

(defn rec-perimeter [r] (+ (* 2 (rec-height r)) (* 2 (rec-width r))))

(defn rec-area [r] (* (rec-height r) (rec-width r)))

(rec-perimeter (make-rect segment))

(rec-area (make-rect segment))

(defn make-rect [width height top-left-point] [width height top-left-point])

(defn rec-width [r] (first r))

(defn rec-height [r] (second r))

(rec-perimeter (make-rect 4 2 :top-left-point))

(rec-area (make-rect 4 2 :top-left-point))

;; Exercise 2.4

(defn cons' [x y]
  (fn [m] (m x y)))

(defn car' [z]
  (z (fn [p q] p)))

(defn cdr' [z]
  (z (fn [p q] q)))

(def pair (cons' :a :b))

(car' pair)

(cdr' pair)

;; Exercise 2.5

(defn cons' [a b] (* (Math/pow 2 a) (Math/pow 3 b)))

(defn car' [x] (if (zero? (rem x 2)) (+ 1 (car' (/ x 2))) 0))

(defn cdr' [x] (if (zero? (rem x 3)) (+ 1 (cdr' (/ x 3))) 0))

(car' (cons' 3 4))

(cdr' (cons' 3 4))

;; Exercise 2.6

(def zero (fn [f] (fn [x] x)))

((zero inc) 0)

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(def one (fn [f] (fn [x] (f x))))

((one inc) 0)

(def two
  (fn [f]
    (fn [x] (f (f x)))))

(defn plus [a b]
  (fn [f]
    (fn [x]
      ((a f)((b f) x)))))

(def three (plus two one))

((three inc) 0)

(defn mult [a b]
  (fn [f]
    (fn [x]
      ((a (b f)) x))))

(def six (mult three two))

((six inc) 0)

(defn exp [a b]
  (fn [f]
    (fn [x]
      (((b a) f) x))))

(def sixty-four (exp two six))

((sixty-four inc) 0)

;; Exercise 2.7

(defn make-interval [a b] [a b])

(defn lower-bound [i] (first i))

(defn upper-bound [i] (second i))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.8

(defn sub-interval [x y]
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9

(defn width-interval [i] (- (upper-bound i) (lower-bound i)))

(def interval1 (make-interval 9.5 12))

(def interval2 (make-interval 3.5 7.5))

(def interval3 (make-interval 0.5 4.5))

(= (width-interval (add-interval interval1 interval2)) (width-interval (add-interval interval1 interval3)))

(= (width-interval (sub-interval interval1 interval2)) (width-interval (sub-interval interval1 interval3)))

(not= (width-interval (mul-interval interval1 interval2)) (width-interval (mul-interval interval1 interval3)))

(not= (width-interval (div-interval interval1 interval2)) (width-interval (div-interval interval1 interval3)))

;; Exercise 2.10

(defn includes-zero? [interval]
  (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0)))

(defn div-interval [x y]
  (if (includes-zero? y)
    (throw (ArithmeticException. "Divide by zero"))
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;; Exercise 2.11

(defn mul-interval [x y]
  (let [xlo (lower-bound x)
        xup (upper-bound x)
        ylo (lower-bound y)
        yup (upper-bound y)]
    (cond (and (>= xlo 0) (>= xup 0) (>= ylo 0) (>= yup 0)) ; [+, +] * [+, +]
            (make-interval (* xlo ylo) (* xup yup))

          (and (>= xlo 0) (>= xup 0) (<= ylo 0) (>= yup 0)) ; [+, +] * [-, +]
            (make-interval (* xup ylo) (* xup yup))

          (and (>= xlo 0) (>= xup 0) (<= ylo 0) (<= yup 0)) ; [+, +] * [-, -]
            (make-interval (* xup ylo) (* xlo yup))

          (and (<= xlo 0) (>= xup 0) (>= ylo 0) (>= yup 0)) ; [-, +] * [+, +]
            (make-interval (* xlo yup) (* xup yup))

          (and (<= xlo 0) (>= xup 0) (<= ylo 0) (>= yup 0)) ; [-, +] * [-, +]
            (make-interval (min (* xup ylo) (* xlo yup))
                         (max (* xlo ylo) (* xup yup)))

          (and (<= xlo 0) (>= xup 0) (<= ylo 0) (<= yup 0)) ; [-, +] * [-, -]
            (make-interval (* xup ylo) (* xlo ylo))

          (and (<= xlo 0) (<= xup 0) (>= ylo 0) (>= yup 0)) ; [-, -] * [+, +]
            (make-interval (* xlo yup) (* xup ylo))

          (and (<= xlo 0) (<= xup 0) (<= ylo 0) (>= yup 0)) ; [-, -] * [-, +]
            (make-interval (* xlo yup) (* xlo ylo))

          (and (<= xlo 0) (<= xup 0) (<= ylo 0) (<= yup 0)) ; [-, -] * [-, -]
            (make-interval (* xup yup) (* xlo ylo)))))

(mul-interval interval1 interval2)

;; Exercise 2.12

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-percent [c p]
  (let [w (* c (/ p 100.0))]
    (make-center-width c w)))

(defn percent [i]
  (* (/ (width i) (center i)) 100.0))

(make-center-width 100 15)

(make-center-percent 100 15)

;; Exercise 2.13

(def small-perc1 (make-center-percent 10 2))
(def small-perc2 (make-center-percent 30 7))
(percent (mul-interval small-perc1 small-perc2)) ;; ~ 9

;; Exercise 2.14

(defn par1 [r1 r2]
   (div-interval (mul-interval r1 r2)
                 (add-interval r1 r2)))

(defn par2 [r1 r2]
   (let [one (make-interval 1 1)]
      (div-interval one
                    (add-interval (div-interval one r1)
                                  (div-interval one r2)))))

(par1 interval1 interval2)
(par2 interval1 interval2)

(def should-be-one (div-interval interval1 interval1))
(center should-be-one)
(percent interval1)

;; Exercise 2.15

;> Because diving an interval by itself does not yield one, so repeating the same iterval in the formula introduces errors when you simplify it.

;; Exercise 2.16

;> http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem

;; Exercise 2.17

(defn last-pair [l]
  (if (empty? (rest l)) (first l) (last-pair (rest l))))

(last-pair [1 2 3 4])

;; Exercise 2.18

(defn append [l1 l2]
  (if (empty? l1) [l2] (cons (first l1) (append (rest l1) l2))))

(append [1 2] [3 4])

(defn reverse' [l]
  (if (empty? l) l (append (reverse' (rest l)) (first l))))

(reverse' [1 2 3 4 5])

;; Exercise 2.19

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1))

(defn no-more? [coin-values] (empty? coin-values))

(defn first-denomination [coin-values] (first coin-values))

(defn except-first-denomination [coin-values] (rest coin-values))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                coin-values))))

(cc 100 us-coins)
(cc 100 uk-coins)

(cc 100 (reverse uk-coins))
(cc 100 (reverse us-coins))

; Order does not matter because the execution is a decision tree

;; Exercise 2.20

(defn same-parity [pivot & more]
  (cons pivot (same-parity' pivot more)))

(defn same-parity' [pivot candidates]
  (if-let [candidate (first candidates)]
    (if (= (rem pivot 2) (rem candidate 2))
      (cons candidate (same-parity' pivot (rest candidates)))
      (same-parity' pivot (rest candidates)))))

(same-parity 1)

(same-parity 1 2 3 4 5 6 7 8 9 )

;; Exercise 2.21

(defn square-list [items]
  (if (empty? items)
      []
      (cons (* (first items) (first items))  (square-list (rest items)))))

(square-list [1 23 4 5])

(defn square-list [items]
  (map #(* % %) items))

(square-list [1 23 4 5])

;; Exercise 2.22

(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
        answer
        (iter (rest things)
              (cons (#(* % %) (first things))
                    answer))))
  (iter items []))

(square-list [1 2 3 4])

; cons insert element to first position

(defn square-list [items]
  (defn iter [things answer]
    (if (empty? things)
        answer
        (iter (rest things)
              (cons answer
                    (#(* % %) (first things))))))
  (iter items nil))

(square-list [1 2 3 4])

; cons takes an element and a list. In that example we should use append

;; Exercise 2.23

(defn for-each [f items]
  (let [head (first items)]
    (when head (f head) (for-each f (rest items)))))

(for-each println [1 2 3])

;; Exercise 2.24

(list 1 (list 2 (list 3 4)))

;; Exercise 2.25

(first (rest (first (rest (rest '(1 3 (5 7) 9))))))

(first (first '((7))))

(first (rest (first (rest (first (rest (first (rest (first (rest (first (rest '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;; Exercise 2.26

(def x (list 1 2 3))
(def y (list 4 5 6))

(append x y) ;> (1 2 3 4 5 6)

(cons x y) ;> ((1 2 3) 4 5 6)

(list x y) ;> ((1 2 3) (4 5 6))

;; Exercise 2.27

(defn deep-reverse [l]
  (when-let [head (first l)]
    (append (deep-reverse (rest l)) (if (coll? head) (deep-reverse head) head))))

(deep-reverse [1 2 3 [1 2 3 [4 5 6]]])

;; Exercise 2.28

(defn fringe [tree]
  (cond (nil? tree) []
        (not (coll? tree)) [tree]
        :else (concat (fringe (first tree))
                      (fringe (next tree)))))

(fringe [1 2 3 [1 2 3 [4 5 6]]])

;; Exercise 2.29

(defn make-mobile [left right] [left right])
(defn make-branch [length structure] [length structure])
(defn left-branch [mobile] (mobile 0))
(defn right-branch [mobile] (mobile 1))
(defn branch-length [branch] (branch 0))
(defn branch-structure [branch] (branch 1))

(defn branch-weight [branch]
  (let [structure (branch-structure branch)]
    (if (number? structure) structure (total-weight structure))))

(defn total-weight [mobile] (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(def unbalanced-mobile
  (make-mobile
   (make-branch 1
                (make-mobile
                 (make-branch 2 3)
                 (make-branch 4 5)
                 )
                )
   (make-branch 3
                (make-mobile
                 (make-branch 4 5)
                 (make-branch 6 7)
                 )
                )
   )
  )

(total-weight unbalanced-mobile)

(defn balanced-branch? [branch]
  (let [structure (branch-structure branch)]
    (if (number? structure) true (balanced? structure))))

(defn balanced? [mobile]
  (let [lb (left-branch mobile) rb (right-branch mobile)]
    (and
     (balanced-branch? lb)
     (balanced-branch? rb)
     (= (* (branch-length lb) (branch-weight lb)) (* (branch-length rb) (branch-weight rb))))))


(balanced? unbalanced-mobile)

(def balanced-mobile
  (make-mobile
   (make-branch 2
                (make-mobile
                 (make-branch 6 7)
                 (make-branch 14 3)
                 )
                )
   (make-branch 5 4)
   )
  )

(balanced? balanced-mobile)

;; Exercise 2.30

(defn square-tree [tree]
  (cond (not (coll? tree)) (* tree tree)
        (empty? tree) nil
        :else (cons (square-tree (first tree))
                    (square-tree (rest tree)))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(defn square-tree [tree]
  (map (fn [sub-tree]
         (if (coll? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; Exercise 2.31

(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (coll? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(tree-map #(* % %) (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; Exercise 2.32

(defn subsets [s]
  (if (nil? s) [[]]
    (let [rest (subsets (next s))]
      (concat rest (map #(cons (first s) %) rest)))))

;; Exercise 2.33

(defn accumulate [op initial sequence]
  (if (empty? sequence)
      initial
      (op (first sequence)
          (accumulate op initial (rest sequence)))))

(defn map' [p sequence]
  (accumulate (fn [x y] (cons (p x) y)) [] sequence))

(map' #(* % 2) [1 2 3])

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(append [1 2 3 4] [5 6 7])

(defn length [sequence]
  (accumulate (fn [x y] (+ y 1)) 0 sequence))

(length [:one :two :three :four])

;; Exercise 2.34

(defn horner-eval [x coefficient-sequence]
  (accumulate (fn [this-coeff higher-terms] (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))


(horner-eval 2 (list 1 3 0 5 0 1))

;; Exercise 2.35

(defn count-leaves [t]
  (accumulate + 0 (map (fn [x] 1) (fringe t))))

(count-leaves [1 [2 [5 [6]]] 3 [ 1 2 [ 3 4]]])

;; Exercise 2.36

(defn accumulate-n [op init seqs]
  (if (empty? (first seqs)) nil
      (cons (accumulate op init (map first seqs))
            (accumulate-n op init (map rest seqs)))))

(accumulate-n + 0 [[1 2 3] [4 5 6] [7 8 9]])

;; Exercise 2.37

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product v %) m))

(defn transpose [mat]
  (accumulate-n cons [] mat))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))


(def v (list 1 3 -5))
(def w (list 4 -2 -1))

(dot-product v w) ;> 3

(def m (list (list 1 2 3) (list 4 5 6)))

(matrix-*-vector m v) ;> [-8 -11]

(def n (list (list 14 9 3) (list 2 11 15)))

(matrix-*-matrix m n)

;; Exercise 2.38

(defn fold-right [op initial sequence]
  (if (nil? sequence)
      initial
      (op (first sequence)
          (fold-right op initial (next sequence)))))

(defn fold-left [op initial sequence]
  (defn iter [result rest]
    (if (nil? rest)
        result
        (iter (op result (first rest))
              (next rest))))
  (iter initial sequence))


(fold-right / 1 (list 1 2 3)) (/ 1 (/ 2 (/ 3 1)))

(fold-left / 1 (list 1 2 3)) (/ (/ (/ 1 1) 2) 3)

(fold-right list nil (list 1 2 3))

(fold-left list nil (list 1 2 3))

(fold-right + 0 (list 1 2 3))

(fold-left + 0 (list 1 2 3))

;; Exerise 2.39

(defn reverse [sequence]
  (fold-right (fn [x y] (append y [x])) [] sequence))

(reverse [1 2 3])

(defn reverse [sequence]
  (fold-left (fn [x y] (cons y x)) [] sequence))

(reverse [1 2 3])

;; Exercise 2.40

(defn unique-pairs [n]
  (mapcat
   (fn [i] (map
            (fn [j] [i j])
            (range 1 i)))
   (range 1 (inc n))))

(unique-pairs 4)


(defn prime? [n]
  (-> n (bigint) (.toBigInteger) (.isProbablePrime 100)))

(defn prime-sum? [[x y]]
  (prime? (+ x y)))

(defn make-pair-sum [[x y]]
  [x y (+ x y)])


(defn prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)

;; Exercise 2.41

(defn unique-triplets [n]
  (mapcat
   (fn [i] (mapcat
            (fn [j] (map
                     (fn [k] [i j k])
                     (range 1 j)))
            (range 1 i)))
   (range 1 (inc n))))

(defn make-triplet-sum [[x y z]]
  [x y z (+ x y z)])

(defn sum-to [[x y z] n]
  (= n (+ x y z)))

(defn sum-triplets [n]
  (map make-triplet-sum (filter #(sum-to % n) (unique-triplets n))))

(sum-triplets 15)

;; Exercise 2.42

(def empty-board [])

(defn adjoin-position [new-row col rest-of-queens]
  (cons new-row rest-of-queens))

(defn safe? [k positions]
   (def candidate (first positions))
   (defn safe-iter [top bot remain]
     (cond (empty? remain) true
           (or (= (first remain) candidate)
                (= (first remain) top)
                (= (first remain) bot)) false
           :else
            (safe-iter (- top 1) (+ bot 1) (rest remain))))
   (safe-iter (- candidate 1) (+ candidate 1) (rest positions)))

(defn queens [board-size]
  (defn queen-cols [k]
    (if (= k 0)
        (list empty-board)
        (filter (fn [positions] (safe? k positions))
         (mapcat
          (fn [rest-of-queens]
            (map (fn [new-row]
                   (adjoin-position new-row k rest-of-queens))
                 (range 1 (inc board-size))))
          (queen-cols (dec k))))))
  (queen-cols board-size))

(queens 4)

;; Exercise 2.42

(defn queens [board-size]
  (defn queen-cols [k]
    (if (= k 0)
        (list empty-board)
        (filter (fn [positions] (safe? k positions))
         (mapcat
          (fn [new-row]
            (map (fn [rest-of-queens]
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (dec k))))
          (range 1 (inc board-size))))))
  (queen-cols board-size))

;; Exercise 2.43

; (queens 6) -> from linear recursive to tree recursive = T^board-size

;; Exercise 2.44

(defn below [p1 p2] :new-painter)
(defn beside [p1 p2] :new-painter)

(defn up-split [painter n]
  (if (= n 0)
      painter
      (let [smaller (up-split painter (- n 1))]
        (below painter (beside smaller smaller)))))

;; Exercise 2.45

(defn split [split1 split2]
  (fn [painter n]
    (if (= n 0)
      painter
      (let [smaller (split painter (dec n))]
        (split1 painter (split2 smaller smaller))))))

(def right-split (split beside below))
(def up-split (split below beside))

;; Exercise 2.46

(defn make-vec [x y])
(defn xcor-vect [v] (v 0))
(defn ycor-vect [v] (v 1))

(defn add-vec [v1 v2] (make-vec (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(defn sub-vec [v1 v2] (make-vec (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(defn scale-vec [v s] (make-vec (* s (xcor-vect v)) (* (ycor-vect v))))

;; Exercise 2.47

(defn make-frame [origin edge1 edge2]
  [origin edge1 edge2])

(defn origin-frame [f]
  (f 0))

(defn edge1-frame [f]
  (f 1))

(defn edge2-frame [f]
  (f 2))

(defn make-frame [origin edge1 edge2]
  (cons origin [edge1 edge2]))

(defn origin-frame [f]
  (f 0))

(defn edge1-frame [f]
  ((f 0) 0))

(defn edge2-frame [f]
  ((f 0) 1))

;; Exercise 2.48

(defn make-segment [v1 v2] [v1 v2])
(defn start-segment [s] (s 0))
(defn end-segment [s] (S 1))

;; Exercise 2.49

; The painter that draws the outline of the designated frame.
(def outline-segments
 (list
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.0 0.99))
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.99 0.0))
  (make-segment
   (make-vect 0.99 0.0)
   (make-vect 0.99 0.99))
  (make-segment
   (make-vect 0.0 0.99)
   (make-vect 0.99 0.99))))

; The painter that draws an 'X' by connecting opposite corners of the frame.
(def x-segments
 (list
  (make-segment
   (make-vect 0.0 0.0)
   (make-vect 0.99 0.99))
  (make-segment
   (make-vect 0.0 0.99)
   (make-vect 0.99 0.0))))

; The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(def diamond-segments
 (list
  (make-segment
   (make-vect 0.0 0.5)
   (make-vect 0.5 0.0))
  (make-segment
   (make-vect 0.0 0.5)
   (make-vect 0.5 0.99))
  (make-segment
   (make-vect 0.5 0.99)
   (make-vect 0.99 0.5))
  (make-segment
   (make-vect 0.99 0.5)
   (make-vect 0.5 0.0))))

; The wave painter.
(def wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))


;; Exercise 2.50

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame) new-origin (m origin)]
      (painter (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin))))))


(defn flip-horizontal [painter]
   ((transform-painter (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
    painter))

(defn rotate-180 [painter]
   ((transform-painter (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0))
    painter))

(defn rotate-270 [painter]
   ((transform-painter (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
    painter))

;; Exercise 2.51

(defn beside [painter1 painter2]
   (let [split-point (make-vect 0.5 0.0)
         paint-left  (transform-painter painter1
                            (make-vect 0.0 0.0)
                            split-point
                            (make-vect 0.0 1.0))
         paint-right (transform-painter painter2
                            split-point
                           (make-vect 1.0 0.0)
                           (make-vect 0.5 1.0))
         ]
     (fn [frame]
       (paint-left frame)
       (paint-right frame))))

(defn below [painter1 painter2]
   (let [split-point (make-vect 0.0 0.5)
         paint-bottom ((transform-painter
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 0.0)
                        split-point) painter1)
         paint-top ((transform-painter
                     split-point
                     (make-vect 1.0 0.5)
                     (make-vect 0.0 1.0)) painter2)
         ]
     (fn (frame)
     (paint-bottom frame)
     (paint-top frame))))

(defn rotate-90 [painter]
 ((transform-painter (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0))
   painter))

(defn below [painter1 painter2]
   (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))

;; Exercise 2.52

(def wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment ; left face
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))
  (make-segment ; eye
   (make-vect 0.395 0.916)
   (make-vect 0.410 0.916))
  (make-segment ; smile
   (make-vect 0.376 0.746)
   (make-vect 0.460 0.790))))

(defn corner-split [painter n]
 (if (= n 0)
     painter
     (let [up (up-split painter (- n 1))
           right (right-split painter (- n 1))
           corner (corner-split painter (- n 1))]
         (beside (below painter up)
                 (below right corner)))))

(defn square-limit [painter n]
 (let [quarter (rotate180 (corner-split painter n))
       half (beside (flip-horiz quarter) quarter)]
     (below (flip-vert half) half)))

;; Exercise 2.53

(defn memq [item x]
  (cond (empty? x) false
        (= item (first x)) x
        :else (memq item (rest x))))

(list 'a 'b 'c)

(list (list 'george))

(rest '((x1 x2) (y1 y2)))

(first '((x1 x2) (y1 y2)))

(coll? (first '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))


;; Exercise 2.54

(defn equal? [a b]
  (cond
   (and (symbol? a) (symbol? b) (= a b)) true
   (and (coll? a) (coll? b) (= (first a) (first b))) (equal? (rest a) (rest b))
   :else false))

;; Exerise 2.55

(first ''abracadabra)

; -> ''abracadabra yields (quote abracadabra)


;; Exercise 2.55

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2]
  (cond (= a1 0) a2
        (= a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (= m1 0) (= m2 0)) 0
        (= m1 1) m2
        (= m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn sum? [x]
  (and (coll? x) (= (first x) '+)))

(defn addend [s] (second s))

(defn augend [s] (second (rest s)))

(defn product? [x]
  (and (coll? x) (= (first x) '*)))

(defn multiplier [p] (second p))

(defn multiplicand [p] (second (rest p)))


(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))
        (product? exp) (make-sum
                         (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                             (multiplicand exp)))
        :else (throw (Exception. "unknown expression type -- DERIV" exp))))

