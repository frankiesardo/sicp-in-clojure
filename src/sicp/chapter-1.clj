(ns sicp.ch1)

;; Exercise 1.3.
;; Define a procedure that takes three numbers as arguments and returns the sum of the of the two larger numbers

(defn square [x] (* x x))

(defn sum-squares [x y]
  (+ (square x) (square y)))

(defn sum-largest-squares [x y z]
  (cond
   (and (< x y) (< x z)) (sum-squares y z)
   (and (< y x) (< y z)) (sum-squares x z)
   (and (< z x) (< z y)) (sum-squares x y)))

;; Exercise 1.5

(defn p [] p)

(defn testForZero [x y]
  (if (= x 0) 0 y))

(testForZero 0 p) ;; Clojure uses normal-order evaluation, thus parameters are lazily evaluated. Schema doesn't.

;; Exercise 1.6

(def tolerance 0.001)

(defn average [x y]
  (/ (+ x y) 2))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) tolerance))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(sqrt 9)

;; --

(defn new-if [predicate then-clause else-clause]
  (cond
   predicate then-clause
   :else else-clause))

(defn new-sqrt-iter [guess x]
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(defn new-sqrt [x]
  (new-sqrt-iter 1.0 x))

(new-sqrt 16) ;; Clojure uses normal-order evaluation, thus parameters are lazily evaluated. Schema doesn't.

;; Exercise 1.7

(defn new-good-enough? [guess x]
  (< (/ (abs (- (square guess) x)) guess) (* guess tolerance)))

(defn new-good-enough2? [guess x]
  (< (abs ( / (- (improve guess x) guess) guess)) tolerance))

(new-good-enough? 0.002236 0.000005)

(new-good-enough2? 3.9 16)

;; Exercise 1.8

(defn cube [x]
  (* x x x))

(defn improve-cube [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn good-enough-cube? [guess x]
  (< (/ (abs (- (cube guess) x)) guess) (* guess tolerance)))

(defn cube-root-iter [guess x]
  (if (good-enough-cube? guess x)
    guess
    (cube-root-iter (improve-cube guess x) x)))

(defn cube-root [x]
  (cube-root-iter 1.0 x))

(cube-root 8)

;; Exercise 1.9

(defn plus-rec [a b]
  (if ( = 0 a)
    b
    (inc (plus-rec (dec a) b) )))

(defn plus-iter [a b]
  (if (= a 0)
    b
    (plus-iter (dec a) (inc b))))

;; Exercise 1.10

(defn A [x y]
  (cond
   (= y 0) 0
   (= x 0) (* 2 y)
   (= y 1) 2
   :else (A (dec x) (A x (dec y)))))

(defn f [n] (A 0 n)) ;; 2 * n

(defn g [n] (A 1 n)) ;; 2 ^ n

(defn h [n] (A 2 n)) ;; 2 ^ h(n - 1)


;; Exercise 1.11

(defn fun-rec [n]
  (if (< n 3) n
    (+ (fun-rec (dec n)) (* 2 (fun-rec (- n 2))) (* 3 (fun-rec (- n 3))))))

(defn fun-iter [n]
  ((fn acc [n n-1 n-2 counter]
     (if (= 0 counter) n-2
       (acc ( + n (* 2 n-1) (* 3 n-2) ) n n-1 (dec counter)))) 2 1 0 n))

;; Exercise 1.12

(defn pascal-element-at [row column]
  (cond (= row column) 1
        (= column 0) 1
        :else (+ (pascal-element-at (dec row) (dec column)) (pascal-element-at (dec row) column))))

(defn pascal-row [n]
  (map #(pascal-element-at n %) (range (inc n))))

;; Exercise 1.13

(def phi (/ (+ 1 (Math/sqrt 5)) 2))

(defn phi-fib [n]
  (/ (Math/pow phi n) (Math/sqrt 5)))

(defn fib [n]
  (fib-iter 1 0 n))

(defn fib-iter [a b counter]
  (if (= 0 counter) b
    (fib-iter (+ a b) a (dec counter))))

(defn phi-aprox-fib? [n]
  (< (abs (- (phi-fib n) (fib n))) 0.5))

(def phi-aprox-fib-for-every-n?
  (->>
   (range 1 11)
   (map phi-aprox-fib?)
   (every? true?)))

phi-aprox-fib-for-every-n?


;; Exercise 1.15

(defn sin [angle]
  (do (println "angle " angle)
    (if-not (> (abs angle) 0.1)
      angle
      (p (sin (/ angle 3.0))))))

(defn p [x]
  (- (* 3 x) (* 4 (cube x))))

(sin 90)

;; Exercise 1.16


(defn exp-iter [b n a]
  (cond (= n 0) a
        (odd? n) (exp-iter b (dec n) (* a b))
        (even? n) (exp-iter (square b) (/ n 2) a)))


;; Exercise 1.17

(defn mult [a b]
  (if (= b 0) 0
    (+ a (mult a (dec b)))))

(defn doubl [x] (* 2 x))

(defn fast-mult-rec [a b]
  (cond
   (= b 0) 0
   (even? b) (doubl (fast-mult-rec a (/ b 2)))
   (odd? b) (+ a (fast-mult-rec a (dec b)))))

;; Exercise 1.18

(defn fast-mult-iter [a b acc]
  (cond
   (= b 0) acc
   (even? b) (fast-mult-iter (doubl a) (/ b 2) acc)
   (odd? b) (fast-mult-iter a (dec b) (+ a acc))))

;; Exercise 1.19

(defn clever-fib [n]
  (clever-fib-iter 1 0 0 1 n))

(defn clever-fib-iter [a b p q counter]
  (cond
   (= counter 0) b
   (even? counter) (clever-fib-iter a
                                    b
                                    (+ (* p p) (* q q))        ; compute p'
                                    (+ (* 2 p q) (* q q))      ; compute q'
                                    (/ counter 2))
   :else (clever-fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (dec counter))))

;; Exercise 1.21.
;; Use the smallest-divisor procedure to find the smallest divisor of each of the following numbers: 199, 1999, 19999.

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn divides? [a b]
  (= (rem b a) 0))

(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999) ; -> 7

;; Exercise 1.22.
;; Most Lisp implementations include a primitive called runtime that returns an integer that specifies the amount of time the system has been running (measured, for example, in microseconds).
;; The following timed-prime-test procedure, when called with an integer n, prints n and checks to see if n is prime.
;; If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.


(defn timed-prime-test [n]
  (println "New test with" n)
  (start-prime-test n (System/currentTimeMillis)))


(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime (- (System/currentTimeMillis) start-time))))

(defn report-prime [elapsed-time]
  (println "Prime found:" elapsed-time "milliseconds"))

(defn prime? [n]
  (slow-prime? n))

(defn slow-prime? [n]
  (= n (smallest-divisor n)))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn check-all-primes [coll]
  (if-not (empty? coll)
    (do
      (timed-prime-test (first coll))
      (check-all-primes (rest coll)))))


(def larger-than-1k
  (->>
   (range 1000 1100)
   (filter odd?)
   ))

(def larger-than-10k
  (->>
   (range 10000 10100)
   (filter odd?)
   ))

(def larger-than-100k
  (->>
   (range 100000 100100)
   (filter odd?)
   ))

(def larger-than-1m
  (->>
   (range 1000000 1000100)
   (filter odd?)
   ))

(check-all-primes larger-than-1m)


;; Exercise 1.23.  The smallest-divisor procedure shown at the start of this section does lots of needless testing: After it checks to see if the number is divisible by 2 there is no point in checking to see if it is divisible by any

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (next-divisor test-divisor))))

(defn next-divisor [divisor]
  (if (= divisor 2) 3 (+ divisor 2)))



(smallest-divisor 1011)

;; Exercise 1.24.

(defn fast-prime? [n times]
  (cond (= times 0) true
        (fermat-test n) (fast-prime? n (- times 1))
        :else false))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-int (- n 1)))))


(defn expmod [base exp m]
  (cond (= exp 0) 1
        (even? exp)
        (rem (square (expmod base (/ exp 2) m))
             m)
        :else
        (rem (* base (expmod base (- exp 1) m))
             m)))

;; Exercise 1.25

(defn expmod [base exp m]
  (rem (exp-iter base exp) m)) ;; Jumps in the territory of big integers


;; Exercise 1.27


(defn expmod [base exp m]
  (cond
   (= exp 0) 1
   (even? exp) (rem (square (expmod base (/ exp 2) m))
                    m)
   :else (rem (* base (expmod base (dec exp) m))
              m)))

(defn fermat-test [n a]
  (= (expmod a n n) a))

(defn fermat-full [n]
  (defn iter [a]
    (cond
     (= a 1) true
     (not (fermat-test n a)) false
     :else (iter (dec a))))
  (iter (dec n)))

(fermat-full 561)

(fermat-full 1105)

(fermat-full 1729)

(fermat-full 2465)

(fermat-full 2821)


;; Exercise 1.28

(defn square-check [x m]
  (if
    (and (not (or (= x 1) (= x (- m 1)))) (= (rem (* x x) m) 1))
    0
    (rem (* x x) m)))

(defn expmod [base exp m]
  (cond
   (= exp 0) 1
   (even? exp) (square-check (expmod base (/ exp 2) m) m)
   :else (rem (* base (expmod base (- exp 1) m))
              m)))

(defn miller-rabin-test [n]
  (defn try-it [a]
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (rand-int (- n 2)))))

(miller-rabin-test 561)

(miller-rabin-test 1105)

(miller-rabin-test 1729)

(miller-rabin-test 2465)

(miller-rabin-test 2821)

(miller-rabin-test 6601)

;; Exercise 1.29

(defn sum [term a nex b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (nex a) nex b))))

(defn integral [f a b dx]
  (defn add-dx [x] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


(defn simpson [f a b n]
  (def h (/ (- b a) n))
  (defn y [k]
    (f (+ a (* k h))))
  (defn term [k]
    (* (cond (odd? k) 4
             (or (= k 0) (= k n)) 1
             (even? k) 2)
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))


(defn cube [x] (* x x x))

(integral cube 0 1 0.01)

(simpson cube 0 1 100.0)

(integral cube 0 1 0.001)

(simpson cube 0 1 1000.0)


;; Exercise 1.30

(defn sum [term a nex b]
  (defn iter [a result]
    (if (> a b) result
      (iter (nex a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31


(defn product [term a nex b]
  (if (> a b)
    1
    (* (term a)
       (product term (nex a) nex b))))

(defn product-iter [term a nex b]
  (defn iter [a result]
    (if (> a b) result
      (iter (nex a) (* result (term a)))))
  (iter a 1))

(defn factorial [x]
  (product-iter identity 1 inc x))

(factorial 10)

(defn pi-term [k]
  (if (even? k)
    (/ (+ 2 k) (+ 3 k))
    (/ (+ 3 k) (+ 2 k))))

(def pi ( * 4.0 (product pi-term 0 inc 1000)))

pi

;; Exercise 1.32

(defn accumulate [combiner null-value term a next b]
  (if (> a b) null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(defn accum-iter [combiner null-value term a next b]
  (defn iter [a result]
    (if (> a b) result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; Exercise 1.33

(defn filtered-accumulate [combiner null-value term a next b valid?]
  (cond
   (> a b) null-value
   (valid? a) (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b valid?))
   :else (filtered-accumulate combiner null-value term (next a) next b valid?)))

(defn sum-squares-prime-numbers [a b]
  (filtered-accumulate + 0 square a inc b prime?))

(defn product-relative-primes-with [n]
  (defn relative-prime? [a]
    (= 1 (gcd a n)))
  (filtered-accumulate * 1 identity 2 inc (dec n) relative-prime?))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

;; Exercise 1.34

(defn f [g]
  (g 2))

(f f) ; evaluates to (2 2)

;; Exercise 1.35

(def tolerance 0.00001)

(defn average [a b] (/ (+ a b) 2))

(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-it [guess]
    (println guess)
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (try-it next))))
  (try-it first-guess))

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

(sqrt 16)

(def golden-ratio (fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0))


;; Exercise 1.36

(def x-to-the-x (fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 2))

(def x-to-the-x-ad (fixed-point (fn [x] (average-damping (Math/log 1000) (Math/log x)))) 2)

;; Exercise 1.37

(defn cont-frac [n d k]
  (defn frac [i]
    (if (< i k)
      (/ (n i) (+ (d i) (frac (+ i 1))))
      (/ (n i) (d i))))
  (frac 1))

(defn cont-frac-iter [n d k]
  (defn frac-iter [i acc]
    (if (zero? i)
      acc
      (frac-iter (dec i) (/ (n i) (+ (d i) acc)))))
  (frac-iter (dec k) (/ (n k) (d k))))

(cont-frac (fn [n] 1.0) (fn [d] 1.0) 11)


;; Exercise 1.38

(def e (+ 2 (cont-frac (fn [n] 1.0) euler-sequence 100)))

(defn euler-sequence [x]
  (if (not (= 0 (rem (+ x 1) 3)))
    1
    (* 2 (/ (+ x 1) 3))))

;; Exercise 1.39

(defn tan-cf [x k] (cont-frac (fn [n] (if (= 1 n) x (* x x))) (fn [d] (- (* 2 d) 1)) k))

(tan-cf 3.0 100)

;; Exercise 1.40

(def dx 0.00001)

(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x)) dx)))

(defn cube [x] (* x x x))

((deriv cube) 5)

(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))


(defn sqrt [x]
  (newtons-method (fn [y] (- (square y) x))
                  1.0))

(sqrt 36)


(defn fixed-point-of-transform [g transform guess]
  (fixed-point (transform g) guess))

(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (/ x y))
                            average-damp
                            1.0))

(sqrt 25)

(defn sqrt [x]
  (fixed-point-of-transform (fn [y] (- (square y) x))
                            newton-transform
                            1.0))

(sqrt 49)


(defn cubic [a b c] (fn [x] (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 2 3 4) 1) ; -1.6506291914330982

;; Exercise 1.41

(defn double [f] (fn [x] (f (f x))))

(((double (double double)) inc) 5)

;; Exercise 1.42

(defn compose [f g] (fn [x] (f (g x))))

((compose square inc) 6)

;; Exercise 1.43

(defn repeated [f n]
  (if (= n 1) f
    (compose f (repeated f (dec n)))))

((repeated square 2) 5)

;; Exercise 1.44

(defn smooth [f]
  (fn [x]
    (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3)))

((smooth square) 4.0)

(defn n-fold-smooth [f n]
  ((repeated smooth n) f))

((n-fold-smooth square 3) 4.0)

;; Exercise 1.45

(defn fourth-root [x]
  (fixed-point ((repeated average-damp 2) (fn [y] (/ x (cube y)))) 1.0))

(fourth-root 256)

(Math/pow 2 10)

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn nth-avg-damp [n] (int (Math/floor (log2 n))))

(nth-avg-damp 4294967296)

(defn nth-root [x n]
  (fixed-point ((repeated average-damp (nth-avg-damp n)) (fn [y] (/ x (Math/pow y (dec n))))) 1.0))

(nth-root 4294967296 32)

;; Exercise 1.46

(defn iterative-improve [good-enough? improve]
  (defn try-it [guess]
    (if (good-enough? guess) guess
      (try-it (improve guess)))))

(defn sqrt [x]
  ((iterative-improve (fn [guess] (good-enough? guess x)) (fn [guess] (improve guess x))) 1.0))

(sqrt 16)

(defn fixed-point [f first-guess]
  ((iterative-improve (fn [guess] (close-enough? guess (f guess))) f) first-guess))

(defn cube-root [x]
  (fixed-point (average-damp (fn [y] (/ x (square y)))) 1.0))

(cube-root 64)
