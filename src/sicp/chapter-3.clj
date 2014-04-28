(ns sicp.ch3)

(defn error [message & args]
  (throw (RuntimeException. (clojure.string/join " " (cons message args)))))

(set! *print-length* 10)

;; Exercise 3.1

(defn make-accumulator [x]
  (let [acc (atom x)
        acc-fn (fn [y] (do (swap! acc #(+ % y)) @acc))]
    acc-fn))

(def A (make-accumulator 5))

(A 10)

(A 10)

;; Exercise 3.2

(defn make-monitored [f]
  (let [counter (atom 0)
        counter-fn (fn [param] (condp = param
                                 'how-many-calls? @counter
                                 'reset-count (reset! counter 0)
                                 (do (swap! counter inc) (f param))))]
    counter-fn))


(def s (make-monitored #(Math/sqrt %)))

(s 100)

(s 25)

(s 'how-many-calls?)

(s 'reset-count)

(s 36)

(s 'how-many-calls?)

;; Exercise 3.3

(defn make-account [initial-balance account-pwd]
  (let [balance (atom initial-balance)
        withdraw (fn [amount]
                   (if (>= @balance amount)
                     (do (swap! balance #(- % amount))
                       @balance)
                     "Insufficient funds"))
        deposit (fn [amount]
                  (swap! balance #(+ % amount))
                  @balance)
        dispatch (fn [m]
                   (condp = m
                     'withdraw withdraw
                     'deposit deposit
                     (error "Unknown request -- MAKE-ACCOUNT " m)))
        pwd-check (fn [input-pwd m]
                    (if (= input-pwd account-pwd)
                      (dispatch m)
                      (fn [ignored] "Incorrect password")))]
    pwd-check))

(def acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

;; Exercise 3.4

(defn make-account [account-pwd initial-balance]
  (let [balance (atom initial-balance)
        withdraw (fn [amount]
                   (if (>= @balance amount)
                     (do (swap! balance #(- % amount))
                       @balance)
                     "Insufficient funds"))
        deposit (fn [amount]
                  (swap! balance #(+ % amount))
                  @balance)
        dispatch (fn [m]
                   (condp = m
                     'withdraw withdraw
                     'deposit deposit
                     (error "Unknown request -- MAKE-ACCOUNT " m)))
        wrong-pwd (make-monitored (fn [ignored] "Incorrect password"))
        call-the-cops (fn [ignored] "Freeze!")
        pwd-check (fn [input-pwd m]
                    (cond
                     (= input-pwd account-pwd) (dispatch m)
                     (< (wrong-pwd 'how-many-calls?) 2) wrong-pwd
                     :else call-the-cops))]
    pwd-check))

(def acc (make-account 'secret-password 100))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

((acc 'some-other-password 'deposit) 50)

((acc 'some-other-password 'deposit) 50)

;; Exercise 3.5

(defn monte-carlo [trials experiment]
  (defn iter [trials-remaining trials-passed]
    (cond
     (= trials-remaining 0) (/ trials-passed trials)
     (experiment) (iter (- trials-remaining 1) (+ trials-passed 1))
     :else (iter (- trials-remaining 1) trials-passed)))
  (iter trials 0))

(defn random-in-range [low high]
  (let [range (- high low)]
    (+ low (rand range))))

(defn estimate-integral [predicate x1 x2 y1 y2 trials]
  (defn experiment [] (predicate (random-in-range x1 x2) (random-in-range y1 y2)))
  (let [percent (monte-carlo trials experiment)
        area (* (- x2 x1) (- y2 y1))]
    (* percent area)))

(defn square [x] (* x x))

(defn unit-circle-predicate [x y] (<= (+ (square x) (square y)) 1))

(def estimate-pi (estimate-integral unit-circle-predicate -1.0 1.0 -1.0 1.0 1000))

estimate-pi

;; Exercise 3.6

(defn rand-update "Linear Congruential Generator" [x]
  (let [a (Math/pow 2 32)
        c 1103515245
        m 12345]
    (mod (+ (* a x) c) m)))

(def random-init 137)

(def random
  (let [x (atom random-init)]
    (defn dispatch [m]
      (condp = m
        'generate (do (swap! x rand-update)
                    @x)
        'reset (fn [new-x]
                 (reset! x new-x))
        (error "unknown request")))
    dispatch))


(random 'generate)

(random 'generate)

((random 'reset) 3062)

(random 'generate)

;; Exercise 3.7

(defn password-protect [password subject]
  (defn call-the-cops [ignored-msg]
    "Freeze!")
  (def wrong-pwd
    (make-monitored (fn [ignored-msg]
                      "Incorrect password")))
  (defn check-pwd [input-pwd msg]
    (cond
     (= input-pwd password) (subject msg)
     (< (wrong-pwd 'how-many-calls?) 2) wrong-pwd
     :else call-the-cops))

  check-pwd)


(defn make-account [account-pwd initial-balance]
  (let [balance (atom initial-balance)
        withdraw (fn [amount]
                   (if (>= @balance amount)
                     (do (swap! balance #(- % amount))
                       @balance)
                     "Insufficient funds"))
        deposit (fn [amount]
                  (swap! balance #(+ % amount))
                  @balance)
        dispatch (fn [m]
                   (condp = m
                     'withdraw withdraw
                     'deposit deposit
                     (error "Unknown request -- MAKE-ACCOUNT " m)))]
    (password-protect account-pwd dispatch)))

(def peter-acc
  (make-account 'open-sesame 100))

(defn make-joint [account original-password new-password]
  (defn forward [msg]
    (account original-password msg))
  (password-protect new-password forward))

(def paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10)

((paul-acc 'rosebud 'withdraw) 10)

;; Exercise 3.8

(def f
  (let [state (atom 0)]
    (defn switch-state [x]
      (let [old-state @state]
        (reset! state (+ x old-state))
        old-state))
    switch-state))

(f 0)

(f 1)

(f 0)


;; Exercise 3.28

(defn call-each [procedures]
  (if (empty? procedures)
      'done
      (do
        ((first procedures))
        (call-each (rest procedures)))))

(defn make-wire []
  (let [signal-value (atom 0)
        action-procedures (atom [])

        set-my-signal! (fn [new-value]
                         (if (not= signal-value new-value)
                           (do
                             (reset! signal-value new-value)
                             (call-each @action-procedures))
                           'done))

        accept-action-procedure! (fn [proc]
                                   (swap! action-procedures #(cons proc %))
                                   (proc))

        dispatch (fn [m]
                   (condp = m
                     'get-signal @signal-value
                     'set-signal! set-my-signal!
                     'add-action! accept-action-procedure!
                     (error "Unknown operation -- WIRE " m)))]
    dispatch))


(defn get-signal [wire]
  (wire 'get-signal))

(defn set-signal! [wire new-value]
  ((wire 'set-signal!) new-value))

(defn add-action! [wire action-procedure]
  ((wire 'add-action!) action-procedure))

;;

(defn make-queue [] (atom []))

(defn delete-queue! [q] (swap! q pop))

(defn insert-queue! [q e] (swap! q #(conj % e)))

(defn empty-queue? [q] (empty? @q))

(defn front-queue [q] (first @q))

(defn make-time-segment [time queue] [time queue])

(defn segment-time [s] (first s))

(defn segment-queue [s] (second s))

(defn make-agenda [] (atom {:time 0 :segments []}))

(defn current-time [agenda] (:time @agenda))

(defn set-current-time! [agenda time] (swap! agenda #(assoc % :time time)))

(defn segments [agenda] (:segments @agenda))

(defn set-segments! [agenda segments] (swap! agenda #(assoc % :segments segments)))

(defn first-segment [agenda] (first (segments agenda)))

(defn rest-segments [agenda] (rest (segments agenda)))

(defn empty-agenda? [agenda] (empty? (segments agenda)))

(defn add-to-agenda! [time action agenda]
  (defn belongs-before? [segments]
    (or (empty? segments)
        (< time (segment-time (first segments)))))

  (defn make-new-time-segment [time action]
    (let [q (make-queue)]
      (insert-queue! q action)
      (make-time-segment time q)))

  (defn add-to-segments! [segments]
    (if (= (segment-time (first segments)) time)
        (do
          (insert-queue!
           (segment-queue (first segments)) action)
          segments)
        (let [others (rest segments)
              others+segment (if (belongs-before? others)
                               (cons (make-new-time-segment time action) others)
                               (add-to-segments! others))]
          (if (empty? others)
            others+segment
            (cons (first segments) others+segment)))))

  (let [new-segments (add-to-segments! (segments agenda))]
    (set-segments! agenda new-segments)))

(defn remove-first-agenda-item! [agenda]
  (let [q (segment-queue (first-segment agenda))]
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let [first-seg (first-segment agenda)]
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(def the-agenda (make-agenda))

(defn after-delay [delay action]
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(defn propagate []
  (if (empty-agenda? the-agenda)
      'done
      (let [first-item (first-agenda-item the-agenda)]
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(defn probe [name wire]
  (add-action! wire #(println
                      "Name:"
                      name
                      "| Time:"
                      (current-time the-agenda)
                      "| New-value:"
                      (get-signal wire))))

;;

(def inverter-delay 2)

(def and-gate-delay 3)

(def or-gate-delay 5)

;;

(defn and-gate [a1 a2 output]
  (defn logical-and [s1 s2]
    (condp = [s1 s2]
      [1 1] 1
      [1 0] 0
      [0 1] 0
      [0 0] 0
      (error "Invalid signals " s1 " " s2)))

  (defn and-action-procedure []
    (let [new-value (logical-and (get-signal a1) (get-signal a2))]
      (after-delay and-gate-delay
                   (fn []
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(defn inverter [input output]
  (defn logical-not [s]
    (case s
      0 1
      1 0
      (error "Invalid signal " s)))

  (defn invert-input []
    (let [new-value (logical-not (get-signal input))]
      (after-delay inverter-delay
                   (fn []
                     (set-signal! output new-value)))))

  (add-action! input invert-input)
  'ok)


(defn or-gate [a1 a2 output]
  (defn logical-or [s1 s2]
    (condp = [s1 s2]
      [1 1] 1
      [1 0] 1
      [0 1] 1
      [0 0] 0
      (error "Invalid signals " s1 " " s2)))

  (defn or-action-procedure []
    (let [new-value (logical-or (get-signal a1) (get-signal a2))]
      (after-delay or-gate-delay
                   (fn []
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; Exercise 3.29

(defn or-gate [a1 a2 output]
  (let [c1 (make-wire)
        c2 (make-wire)
        c3 (make-wire)]
    (inverter a1 c1)
    (inverter a2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)))

;; Exercise 3.30

(defn half-adder [a b s c]
  (let [d (make-wire) e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defn full-adder [a b c-in sum c-out]
  (let [s (make-wire)
        c1 (make-wire)
        c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(defn ripple-carry-adder [a b s c-out]
  (let [c-in (make-wire)]
    (if (empty? a)
      (set-signal! c-in 0)
      (ripple-carry-adder (first a) (first b) (first s) c-in))
    (full-adder (first a) (first b) c-in (first s) c-out)))

;; Exercise 3.31

; Because accept-action-procedure! adds the procedure to the wire, not to the agenda

;; Exercise 3.32

(def input-1 (make-wire))
(def input-2 (make-wire))
(def sum (make-wire))
(def carry (make-wire))
(probe 'sum sum)
; sum 0  New-value = 0
(probe 'carry carry)
; carry 0  New-value = 0
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
; sum 8  New-value = 1
(set-signal! input-2 1)
(propagate)
; carry 11  New-value = 1
; sum 16  New-value = 0

; The procedure that sets the output checks the current value of the input wires.
; If that procedure is executed first (LIFO) it may not yet see the changes to the inputs.


;; Exercise 3.50

(defmacro cons-stream [el coll]
  (list 'lazy-seq (list cons el coll)))

(defn stream-map [proc coll & more]
  (let [argstreams (cons coll more)]
    (if (some true? (map empty? argstreams))
      '()
      (cons-stream
       (apply proc (map first argstreams))
       (apply stream-map proc (map rest argstreams))))))

;; Exercise 3.51

(defn show [x]
  (println x)
  x)

(def x (stream-map show (range 10))) ; 0
(nth x 5) ; 1 2 3 4 5
(nth x 7) ; 6 7

;; Exercise 3.52

(def sum (atom 0))

(defn accum [x]
  (swap! sum #(+ x %)))

(def stream (map accum (range 1 20))) ; 1

(def y (filter even? stream)) ; 6

(def z (filter #(= (rem % 5) 0) stream)) ; 10

(nth y 7) ; 136

;; Exercise 3.53

(def s (cons-stream 1 (map + s s))) ; 1 2 4 6 16

;; Exercise 3.54

(def factorials (cons-stream 1 (map * factorials (iterate inc 2))))

;; Exercise 3.55

(defn partial-sums [stream]
  (if (empty? stream)
    '()
    (cons-stream
     (first stream)
     (map +
          (rest stream)
          (partial-sums stream)))))

;; Exercise 3.56

(defn merge-streams [s1 s2]
  (cond
   (empty? s1) s2
   (empty? s2) s1
   :else (let [s1car (first s1)
               s2car (first s2)
               s1cdr (rest s1)
               s2cdr (rest s2)]
           (cond
            (< s1car s2car) (cons-stream s1car (merge-streams s1cdr s2))
            (> s1car s2car) (cons-stream s2car (merge-streams s1 s2cdr))
            (= s1car s2car) (cons-stream s1car (merge-streams s1cdr s2cdr))))))

(defn scale-stream [s n]
  (map #(* n %) s))

(def integers (range 1 100))

(def S (cons-stream 1 (merge-streams (scale-stream integers 2)
                                     (merge-streams (scale-stream integers 3)
                                                    (scale-stream integers 5)))))

(take 20 S)

;; Exercise 3.57

(def fibs
  (cons-stream 0
               (cons-stream 1
                            (map + fibs (rest fibs)))))

; If memoized steps are linear, otherwise it's the same as a naive recursive.

;; Exercise 3.58

(defn expand [num den radix]
  (cons-stream
   (quot (* num radix) den)
   (expand (rem (* num radix) den) den radix)))

(expand 1 7 10)

(expand 3 8 10)

;; Exercise 3.59

; a

(defn integrate-series [s]
   (map / s integers))

; b

(def cosine-series
  (cons-stream 1 (map #(* -1 %) (integrate-series sine-series))))
(def sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Exercise 3.60

(defn add-streams [s1 s2]
  (map + s1 s2))

(defn mul-series [s1 s2]
  (cons-stream (* (first s1) (first s2))
               (add-streams
                (add-streams (scale-stream (rest s1)
                                           (first s2))
                             (scale-stream (rest s1)
                                           (first s2)))
                (cons-stream 0 (mul-series (rest s1)
                                           (rest s2))))))

(def one (add-streams (mul-series sine-series sine-series)
                      (mul-series cosine-series cosine-series)))

(take 3 one)

;; Exercise 3.61

(defn invert-unit-series [s]
  (cons-stream 1 (mul-series
                  (map #(* -1 %) s)
                  (invert-unit-series s))))

;; Exercise 3.62

(defn div-series [num den]
  (let [divisor (/ 1 (first den))]
    (scale-stream  (mul-series num
                               (invert-unit-series (scale-stream den divisor))
                   divisor))))

(def tangent-series (div-series sine-series cosine-series))

;; Exercise 3.63

(defn average [x y] (/ (+ x y) 2))

(defn sqrt-improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-stream [x]
  (def guesses
    (cons-stream 1.0
                 (map (fn [guess]
                        (sqrt-improve guess x)) guesses)))
  guesses)

(take 8 (sqrt-stream 2))

; By defining guesses we leverage on the memoization used inside the data structure itself
; If we call (sqrt-stream x) directly every time there's no caching of values

;; Exercise 3.64

(defn stream-limit [stream tolerance]
  (let [difference (Math/abs (- (first stream) (second stream)))]
    (if (< difference tolerance)
      (second stream)
      (stream-limit (rest stream) tolerance))))

(defn sqrt [x tolerance]
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.1)

;; Exercise 3.65

(defn pi-summands [n]
  (cons-stream (/ 1.0 n)
               (map - (pi-summands (+ n 2)))))
(def pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(take 8 pi-stream)

(defn euler-transform [s]
  (let [s0 (nth s 0)  ; Sn-1
        s1 (nth s 1)  ; Sn
        s2 (nth s 2)] ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (rest s)))))

(take 8 (euler-transform pi-stream))

(defn make-tableau [transform s]
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(defn accelerated-sequence [transform s]
  (map first (make-tableau transform s)))

(take 8 (accelerated-sequence euler-transform pi-stream))

;

(defn ln2-summands [n]
  (cons-stream (/ 1.0 n)
               (map - (ln2-summands (inc n)))))
(def ln2-stream
  (partial-sums (ln2-summands 1)))

(take 8 ln2-stream)

(take 8 (accelerated-sequence euler-transform ln2-stream))

;; Exercise 3.66

(defn pairs [s t]
  (cons-stream
   [(first s) (first t)]
   (interleave
    (map #(vector (first s) %) (rest t))
    (pairs (rest s) (rest t)))))

(pairs [1 2 3 4] [5 6 7 8])

;; Exercise 3.67

(defn pairs [s t]
  (cons-stream
    [(first s) (first t)]
    (interleave
     (map #(vector (first s) %) (rest t))
     (map #(vector % (first t)) (rest s))
     (pairs (next s) (next t)))))

(pairs [1 2 3 4] [5 6 7 8])

;; Exercise 3.68

(defn pairs [s t]
  (interleave
   (map #(vector (first s) %) t)
   (pairs (rest s) (rest t))))

(pairs integers integers) ; The first 'map never terminates if not defined lazily

;; Exercise 3.69

(defn triples [s t u]
  (cons-stream
    [(first s) (first t) (first u)]
   (interleave
    (map #(cons (first s) %) (pairs t (rest u)))
    (triples (rest s) (rest t) (rest u)))))

(triples [1 2 3 4] [5 6 7 8] [9 10 11 12])

(defn pythagorean-test [x y z] (= (+ (square x) (square y)) (square z)))

(def pythagorean-triples (filter pythagorean-test (triples integers integers integers)))

(take 10 pythagorean-triples)

;; Exercise 3.70

(defn merge-weighted [s1 s2 weight]
  (cond
   (empty? s1) s2
   (empty? s2) s1
   :else (let [s1car (first s1)
               s2car (first s2)
               w1 (weight s1car)
               w2 (weight s2car)
               s1cdr (rest s1)
               s2cdr (rest s2)]
           (cond
            (< w1 w2) (cons-stream s1car (merge-streams s1cdr s2))
            (> w1 w2) (cons-stream s2car (merge-streams s1 s2cdr))
            (= w1 w2) (cons-stream s1car (merge-streams s1cdr s2cdr))))))

(defn weighted-pairs [s1 s2 weight]
  (merge-weighted
   (map #(vector (first s) %) t)
   (pairs (rest s) (rest t))
   weight))

; a

(take 20 (weighted-pairs integers integers #(+ %1 %2)))

; b

(defn weight [i j] (+ (* 2 i) (* 3 j) (* 5 i j)))

(take 20 (-> (scale-stream integers 2)
             (weighted-pairs (scale-stream 3) weight)
             (weighted-pairs (scale-stream 5) weight)))

;; Exercise 3.71

(defn cube [x] (* x x x))

(defn weight [i j] (+ (cube i) (cube j)))

(defn search-pairs [int-pairs]
  (let [x (first int-pairs)
        y (second int-pairs)]
    (if (= (weight x) (weight y))
      (cons-stream x (search-pairs (rest int-pairs)))
      (search-pairs (rest int-pairs)))))

(def rama-pairs (search-pairs (weighted-pairs integers integers weight)))

(take 6 rama-pairs)

;; Exercise 3.72

(defn weight [i j] (+ (square i) (square j)))

(defn search-pairs [int-pairs]
  (let [x (nth int-pairs 0)
        y (nth int-pairs 1)
        y (nth int-pairs 2)]
    (if (= (weight x) (weight y) (weight z))
      (cons-stream [(weight x) [x y z]] (search-pairs (rest int-pairs)))
      (search-pairs (rest int-pairs)))))

(take 6 (search-pairs (weighted-pairs integers integers weight)))

;; Exercise 3.73

(defn integral [integrand initial-value dt]
  (def stream
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              stream)))
  stream)

(defn RC [R C dt]
  (fn [i v0]
    (let [Ri (scale-stream i R)
          reverse-C (scale-stream i (/ 1 C))
          integral (reverse-C v0 dt)]
      (add-streams Ri integral))))

(def RC1 (RC 5 1 0.5))

;; Exercise 3.74

(defn sign-change-detector [current last]
  (cond
   (and (neg? last) (pos? current)) 1
   (and (pos? last) (neg? current)) -1
   :else 0))

(def sense-data '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(def zero-crossings
  (map sign-change-detector sense-data (cons-stream 0 sense-data)))

;; Exercise 3.75

(defn make-zero-crossings [input-stream last-value last-avpt]
  (let [avpt (/ (+ (first input-stream) last-value) 2)]
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (rest input-stream)
                                      (first input-stream)
                                      avpt))))

;; Exercise 3.76

(defn smooth [input-stream]
  (let [a (first input-stream)
        b (second input-stream)
        avg (/ (+ a b) 2)]
    (cons-stream avg (smooth (rest input-stream)))))

(defn make-zero-crossings [input-stream]
  (let [smooth-input (smooth input-stream)]
    (map sign-change-detector smooth-input (cons-stream 0 smooth-input))))

;; Exercise 3.77

(defn integral [delayed-integrand initial-value dt]
  (cons-stream initial-value
               (let [integrand (force delayed-integrand)]
                 (if (empty? integrand)
                     integrand
                     (integral (delay (rest integrand))
                               (+ (* dt (first integrand))
                                  initial-value)
                               dt)))))

;; Exercise 3.78

(defn solve-2nd [a b dt y0 dy0]
  (letfn [(y [] (integral (delay (dy))  y0 dt))
          (dy [] (integral (delay (ddy)) dy0 dt))
          (ddy [] (add-streams
                   (scale-stream (dy) a)
                   (scale-stream (y) b)))]
    (y)))

;; Exercise 3.79

(defn solve-2nd [f dt y0 dy0]
  (letfn [(y [] (integral (delay (dy)) y0 dt))
          (dy [] (integral (delay (ddy)) dy0 dt))
          (ddy [] (map f (dy) (y)))]
    (y)))

;; Exercise 3.80

(defn RLC [R L C dt]
  (fn [vC0 iL0]
   (letfn [(iL [] (integral (delay (diL)) iL0 dt))
           (diL [] (add-streams
                    (scale-stream (iL) (- (/ R L)))
                    (scale-stream (vC) (/ 1 L))))
           (vC [] (integral (delay (dvC)) vC0 dt))
           (dvC [] (scale-stream (iL) (/ -1 C)))]
     (list (vC) (iL)))))

;; Exercise 3.81

(defn random-numbers [requests-stream]
  (defn action [last-value message]
    (if (= message 'generate)
      (rand-update last-value)
      message ; New seed
      ))
  (cons-stream random-init
               (map action (random-numbers requests-stream) requests-stream)))

;; Exercise 3.82

(defn monte-carlo [experiment-stream passed failed]
  (defn iter [passed failed]
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (rest experiment-stream) passed failed)))

  (if (first experiment-stream)
      (iter (+ passed 1) failed)
      (iter passed (+ failed 1))))

(defn random-numbers-in-range [low high]
  (cons-stream
   (random-in-range low high)
   (random-numbers-in-range low high)))

(defn estimate-integral [predicate x1 x2 y1 y2]
  (let [experiment-stream (map predicate (random-numbers-in-range x1 x2) (random-numbers-in-range y1 y2))
        monte-carlo-stream (monte-carlo experiment-stream 0 0)
        area (* (- x2 x1) (- y2 y1))]
    (scale-stream monte-carlo-stream area)))

(def estimate-pi-stream (estimate-integral unit-circle-predicate -1.0 1.0 -1.0 1.0))

(nth estimate-pi-stream 1000)
