(ns sicp.ch3)

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
                     (throw (RuntimeException. (str "Unknown request -- MAKE-ACCOUNT " m)))))
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
                     (throw (RuntimeException. (str "Unknown request -- MAKE-ACCOUNT " m)))))
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

(estimate-integral unit-circle-predicate -1 1 -1 1 1000)

(def estimate-pi (estimate-integral unit-circle-predicate -1 1 -1 1 1000))

(double estimate-pi)

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
        (throw (RuntimeException. "unknown request"))))
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
                     (throw (RuntimeException. (str "Unknown request -- MAKE-ACCOUNT " m)))))]
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


