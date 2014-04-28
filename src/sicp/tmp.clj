(ns tmp)

(defn error [message & args]
  (throw (RuntimeException. (clojure.string/join " " (cons message args)))))

;;

(defn for-each-except [exception procedure elements]
  (defn iter [items]
    (cond
     (empty? items) 'done
     (= (first items) exception) (iter (rest items))
     :else (do
             (procedure (first items))
             (iter (rest items)))))
  (iter elements))

(defn has-value? [connector]
  (connector 'has-value?))

(defn get-value [connector]
  (connector 'value))

(defn set-value! [connector new-value informant]
  ((connector 'set-value!) new-value informant))

(defn forget-value! [connector retractor]
  ((connector 'forget) retractor))

(defn connect [connector new-constraint]
  ((connector 'connect) new-constraint))

(defn make-connector []
  (let [value (atom false)
        informant (atom false)
        constraints (atom '())
        set-my-value (fn [newval setter]
                       (cond
                        (not (has-value? me)) (do
                                                (reset! value newval)
                                                (reset! informant setter)
                                                (for-each-except setter
                                                                 inform-about-value
                                                                 constraints))
                        (not= value newval) (error "Contradiction" value newval)
                        :else 'ignored))

        forget-my-value (fn [retractor]
                          (if (= retractor @informant)
                            (do
                              (reset! informant false)
                              (for-each-except retractor
                                               inform-about-no-value
                                               constraints))
                            'ignored))

        connect (fn [new-constraint]
                  (when (not-any? #{new-constraint} @constraints)
                    (swap! constraints #(cons new-constraint %)))
                  (when (has-value? me)
                    (inform-about-value new-constraint))
                  'done)

        me (fn [request]
             (condp = request
               'has-value? (if @informant true false)
               'value @value
               'set-value! set-my-value
               'forget forget-my-value
               'connect connect
               (error "Unknown operation -- CONNECTOR"request)))]

    me))

;;

(defn adder [a1 a2 sum]
  (let [process-new-value (fn []
                            (cond
                             (and
                              (has-value? a1)
                              (has-value? a2)) (set-value! sum (+ (get-value a1) (get-value a2)) me)
                             (and
                              (has-value? a1)
                              (has-value? sum)) (set-value! a2 (- (get-value sum) (get-value a1)) me)
                             (and
                              (has-value? a2)
                              (has-value? sum)) (set-value! a1 (- (get-value sum) (get-value a2)) me)))

        process-forget-value (fn []
                               (forget-value! sum me)
                               (forget-value! a1 me)
                               (forget-value! a2 me)
                               (process-new-value))

        me (fn [request]
             (condp = request
               'I-have-a-value (process-new-value)
               'I-lost-my-value (process-forget-value)
               (error "Unknown request -- ADDER" request)))]

    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me))

(defn multiplier [m1 m2 product]
  (let [process-new-value (fn []
                            (cond
                             (or (and
                                  (has-value? m1)
                                  (= (get-value m1) 0))
                                 (and
                                  (has-value? m2)
                                  (= (get-value m2) 0))) (set-value! product 0 me)
                             (and
                              (has-value? m1)
                              (has-value? m2)) (set-value! product (* (get-value m1) (get-value m2)) me)
                             (and
                              (has-value? m1)
                              (has-value? product)) (set-value! m2 (/ (get-value product) (get-value m1)) me)
                             (and
                              (has-value? m2)
                              (has-value? product)) (set-value! m1 (/ (get-value product) (get-value m2)) me)))

        process-forget-value (fn []
                               (forget-value! product me)
                               (forget-value! m1 me)
                               (forget-value! m2 me)
                               (process-new-value))

        me (fn [request]
             (condp = request
               'I-have-a-value (process-new-value)
               'I-lost-my-value (process-forget-value)
               (error "Unknown request -- MULTIPLIER" request)))]

    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me))

(defn constant [value connector]
  (let [me (fn [request]
             (error "Unknown request -- CONSTANT" request))]

    (connect connector me)
    (set-value! connector value me)
    me))

(defn probe [name connector]
  (let [print-probe (fn [value]
                      (println-str "Probe:" name "=" value))

        process-new-value (fn []
                            (print-probe (get-value connector)))

        process-forget-value (fn []
                               (print-probe "?"))

        me (fn [request]
             (condp = request
               'I-have-a-value (process-new-value)
               'I-lost-my-value (process-forget-value)
               (error "Unknown request -- PROBE" request)))]

    (connect connector me)
    me))

(defn inform-about-value [constraint]
  (constraint 'I-have-a-value))

(defn inform-about-no-value [constraint]
  (constraint 'I-lost-my-value))

;;


;;

(defn celsius-fahrenheit-converter [c f]
  (let [u (make-connector)
        v (make-connector)
        w (make-connector)
        x (make-connector)
        y (make-connector)]
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(def C (make-connector))

(def F (make-connector))

(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)

(probe "Fahrenheit temp" F)

(set-value! C 25 'user)

(forget-value! C 'user)

(set-value! F 212 'user)
