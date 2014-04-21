;; Exercise 2.77

(def proc-table (atom {}))

(defn pt-get [op type] (@proc-table [op type]))

(defn pt-put [op type item] (swap! proc-table #(assoc % [op type] item)))

(defn type-tag [datum]
  (cond (number? datum) datum
        (coll? datum) (first datum)
        :else (throw (RuntimeException. (str "Wrong datum -- TYPE-TAG " datum)))))

(defn contents [datum]
  (cond (number? datum) datum
        (coll? datum) (rest datum)
        :else (throw (RuntimeException. (str "Wrong datum -- CONTENGS " datum)))))

(defn attach-tag [tag content]
  (if (coll? content) (cons tag content) content))

(defn gcd [a b]
  (if (= b 0)
    a
    (gcd b (rem a b))))

;;; 2.77

(defn install-rectangular-package []
  (let [real-part (fn [z] (first z))
        imag-part (fn [z] (second z))
        make-from-real-imag (fn [x y] [x y])
        magnitude (fn [z]
                    (Math/sqrt (+ (#(* % %) (real-part z))
                                  (#(* % %) (imag-part z)))))
        angle (fn [z]
                (Math/atan2 (imag-part z) (real-part z)))
        make-from-mag-ang (fn [r a]
                            [(* r (Math/cos a))
                             (* r (Math/sin a))])
        tag (fn [x] (attach-tag 'rectangular x))]

    (pt-put 'real-part '(rectangular) real-part)
    (pt-put 'imag-part '(rectangular) imag-part)
    (pt-put 'magnitude '(rectangular) magnitude)
    (pt-put 'angle '(rectangular) angle)
    (pt-put 'make-from-real-imag 'rectangular
            (fn [x y] (tag (make-from-real-imag x y))))
    (pt-put 'make-from-mag-ang 'rectangular
            (fn [r a] (tag (make-from-mag-ang r a))))))

(defn install-polar-package []
  (let [magnitude (fn [z] (first z))
        angle (fn [z] (second z))
        make-from-mag-ang (fn [r a] [r a])
        real-part (fn [z]
                    (* (magnitude z) (Math/cos (angle z))))
        imag-part (fn [z]
                    (* (magnitude z) (Math/sin (angle z))))
        make-from-real-imag (fn [x y]
                              [(Math/sqrt (+ (#(* % %) x) (#(* % %) y)))
                               (Math/atan2 y x)])
        tag (fn [x] (attach-tag 'polar x))]


    (pt-put 'real-part '(polar) real-part)
    (pt-put 'imag-part '(polar) imag-part)
    (pt-put 'magnitude '(polar) magnitude)
    (pt-put 'angle '(polar) angle)
    (pt-put 'make-from-real-imag 'polar
            (fn [x y] (tag (make-from-real-imag x y))))
    (pt-put 'make-from-mag-ang 'polar
            (fn [r a] (tag (make-from-mag-ang r a))))))


(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (pt-get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (RuntimeException. (str "No method for -- " op type-tags))))))


(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle [z] (apply-generic 'angle z))

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))

(defn install-scheme-number-package []
  (let [tag (fn [x]
              (attach-tag 'scheme-number x))]

    (pt-put 'add '(scheme-number scheme-number)
            (fn [x y] (tag (+ x y))))
    (pt-put 'sub '(scheme-number scheme-number)
            (fn [x y] (tag (- x y))))
    (pt-put 'mul '(scheme-number scheme-number)
            (fn [x y] (tag (* x y))))
    (pt-put 'div '(scheme-number scheme-number)
            (fn [x y] (tag (/ x y))))
    (pt-put 'make 'scheme-number
            (fn [x] (tag x)))))

(defn make-scheme-number [n]
  ((pt-get 'make 'scheme-number) n))

(defn install-rational-package []
  (let [numer (fn [x] (first x))
        denom (fn [x] (second x))
        make-rat (fn [n d] (let [g (gcd n d)] [(/ n g) (/ d g)]))
        add-rat (fn [x y]
                  (make-rat (+ (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
        sub-rat (fn [x y]
                  (make-rat (- (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
        mul-rat (fn [x y]
                  (make-rat (* (numer x) (numer y))
                            (* (denom x) (denom y))))
        div-rat (fn [x y]
                  (make-rat (* (numer x) (denom y))
                            (* (denom x) (numer y))))
        tag (fn [x] (attach-tag 'rational x))
        ]

    (pt-put 'add '(rational rational)
            (fn [x y] (tag (add-rat x y))))
    (pt-put 'sub '(rational rational)
            (fn [x y] (tag (sub-rat x y))))
    (pt-put 'mul '(rational rational)
            (fn [x y] (tag (mul-rat x y))))
    (pt-put 'div '(rational rational)
            (fn [x y] (tag (div-rat x y))))
    (pt-put 'make 'rational
            (fn [n d] (tag (make-rat n d))))))

(defn make-rational [n d]
  ((pt-get 'make 'rational) n d))


(defn install-complex-package []
  (let [;; imported procedures from rectangular and polar packages
        make-from-real-imag (fn [x y]
                              ((pt-get 'make-from-real-imag 'rectangular) x y))
        make-from-mag-ang (fn [r a]
                            ((pt-get 'make-from-mag-ang 'polar) r a))
        add-complex (fn [z1 z2]
                      (make-from-real-imag (+ (real-part z1) (real-part z2))
                                           (+ (imag-part z1) (imag-part z2))))
        sub-complex (fn [z1 z2]
                      (make-from-real-imag (- (real-part z1) (real-part z2))
                                           (- (imag-part z1) (imag-part z2))))
        mul-complex (fn [z1 z2]
                      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                         (+ (angle z1) (angle z2))))
        div-complex (fn [z1 z2]
                      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                         (- (angle z1) (angle z2))))
        tag (fn [z] (attach-tag 'complex z))
        ]
    (pt-put 'add '(complex complex)
            (fn [z1 z2] (tag (add-complex z1 z2))))
    (pt-put 'sub '(complex complex)
            (fn [z1 z2] (tag (sub-complex z1 z2))))
    (pt-put 'mul '(complex complex)
            (fn [z1 z2] (tag (mul-complex z1 z2))))
    (pt-put 'div '(complex complex)
            (fn [z1 z2] (tag (div-complex z1 z2))))
    (pt-put 'make-from-real-imag 'complex
            (fn [x y] (tag (make-from-real-imag x y))))
    (pt-put 'make-from-mag-ang 'complex
            (fn [r a] (tag (make-from-mag-ang r a))))))

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(defn make-complex-from-real-imag [x y]
  ((pt-get 'make-from-real-imag 'complex) x y))
(defn make-complex-from-mag-ang [r a]
  ((pt-get 'make-from-mag-ang 'complex) r a))

(def z (make-complex-from-mag-ang 8 6))

; This fails
(magnitude z)

(pt-put 'real-part '(complex) real-part)
(pt-put 'imag-part '(complex) imag-part)
(pt-put 'magnitude '(complex) magnitude)
(pt-put 'angle '(complex) angle)

(magnitude z)


; apply-generic is invoked twice, first dispatch is magnitude of 'complex, second is magnitude of 'rectangular.

;; Exercise 2.78

(defn type-tag [datum]
  (cond (number? datum) 'scheme-number
        (coll? datum) (first datum)
        :else (throw (RuntimeException. (str "Wrong datum -- TYPE-TAG" datum)))))

(defn contents [datum]
  (cond (number? datum) datum
        (coll? datum) (rest datum)
        :else (throw (RuntimeException. (str "Wrong datum -- CONTENGS" datum)))))

(defn attach-tag [tag content]
  (if (coll? content) (cons tag content) content))

;; Exercise 2.81

;;a
; apply-generic will go into infinite recursion.

;;b
; apply-generic just works as it is.

;;c

(defn put-coercion [source-type target-type proc]
  (pt-put 'coercion [source-type target-type] proc))

(defn get-coercion [source-type target-type]
  (pt-get 'coercion [source-type target-type]))

(defn scheme-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(defn apply-generic [op & args]
  (defn no-method [type-tags]
    (throw (RuntimeException. (str "No method for -- " op " -- " type-tags))))

  (let [type-tags (map type-tag args)
        proc (pt-get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [type1 (first type-tags)
              type2 (second type-tags)
              a1 (first args)
              a2 (second args)]
          (if (= type1 type2)
            (no-method type-tags)
            (let [t1->t2 (get-coercion type1 type2)
                  t2->t1 (get-coercion type2 type1)]
              (cond
               t1->t2 (apply-generic op (t1->t2 a1) a2)
               t2->t1 (apply-generic op a1 (t2->t1 a2))
               :else (no-method type-tags)))))
        (no-method type-tags)))))

(add (make-rational 1 2) (make-rational 3 4))

(add (make-scheme-number 2) (make-complex-from-real-imag 3 4))

(get-coercion 'scheme-number 'complex)


(defn add [& args] (apply apply-generic 'add args))
(pt-put 'add '(scheme-number scheme-number scheme-number) str)
(pt-put 'add '(complex complex complex) str)

(defn apply-generic [op & args]
  ; coercing list to a type
  (defn coerce-list-to-type [lst type]
    (if (empty? lst) []
      (let [t1->t2 (get-coercion (type-tag (first lst)) type)]
        (if t1->t2
          (cons (t1->t2 (first lst)) (coerce-list-to-type (rest lst) type))
          (cons (first lst) (coerce-list-to-type (rest lst) type))))))

  ; applying to a list of multiple arguments
  (defn apply-coerced [lst]
    (if (empty? lst)
      (throw (RuntimeException. (str "No method for -- " op " - " args)))
      (let [coerced-list (coerce-list-to-type args (type-tag (first lst)))
            proc (pt-get op (map type-tag coerced-list))]
          (if proc
            (apply proc (map contents coerced-list))
            (apply-coerced (rest lst))))))

  ; logic to prevent always coercing if there is already direct input entry
  (let [type-tags (map type-tag args)
        proc (pt-get op type-tags)]
      (if proc
        (apply proc (map contents args))
        (apply-coerced args))))

(add (make-scheme-number 2) (make-scheme-number 2) (make-scheme-number 2))
(add (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))
(add (make-scheme-number 2) (make-complex-from-real-imag 3 4) (make-scheme-number 2))

; number -> rational -> complex

(defn raise [x] (apply-generic 'raise x))

(defn install-scheme-number-package []
  (let [tag (fn [x]
              (attach-tag 'scheme-number x))]

    (pt-put 'add '(scheme-number scheme-number)
            (fn [x y] (tag (+ x y))))
    (pt-put 'sub '(scheme-number scheme-number)
            (fn [x y] (tag (- x y))))
    (pt-put 'mul '(scheme-number scheme-number)
            (fn [x y] (tag (* x y))))
    (pt-put 'div '(scheme-number scheme-number)
            (fn [x y] (tag (/ x y))))
    (pt-put 'raise '(scheme-number)
            (fn [x] (make-rational x 1)))
    (pt-put 'make 'scheme-number tag)))

(defn install-rational-package []
  (let [numer (fn [x] (first x))
        denom (fn [x] (second x))
        make-rat (fn [n d] (let [g (gcd n d)] [(/ n g) (/ d g)]))
        add-rat (fn [x y]
                  (make-rat (+ (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
        sub-rat (fn [x y]
                  (make-rat (- (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
        mul-rat (fn [x y]
                  (make-rat (* (numer x) (numer y))
                            (* (denom x) (denom y))))
        div-rat (fn [x y]
                  (make-rat (* (numer x) (denom y))
                            (* (denom x) (numer y))))
        tag (fn [x] (attach-tag 'rational x))
        ]

    (pt-put 'add '(rational rational)
            (fn [x y] (tag (add-rat x y))))
    (pt-put 'sub '(rational rational)
            (fn [x y] (tag (sub-rat x y))))
    (pt-put 'mul '(rational rational)
            (fn [x y] (tag (mul-rat x y))))
    (pt-put 'div '(rational rational)
            (fn [x y] (tag (div-rat x y))))
    (pt-put 'raise '(rational)
            (fn [x] (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
    (pt-put 'make 'rational
            (fn [n d] (tag (make-rat n d))))))

(defn install-complex-package []
  (let [;; imported procedures from rectangular and polar packages
        make-from-real-imag (fn [x y]
                              ((pt-get 'make-from-real-imag 'rectangular) x y))
        make-from-mag-ang (fn [r a]
                            ((pt-get 'make-from-mag-ang 'polar) r a))
        add-complex (fn [z1 z2]
                      (make-from-real-imag (+ (real-part z1) (real-part z2))
                                           (+ (imag-part z1) (imag-part z2))))
        sub-complex (fn [z1 z2]
                      (make-from-real-imag (- (real-part z1) (real-part z2))
                                           (- (imag-part z1) (imag-part z2))))
        mul-complex (fn [z1 z2]
                      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                         (+ (angle z1) (angle z2))))
        div-complex (fn [z1 z2]
                      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                         (- (angle z1) (angle z2))))
        tag (fn [z] (attach-tag 'complex z))
        ]
    (pt-put 'add '(complex complex)
            (fn [z1 z2] (tag (add-complex z1 z2))))
    (pt-put 'sub '(complex complex)
            (fn [z1 z2] (tag (sub-complex z1 z2))))
    (pt-put 'mul '(complex complex)
            (fn [z1 z2] (tag (mul-complex z1 z2))))
    (pt-put 'div '(complex complex)
            (fn [z1 z2] (tag (div-complex z1 z2))))
    (pt-put 'make-from-real-imag 'complex
            (fn [x y] (tag (make-from-real-imag x y))))
    (pt-put 'make-from-mag-ang 'complex
            (fn [r a] (tag (make-from-mag-ang r a))))))

(install-scheme-number-package)
(install-rational-package)

(raise (raise (make-scheme-number 3)))

(def type-levels {'scheme-number 0, 'rational 1, 'complex 2})

(defn get-coercion [orig-type dest-type]
  (let [orig-level (type-levels orig-type)
        dest-level (type-levels dest-type)
        level-diff (- dest-level orig-level)]
    (if (> level-diff 0)
      (apply comp (repeat level-diff raise))
      nil)))

(defn apply-generic [op & args]
  ; coercing list to a type
  (defn coerce-list-to-type [lst type]
    (if (empty? lst) []
      (let [t1->t2 (get-coercion (type-tag (first lst)) type)]
        (if t1->t2
          (cons (t1->t2 (first lst)) (coerce-list-to-type (rest lst) type))
          (cons (first lst) (coerce-list-to-type (rest lst) type))))))

  ; applying to a list of multiple arguments
  (defn apply-coerced [lst]
    (if (empty? lst)
      (throw (RuntimeException. (str "No method for -- " op " - " args)))
      (let [coerced-list (coerce-list-to-type args (type-tag (first lst)))
            proc (pt-get op (map type-tag coerced-list))]
          (if proc
            (apply proc (map contents coerced-list))
            (apply-coerced (rest lst))))))

  ; logic to prevent always coercing if there is already direct input entry
  (let [type-tags (map type-tag args)
        proc (pt-get op type-tags)]
      (if proc
        (apply proc (map contents args))
        (apply-coerced args))))

(add (make-scheme-number 2) (make-scheme-number 2) (make-scheme-number 2))
(add (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 3 4))
(add (make-scheme-number 2) (make-complex-from-real-imag 3 4) (make-scheme-number 2))


(defn equ? [x y] (apply-generic 'equ? x y))

;; Exercise 2.85

(defn install-scheme-number-package []
  (let [tag (fn [x]
              (attach-tag 'scheme-number x))]

    (pt-put 'add '(scheme-number scheme-number)
            (fn [x y] (tag (+ x y))))
    (pt-put 'sub '(scheme-number scheme-number)
            (fn [x y] (tag (- x y))))
    (pt-put 'mul '(scheme-number scheme-number)
            (fn [x y] (tag (* x y))))
    (pt-put 'div '(scheme-number scheme-number)
            (fn [x y] (tag (/ x y))))
    (pt-put 'raise '(scheme-number)
            (fn [x] (make-rational x 1)))
    (pt-put 'equ? '(scheme-number scheme-number) =)
    (pt-put 'make 'scheme-number tag)))

(defn install-rational-package []
  (let [numer (fn [x] (first x))
        denom (fn [x] (second x))
        make-rat (fn [n d] (let [g (gcd n d)] [(/ n g) (/ d g)]))
        add-rat (fn [x y]
                  (make-rat (+ (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
        sub-rat (fn [x y]
                  (make-rat (- (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
        mul-rat (fn [x y]
                  (make-rat (* (numer x) (numer y))
                            (* (denom x) (denom y))))
        div-rat (fn [x y]
                  (make-rat (* (numer x) (denom y))
                            (* (denom x) (numer y))))
        tag (fn [x] (attach-tag 'rational x))]

    (pt-put 'add '(rational rational)
            (fn [x y] (tag (add-rat x y))))
    (pt-put 'sub '(rational rational)
            (fn [x y] (tag (sub-rat x y))))
    (pt-put 'mul '(rational rational)
            (fn [x y] (tag (mul-rat x y))))
    (pt-put 'div '(rational rational)
            (fn [x y] (tag (div-rat x y))))
    (pt-put 'raise '(rational)
            (fn [x] (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
    (pt-put 'project '(rational)
            (fn [x] (make-scheme-number (quot (numer x) (denom x)))))
    (pt-put 'equ? '(rational rational)
            (fn [x y] (= (* (numer x) (denom y)) (* (numer y) (denom x)))))
    (pt-put 'make 'rational
            (fn [n d] (tag (make-rat n d))))))

(defn install-complex-package []
  (let [;; imported procedures from rectangular and polar packages
        make-from-real-imag (fn [x y]
                              ((pt-get 'make-from-real-imag 'rectangular) x y))
        make-from-mag-ang (fn [r a]
                            ((pt-get 'make-from-mag-ang 'polar) r a))
        add-complex (fn [z1 z2]
                      (make-from-real-imag (+ (real-part z1) (real-part z2))
                                           (+ (imag-part z1) (imag-part z2))))
        sub-complex (fn [z1 z2]
                      (make-from-real-imag (- (real-part z1) (real-part z2))
                                           (- (imag-part z1) (imag-part z2))))
        mul-complex (fn [z1 z2]
                      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                         (+ (angle z1) (angle z2))))
        div-complex (fn [z1 z2]
                      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                         (- (angle z1) (angle z2))))
        tag (fn [z] (attach-tag 'complex z))]
    (pt-put 'add '(complex complex)
            (fn [z1 z2] (tag (add-complex z1 z2))))
    (pt-put 'sub '(complex complex)
            (fn [z1 z2] (tag (sub-complex z1 z2))))
    (pt-put 'mul '(complex complex)
            (fn [z1 z2] (tag (mul-complex z1 z2))))
    (pt-put 'div '(complex complex)
            (fn [z1 z2] (tag (div-complex z1 z2))))
    (pt-put 'project '(complex)
            (fn [x] (make-rational (real-part x) 1)))
    (pt-put 'equ? '(complex complex)
            (fn [x y] (and (= (real-part x) (real-part y)) (= (imag-part y) (imag-part x)))))
    (pt-put 'make-from-real-imag 'complex
            (fn [x y] (tag (make-from-real-imag x y))))
    (pt-put 'make-from-mag-ang 'complex
            (fn [r a] (tag (make-from-mag-ang r a))))))


(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(equ? (make-scheme-number 2) (make-scheme-number 2))
(equ? (make-rational 2 3) (make-rational 4 6))
(equ? (make-scheme-number 1) (make-rational 6 6))

(defn drop-type [x]
  (if-let [project-proc (pt-get 'project (list (type-tag x)))]
    (let [project-number (project-proc (contents x))]
      (if (equ? project-number x)
        (drop-type project-number)
        x))
    x))


(apply-generic 'project (make-complex-from-real-imag 3 0))
(apply-generic 'raise (make-rational 3 1))
(type-tag (make-complex-from-real-imag 3 0))
(let [project-proc (pt-get 'project (type-tag (make-complex-from-real-imag 3 0)))] project-proc)
(drop-type (make-complex-from-real-imag 3 0))

(defn apply-generic [op & args]
  ; coercing list to a type
  (defn coerce-list-to-type [lst type]
    (if (empty? lst) []
      (let [t1->t2 (get-coercion (type-tag (first lst)) type)]
        (if t1->t2
          (cons (t1->t2 (first lst)) (coerce-list-to-type (rest lst) type))
          (cons (first lst) (coerce-list-to-type (rest lst) type))))))

  ; applying to a list of multiple arguments
  (defn apply-coerced [lst]
    (if (empty? lst)
      (throw (RuntimeException. (str "No method for -- " op " - " args)))
      (let [coerced-list (coerce-list-to-type args (type-tag (first lst)))
            proc (pt-get op (map type-tag coerced-list))]
          (if proc
            (apply proc (map contents coerced-list))
            (apply-coerced (rest lst))))))

  ; logic to prevent always coercing if there is already direct input entry
  (let [type-tags (map type-tag args)
        proc (pt-get op type-tags)]
      (if proc
        (drop-type (apply proc (map contents args)))
        (apply-coerced args))))

(add (make-complex-from-real-imag 5 -3) (make-complex-from-real-imag 2 3))

; /Users/frankie/Desktop/temp-2-82.clj:309 proc-table/raise
; /Users/frankie/Desktop/temp-2-82.clj:546 proc-table/drop-type
; /Users/frankie/Desktop/temp-2-82.clj:581 proc-table/apply-generic
