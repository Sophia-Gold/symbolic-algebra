(ns symbolic-algebra.core)

;GCD 
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))
  
;TYPES
(defrecord Real [n])
(defrecord Rational [n d])
(defrecord Complex [r i])
(defrecord Poly [p1 p2])
(def type-hierarchy [Real Rational Complex Poly])

;INTERFACE
(defmulti make (fn [x & y] x))

;REALS
(defmethod make Real [x & y] x)

;RATIONALS
(defn numer [x] (first x))
(defn denom [x] (rest x))
(defn make-rat [n d]
  (let [g (gcd n d)]
    (cons (/ n g) (/ d g))))
(defmethod make Rational [x & y]
  (make-rat x (first y)))

;COMPLEX
(defn make-from-real-imag [x y]
  (cons x y))
(defn real-part [rect] (first rect))
(defn imag-part [rect] (rest rect))
(defn magnitude [polar]
  (first polar))
(defn angle [polar]
  (rest polar))
(defn make-from-mag-ang [r a]
  (cons r a))
(defmethod make Complex [x & y]
  (make-from-real-imag x (first y)))

;TYPE COERCION
(defn raise-types [a b proc]
  (let [type-a (class a)
        type-b (class b)]
      (cond
        (= (class type-b) Complex) (proc (make Complex a 0) b)
        (= (class type-b) Rational) (proc (make Rational a a) b)
        (= (class type-a) Complex) (proc (make Complex b 0) a)
        (= (class type-a) Rational) (proc (make Rational b b) a))))

(defn reduce-type [a]
  (let [type-a (class a)]
    (cond
      (and (= (class type-a) Complex) (= (imag-part a) 0)) (if (= (class (real-part a)) Rational)                                                       
                                                                 (reduce-type (make Rational (real-part a)))                                                               
                                                                 (make Real (real-part a)))
      (and (= (class type-a) Rational) (= (denom a) 1)) (make Real (numer a))
      :else a)))

;; (defn raise-types [a b proc]
;;   (let [type-a (class a)
;;         type-b (class b)]
;;     (if (< (.indexOf type-hierarchy type-a) (.indexOf type-hierarchy type-b))
;;       (if (= (type type-b) Real)
;;         (proc (make Rational a a) b)
;;         (proc (make Complex a 0) b))
;;       (if (= (type type-b) Real)
;;         (proc b (make Rational b b))
;;         (proc b (make Complex a 0))))))


;PROTOCOLS
(defprotocol Algebra
  (add [a b])
  (sub [a b])
  (mul [a b])
  (div [a b])
  (equal? [a b]))

(extend-type Real
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (+ a b)
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (- a b)
      (raise-types a b sub)))
  (mul [a b]
     (if (= (class a) (class b))
      (* a b)
      (raise-types a b mul)))
  (div [a b]
    (if (= (class a) (class b))
      (/ a b)
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (= a b)
      (raise-types a b equal?))))

(extend-type Rational
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (+ (* (numer a) (denom b))
                    (* (numer b) (denom a)))
                 (* (denom a) (denom b))))
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (- (* (numer a) (denom b))
                   (* (numer b) (denom a)))
                (* (denom a) (denom b))))
       (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (* (numer a) (numer b))
                 (* (denom a) (denom b))))
      (raise-types a b mul))
  (div [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (* (numer a) (denom b))
                  (* (denom a) (numer b))))       
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (if (and (= (numer a) (numer b))
               (= (denom a) (denom b)))
        true)
      (raise-types a b equal?)))))

(extend-type Complex
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-real-imag (+ (real-part a) (real-part b))
                            (+ (imag-part a) (imag-part b))))
    (raise-types a b add)))
  (sub [a b]
   (if (= (class a) (class b))
      (reduce-type
       (make-from-real-imag (- (real-part a) (real-part b))
                            (- (imag-part a) (imag-part b))))
      (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-mag-ang (* (magnitude a) (magnitude b))
                          (+ (angle a) (angle b))))
      (raise-types a b mul)))
  (div [a b]
   (if (= (class a) (class b))
      (reduce-type
       (make-from-mag-ang (/ (magnitude a) (magnitude b))
                          (- (angle a) (angle b))))
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (if (and (= (real-part a) (real-part b))
               (= (imag-part a) (imag-part b)))
        true)
      (raise-types a b equal?))))

;test
(defn -main []
  (println
   (add
    (make Real 1)
    (make Real 1))))


;POLYNOMIALS

;constructors&selectors
(defn make-poly [variable term-list]
  (cons variable term-list))
(defn variable [p]
  (first p))
(defn term-list [p]
  (rest p))
(defn variable? [x]
  (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

;term-lists
(defn order [term]
  (first term))
(defn coeff [term]
  (fnext term))
(defn make-term [order coeff]
  (list order coeff))
(defn first-term [term-list]
  (first term-list))
(defn the-empty-termlist []
  '())
(defn rest-terms [term-list]
  (rest term-list))
(defn empty-termlist? [term-list]
  (nil? term-list))
(defn adjoin-term [term term-list]
  (if (zero? (coeff term))
    term-list
    (cons term term-list)))

;term-list constructors
(defn add-terms [l1 l2]
  (cond
    (empty-termlist? l1) l2
    (empty-termlist? l2) l1
    :else
    (let [t1 (first-term l1)
          t2 (first-term l2)]
      (cond
        (> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms l1) l2))
        (< (order t1) (order t2)) (adjoin-term t2 (add-terms l1 (rest-terms l2)))
        :else (adjoin-term (make-term (order t1)
                                      (add (coeff t1) (coeff t2)))
                           (add-terms (rest-terms l1)
                                      (rest-terms l2)))))))
(defn mul-term-by-all-terms [t1 L]
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let [t2 (first-term L)]
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))
(defn mul-terms [l1 l2]
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))


(extend-type Poly
  Algebra
  (add [a b]
    (if (same-variable? (variable a) (variable b))
      (make-poly (variable a)
                 (add-terms (term-list a)
                            (term-list b)))
      (println "ERROR: Polys not in same var -- ADD-POLY"
               (list a b))))
  (mul [a b]
    (if (same-variable? (variable a) (variable b))
      (make-poly (variable a)
                 (mul-terms (term-list a)
                            (term-list b)))
      (println "ERROR: Polys not in same var -- MUL-POLY"
             (list a b)))))
