(ns symbolic-algebra.core
  (gen-class
   :name Number
   :extends java.lang.Number
   :constructors {[ ][ ]}))

(defn -Number [this] (num this))

;TYPES
(defrecord Rational [numerator denominator])
(defrecord Complex [real imaginary])
(defrecord Poly [variable term-list])

;PROTOCOLS
(defprotocol Algebra
  (add [a b])
  (sub [a b])
  (mul [a b])
  (div [a b])
  (equal? [a b]))

;Maths
(defn round [n]
  (if (>= n 0)
    (Math/floor n)
    (Math/ceil n)))
;; (defn rem [n d]
;;   (let [q (round (div n d))]
;;     (sub n (mul d q))))
(defn mod [num div] 
  (let [m (rem num div)] 
    (if (or (= m 0) (= (pos? num) (pos? div)))
      m 
      (add m div))))
(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))
(defn square [x]
  (mul x x))

;RATIONALS
(defn numer [x]
  (get x :numerator))
(defn denom [x]
  (get x :denominator))
(defn make-rat [n d]
  (let [g (gcd n d)]
    (Rational. (div n g) (div d g))))

;COMPLEX
(defn real-part [rect]
  (get rect :real))
(defn imag-part [rect]
  (get rect :imaginary))
(defn magnitude [polar]
  (Math/sqrt (add (square (real-part polar))
                  (square (imag-part polar)))))
(defn angle [polar]
  (Math/atan2 (imag-part polar)
              (real-part polar)))
(defn make-from-real-imag [x y]
  (Complex. x y))
(defn make-from-mag-ang [r a]
  (Complex. (round (mul r (Math/cos a)))
            (round (mul r (Math/sin a)))))

;TYPE COERCION
(defn raise-types [a b proc]
  (let [class-a (class a)
        class-b (class b)]
    (cond
      (and (number? a)          (= class-b Rational))  (proc (Rational. a 1) b)
      (and (number? a)          (= class-b Complex))   (proc (Complex. a 0) b)
      (and (= class-a Rational) (number? b))           (proc a (Rational. b 1)) 
      (and (= class-a Rational) (= class-b Complex))   (proc (Complex. a 0) b)
      (and (= class-a Complex)  (number? b))           (proc a (Complex. b 0))
      (and (= class-a Complex)  (= class-b Rational))  (proc a (Complex. b 0)))))

(defn reduce-type [a]
  (let [class-a (class a)]
    (cond
      (and (= class-a Complex) (= (imag-part a) 0))  (if (= (class (real-part a)) Rational)                                                      
                                                       (reduce-type (make-rat (real-part a)))                                                               
                                                       (-Number (real-part a)))
      (and (= class-a Rational) (= (denom a) 1))  (-Number (numer a))
      :else a)))

(extend-type Number
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
       (make-rat (add (mul (numer a) (denom b))
                      (mul (numer b) (denom a)))
                 (mul (denom a) (denom b))))
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (sub (mul (numer a) (denom b))
                      (mul (numer b) (denom a)))
                 (mul (denom a) (denom b))))
       (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (mul (numer a) (numer b))
                 (mul (denom a) (denom b))))
      (raise-types a b mul)))
  (div [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-rat (mul (numer a) (denom b))
                 (mul (denom a) (numer b))))       
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (if (and (= (numer a) (numer b))
               (= (denom a) (denom b)))
        true)
      (raise-types a b equal?))))

(extend-type Complex
  Algebra
  (add [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-real-imag (add (real-part a) (real-part b))
                            (add (imag-part a) (imag-part b))))
    (raise-types a b add)))
  (sub [a b]
   (if (= (class a) (class b))
      (reduce-type
       (make-from-real-imag (sub (real-part a) (real-part b))
                            (sub (imag-part a) (imag-part b))))
      (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (make-from-mag-ang (mul (magnitude a) (magnitude b))
                          (add (angle a) (angle b))))
      (raise-types a b mul)))
  (div [a b]
   (if (= (class a) (class b))
      (reduce-type
       (make-from-mag-ang (div (magnitude a) (magnitude b))
                          (sub (angle a) (angle b))))
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (if (and (= (real-part a) (real-part b))
               (= (imag-part a) (imag-part b)))
        true)
      (raise-types a b equal?))))

;POLYNOMIALS
(defn variable [p]
  (get p :variable))
(defn term-list [p]
  (get p :term-list))
(defn order [term]
  (first term))
(defn coeff [term]
  (fnext term))
(defn make-term [order coeff]
  (list order coeff))
(defn first-term [term-list]
  (first term-list))
(defn rest-terms [term-list]
  (rest term-list))
(defn adjoin-term [term term-list]
  (if (zero? (coeff term))
    term-list
    (cons term term-list)))
(defn negate-terms [termlist] 
  (map  
   (fn [t]
     (make-term (order t) 
                (- (coeff t)))) 
   termlist))

(defn add-terms [l1 l2]
  (cond
    (empty? l1) l2
    (empty? l2) l1
    :else
    (let [t1 (first-term l1)
          t2 (first-term l2)]
      (cond
        (> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms l1) l2))
        (< (order t1) (order t2)) (adjoin-term t2 (add-terms l1 (rest-terms l2)))
        :else 
        (adjoin-term (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms l1)
                                (rest-terms l2)))))))

(defn mul-term-by-all-terms [t1 l]
  (if (empty? l)
      l
      (let [t2 (first-term l)]
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms l))))))
(defn mul-terms [l1 l2]
  (if (empty? l1)
      l1
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))

(defn div-terms [l1 l2]
  (if (empty? l1)
      (list () ())
      (let [t1 (first-term l1)
            t2 (first-term l2)]
        (if (> (order t2) (order t1))
            (list () l1)
            (let [new-c (div (coeff t1) (coeff t2))
                  new-o (- (order t1) (order t2))]
              (let [rest-of-result
                    (div-terms
                     (add-terms l1
                                (negate-terms
                                 (mul-terms l2
                                            (list
                                             (make-term new-o new-c)))))
                     l2)]
                (list (adjoin-term (make-term new-o new-c) 
                                   (first rest-of-result)) 
                      (fnext rest-of-result))))))))

(extend-type Poly
  Algebra
  (add [a b]
    (if (= (variable a) (variable b))
      (Poly. (variable a)
             (add-terms (term-list a)
                        (term-list b)))
      (println "ERROR: Polys not in same var -- ADD-POlY"
               (list a b))))
  (sub [a b]
    (if (= (variable a) (variable b))
      (Poly. (variable a)
             (add-terms (term-list a)
                        (negate-terms (term-list b))))
      (println "ERROR: Polys not in same var -- SUB-POlY"
               (list a b))))
  (mul [a b]
    (if (= (variable a) (variable b))
      (Poly. (variable a)
             (mul-terms (term-list a)
                        (term-list b)))
      (println "ERROR: Polys not in same var -- MUl-POlY"
               (list a b))))
  (div [a b]
    (if (= (variable a) (variable b))
      (let [result (div-terms (term-list a) 
                              (term-list b))]
        (Poly. (variable a) (list (first result)
                                  (fnext result))))
      (println "ERROR: Polys not in same var -- DIV-POlY"
               (list a b))))
  (equal? [a b]
    (if (and (= (variable a) (variable b))
             (= (term-list a) (term-list b)))
      true
      false)))


;; (defn -main []
;;   "division is buggy...good reason to implement rational polys"
;;   (println
;;    (div
;;     (Poly. 'x '((3 2) (2 2)))
;;     (Poly. 'x '((3 1) (2 2))))))

;; (defn -main []
;;   "subtyping still buggy due to redefining math functions for java.lang.Number"
;;   (println
;;    (add
;;     (Rational. (Complex. 1 2) (Complex. 1 4))
;;     (Rational. (Complex. 1 2) (Complex. 1 4)))))

(defn -main []
  )
