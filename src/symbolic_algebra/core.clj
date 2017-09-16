(ns symbolic-algebra.core
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.spec.gen :as gen]))

;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

;TYPES
(defrecord Rational [^long numerator ^long denominator])
(defmethod print-method Rational [v ^java.io.Writer w]
  (print-method (:numerator v) w)
  (.write w "/")
  (print-method (:denominator v) w))
(defrecord Complex [^long real ^long imaginary])
(defmethod print-method Complex [v ^java.io.Writer w]
  (print-method (:real v) w)
  (.write w "+")
  (print-method (:imaginary v) w)
  (.write w "i"))
(defrecord Poly [variable term-list])
(defmethod print-method Poly [v ^java.io.Writer w]
  (print-method (:variable v) w)
  (.write w ":")
  (print-method (:term-list v) w))

;PROTOCOLS
(defprotocol Algebra
  (add [a b])
  (sub [a b])
  (mul [a b])
  (div [a b])
  (equal? [a b]))

;Maths
(defn round [n]
  (if (>= n 0.0)
    (Math/floor n)
    (Math/ceil n)))
(defn square [x]
  (mul x x))

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
  (Complex. (int (round (mul r (Math/cos a))))
            (int (round (mul r (Math/sin a))))))

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
(defn first-term-dense [term-list] 
  (concat
   (first term-list)
   (- (count term-list) 1)))
(defn rest-terms [term-list]
  (next term-list))
(defn adjoin-term [term term-list]
  (cons term term-list))
(defn adjoin-term-dense [term term-list]
  (if (zero? (coeff term))
    term-list
    (cons (coeff term) term-list)))
(defn sparse-to-dense [p]
  (let [poly (reverse (term-list p))
        diff-terms (map #(vector (dec (- (first %1) (first %2))) (second %1))
                        (next poly) poly)]
    (->> diff-terms
         (cons (first poly))
         (mapcat #(concat (repeat (first %) 0) [(second %)]))
         (reverse)
         (#(Poly. (variable p) %))))) 
(defn dense-to-sparse [p]
  (->> (term-list p)
       (reverse)
       (map-indexed #(if (not= %2 0) (list %1 %2)))
       (filter some?)
       (reverse)
       (#(Poly. (variable p) %))))
 
;RATIONALS
(defn numer [x]
  (get x :numerator))
(defn denom [x]
  (get x :denominator))

;; (defn gcd [a b]
;;   (if (zero? b)
;;     a
;;     (recur b (mod a b))))
(defn gcd [a b]
  (cond
    (zero? a) b
    (zero? b) a
    (neg? a) (- a)
    (neg? b) (- b)
    (and (even? a) (even? b)) (* 2
                                 (gcd (unsigned-bit-shift-right a 1)
                                      (unsigned-bit-shift-right b 1)))
    (and (even? a) (odd? b)) (recur (unsigned-bit-shift-right a 1) b)
    (and (odd? a) (even? b)) (recur a (unsigned-bit-shift-right b 1))
    (and (odd? a) (odd? b)) (recur (unsigned-bit-shift-right
                                    (Math/abs (long (- a b))) ;; coerce to avoid reflection
                                    1) (min a b))))
(defn extended-gcd [a b]
  (let [class-a (class a)]
    (cond
      (number? a)        (gcd a b)
      (= class-a Rational) (reduce gcd (list (numer a) (denom a) (numer b) (denom b)))
      (= class-a Complex)  (reduce gcd (list (real-part a) (imag-part a) (real-part b) (imag-part b)))
      (= class-a Poly)     (reduce gcd (concat (term-list a) (term-list b))))))

(defn make-rat [n d]
  (let [g (extended-gcd n d)]
    (Rational. (div n g) (div d g))))

;TYPE COERCION
(defn raise-types [a b proc]
  (let [class-a (class a)
        class-b (class b)]
    (cond
      (and (number? a)          (= class-b Rational))  (proc (Rational. a 1) b)
      (and (number? a)          (= class-b Complex))   (proc (Complex. a 0) b)
      (and (number? a)          (= class-b Poly))      (proc (Poly. (variable b) (list (list 0 a))) b)
      (and (= class-a Rational) (number? b))           (proc a (Rational. b 1)) 
      (and (= class-a Rational) (= class-b Complex))   (proc (Complex. a 0) b)
      (and (= class-a Rational) (= class-b Poly))      (proc (Poly. (variable b) (list (list 0 a))) b)
      (and (= class-a Complex)  (number? b))           (proc a (Complex. b 0))
      (and (= class-a Complex)  (= class-b Rational))  (proc a (Complex. b 0))
      (and (= class-a Complex)  (= class-b Poly))      (proc (Poly. (variable b) (list (list 0 a))) b)
      (and (= class-a Poly)     (number? b))           (proc a (Poly. (variable a) (list (list 0 b))))
      (and (= class-a Poly)     (= class-b Rational))  (proc a (Poly. (variable a) (list (list 0 b))))
      (and (= class-a Poly)     (= class-b Complex))   (proc a (Poly. (variable a) (list (list 0 b)))))))

(defn reduce-type [a]
  (let [class-a (class a)]
    (cond
      (and (= class-a Rational)
           (= (denom a) 1))                          (numer a)
      (and (= class-a Complex)
           (= (imag-part a) 0))                      (reduce-type (real-part a))
      (and (= class-a Poly)
           (empty (rest-terms (term-list a)))
           (= (order (first-term (term-list a))) 0)) (reduce-type (coeff (first-term (term-list a))))
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
      (let [simple-a (make-rat (numer a) (denom a))
            simple-b (make-rat (numer b) (denom b))]
        (if (and (equal? (numer simple-a) (numer simple-b))
                 (equal? (denom simple-a) (denom simple-b)))
          true
          false))
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
      (if (and (equal? (real-part a) (real-part b))
               (equal? (imag-part a) (imag-part b)))
        true
        false)
      (raise-types a b equal?))))

(defn negate-terms [termlist] 
  (map  
   (fn [t]
     (make-term (order t) 
                (sub 0 (coeff t)))) 
   termlist))
(defn add-terms [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) '()
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
    l1
    (let [t1 (first-term l1)
          t2 (first-term l2)]
      (if (> (order t2) (order t1))
        l1
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
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (Poly. (variable a)
                (add-terms (term-list a)
                           (term-list b)))
         (println "ERROR: Polys not in same var -- ADD-POLY"
                  (list a b))))
      (raise-types a b add)))
  (sub [a b]
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (Poly. (variable a)
                (add-terms (term-list a)
                           (negate-terms (term-list b))))
         (println "ERROR: Polys not in same var -- SUB-POLY"
                  (list a b))))
      (raise-types a b sub)))
  (mul [a b]
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (Poly. (variable a)
                (mul-terms (term-list a)
                           (term-list b)))
         (println "ERROR: Polys not in same var -- MUL-POLY"
                  (list a b))))
      (raise-types a b mul)))
  (div [a b]
    (if (= (class a) (class b))
      (reduce-type
       (if (= (variable a) (variable b))
         (let [result (div-terms (term-list a) 
                                 (term-list b))]
           (Poly. (variable a) (first result)))
         (println "ERROR: Polys not in same var -- DIV-POLY"
                  (list a b))))
      (raise-types a b div)))
  (equal? [a b]
    (if (= (class a) (class b))
      (reduce-type
       (let [sparse-a (dense-to-sparse a)
             sparse-b (dense-to-sparse b)]
         (if (= (variable sparse-a) (variable sparse-b))
           (not-any? false? 
                     (map equal? (term-list sparse-a) (term-list sparse-b)))
           false)))
      (raise-types a b equal?))))


(defn -main []
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(def non-zero-int
  (gen/such-that (complement zero?)
                 (s/gen int?)))

(s/def ::rational
  (s/with-gen #(instance? Rational %)
              (fn [] (gen/fmap #(->Rational (first %) (second %))
                               (gen/tuple
                                non-zero-int
                                non-zero-int)))))
(s/def ::rational-rational
  (s/with-gen #(instance? Rational %)
              (fn [] (gen/fmap #(->Rational (first %) (second %))
                               (gen/tuple
                                (s/gen ::rational)
                                (s/gen ::rational))))))
(s/def ::rational-complex-numer
  (s/with-gen #(instance? Rational %)
              (fn [] (gen/fmap #(->Rational (first %) (second %))
                               (gen/tuple
                                (s/gen ::complex)
                                (s/gen int?))))))
(s/def ::rational-complex-denom
  (s/with-gen #(instance? Rational %)
              (fn [] (gen/fmap #(->Rational (first %) (second %))
                               (gen/tuple
                                (s/gen int?)
                                (s/gen ::complex))))))
(s/def ::rational-complex
  (s/with-gen #(instance? Rational %)
              (fn [] (gen/fmap #(->Rational (first %) (second %))
                               (gen/tuple
                                (s/gen ::complex)
                                (s/gen ::complex))))))
(s/def ::complex
  (s/with-gen #(instance? Complex %)
              (fn [] (gen/fmap #(->Complex (first %) (second %))
                               (gen/tuple
                                (s/gen int?)
                                (s/gen int?))))))
(s/def ::complex-rational-real
  (s/with-gen #(instance? Complex %)
              (fn [] (gen/fmap #(->Complex (first %) (second %))
                               (gen/tuple
                                (s/gen ::rational)
                                non-zero-int)))))
(s/def ::complex-rational-imag
  (s/with-gen #(instance? Complex %)
              (fn [] (gen/fmap #(->Complex (first %) (second %))
                               (gen/tuple
                                (s/gen int?)
                                (s/gen ::rational))))))
(s/def ::complex-rational
  (s/with-gen #(instance? Complex %)
              (fn [] (gen/fmap #(->Complex (first %) (second %))
                               (gen/tuple
                                (s/gen ::rational)
                                (s/gen ::rational))))))

(s/def ::mono (s/or ;; :int int?
                    :rational ::rational
                    :rational-:rational ::rational-rational
                    ;; :rational-complex-numer ::rational-complex-numer
                    ;; :rational-complex-denom ::rational-complex-denom
                    ;; :rational-complex ::rational-complex
                    :complex ::complex
                    ;; :complex-rational ::complex-rational-real
                    ;; :complex-rational-imag ::complex-rational-imag
                    ;; :complex-rational ::complex-rational
                    ))

(s/def ::poly
  (s/with-gen #(instance? Poly %)
    (fn [] (gen/fmap #(->Poly 'x (list
                                  (list 5 (nth % 0))
                                  (list 4 (nth % 1))
                                  (list 3 (nth % 2))
                                  (list 0 (nth % 3))))
                     (gen/tuple
                      (s/gen ::mono)
                      (s/gen ::mono)
                      (s/gen ::mono)
                      (s/gen ::mono))))))

(s/def ::all (s/or :mono ::mono
                   ;; :poly ::poly
                   ))

(s/fdef add
        :args (s/cat :a ::all :b ::all)
        :ret any?)
(s/fdef sub
        :args (s/cat :a ::all :b ::all)
        :ret any?)
(s/fdef mul
        :args (s/cat :a ::all :b ::all)
        :ret any?)
(s/fdef div
        :args (s/cat :a ::all :b ::all)
        :ret any?)
(s/fdef equal?
        :args (s/cat :a ::all :b ::all)
        :ret boolean?)

;; (stest/summarize-results (stest/check `add))
;; (s/exercise-fn `add)

;; (stest/summarize-results (stest/check `sub))
;; (s/exercise-fn `sub)

;; (stest/summarize-results (stest/check `mul))
;; (s/exercise-fn `mul)

;; (stest/summarize-results (stest/check `div))
;; (s/exercise-fn `div)

;; (stest/summarize-results (stest/check `equal?))
;; (s/exercise-fn `equal?)
