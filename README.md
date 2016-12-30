# Symbolic-Algebra.clj

A library for performing algebraic operations across multiple numeric types, inspired by the exercise presented in [Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/) as well as [Richard Zippel’s Weyl package for Common Lisp](http://www.cs.cornell.edu/rz/computer-algebra.html). 

Currently this implements **rationals**, **complex numbers**, and univariate **polynomials** as well as any combination thereof. Clojure's protocols are used for single dispatch along with functions for type coercion. The base type extends the java.lang.Number abstract class in order to support all JVM numeric types, everything from shorts to doubles to BigIntegers. Finally, reader macros are used for legibility.

The latest update uses [Stein's Algorithm (Binary GCD)](https://en.wikipedia.org/wiki/Binary_GCD_algorithm) instead of Euclid's to factor rationals,  adds functions to convert between polynomials with dense and sparse term lists, and comes with a suite of generative tests written using the new [clojure.spec](http://clojure.org/about/spec) library—and therefore now requires Clojure 1.9.0-alpha14.

##Usage:

```
;; 5+1/2i + 2+1/4i = 7+3/4i
=> (add (Complex. 5 (Rational. 1 2)) (Complex. 2 (Rational. 1 4)))
=> 10+3/4i
```
```
;; 1/2 - 1/3 = 1/6
=> (sub (Rational. 1 2) (Rational. 1 3))
=> 1/6
```
```
;; 5+2i * 1+1i = 3+7i
=> (mul (Complex. 5 2) (Complex. 1 1))
=> 3+7i
```
```
;; (x^5 + 3)/(x^5 + 3) = 1
=> (div (Poly. 'x '((5 1) (0 3))) (Poly. 'x '((5 1) (0 3))))
=> x:((0 1))
```
```
;; 3/9 == 9/27
=> (equal? (Rational. 3 9) (Rational. 9 27))
=> true
```
```
;;polynomials must be in dense form to operate on
;; y^5 + 2y^4 + 3y^2 - 2y - 5
=> (sparse-to-dense (Poly. 'y '(1 2 0 3 -2 -5)))
=> y:((5 1) (4 2) (2 3) (1 -2) (0 -5))
=> (dense-to-sparse (Poly. 'y '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
=> y:(1 2 0 3 -2 -5)
```

##Known Issues and Future Features

Subtyping is still not perfect (thanks core.spec!) as it requires extending GCD over a Euclidean domain in order to factor rationals and polynomials. In addition, the next version will allow multivariate polynomials for use with my [Power Series](https://github.com/Sophia-Gold/power-series.clj) package.