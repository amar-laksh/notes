#lang racket

(require "utils.rkt")

(provide sum-of-squares-largest-two
         newton-sqrt
         newton-cubert)

;; Exercise 1.3
(define (greaterThanBoth? x y z) (if (and (>= x y) (>= x z)) true false ))
(define (max x y z) (if (greaterThanBoth? x y z) x (if (greaterThanBoth? y x z) y z)))
(define (is-n-max? x y z n) ( if (= (max x y z) n) true false))
(define (max-two x y z) (if (is-n-max? x y z x) (list x (max y y z))
                            (if (is-n-max? x y z y) (list y (max x x z)) (list z (max x y y)))))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sum-of-squares-largest-two x y z)
  (sum-of-squares  (head (max-two x y z)) (tail (max-two x y z))))


(define (good-enough? guess x f) (< (abs (- (f guess) x) ) 0.0001))
(define (average x y) (/ (+ x y) 2) )
(define (improveSqrt guess x) (average guess (/ x guess)))
(define (sqrt-iter guess x) ( if (good-enough? guess x square) guess (sqrt-iter (improveSqrt guess x) x)))
(define (newton-sqrt x) (sqrt-iter 1.0 x))

;; Exercise 1.7

;; Exercise 1.8
(define (improveCubert guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cubert-iter guess x) ( if (good-enough? guess x cube) guess (cubert-iter (improveCubert guess x) x)))
(define (newton-cubert x) (cubert-iter 1.0 x))

;; Exercise 1.11
(define (f_n n)  ( if (< n 3) n  (f_n (- n 1) ) ))
