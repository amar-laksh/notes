#lang racket
(provide sum-of-squares-largest-two)
(provide newton-sqrt)

(define head car)
(define (tail x) (head (cdr x)))
(define (greaterThanBoth? x y z) (if (and (>= x y) (>= x z)) true false ))
(define (max x y z) (if (greaterThanBoth? x y z) x (if (greaterThanBoth? y x z) y z)))
(define (is-n-max? x y z n) ( if (= (max x y z) n) true false))
(define (max-two x y z) (
                        if (is-n-max? x y z x) (list x (max y y z))
                        (if (is-n-max? x y z y) (list y (max x x z)) (list z (max x y y)))))
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sum-of-squares-largest-two x y z)
    (sum-of-squares  (head (max-two x y z)) (tail (max-two x y z))

))

(define (goodenough? guessX x) (< (abs (- (square guessX) x) ) 0.0001))
(define (average x y) (/ (+ x y) 2) )
(define (guess? x y) (average y (/ x y)))
(define (sqrt-iter x guess) ( if (goodenough? (guess? x guess) x) (guess? x guess) (sqrt-iter x (guess? x guess))))
(define (newton-sqrt x) (sqrt-iter x 1.0))
