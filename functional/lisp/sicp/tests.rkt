#lang racket

(require rackunit)
(require "chap1.rkt")


(check-equal? (sum-of-squares-largest-two 100 2 13) 10169)
(check-equal? (sum-of-squares-largest-two 100 2 13) 10169)
(check-equal? (round (newton-sqrt 121)) 11.0)
(check-equal? (round (newton-cubert 27)) 3.1)
