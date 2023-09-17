#lang racket

(provide square 
         cube 
         head 
         tail)

(define head car)
(define (tail x) (head (cdr x)))


(define (square x) (* x x))
(define (cube x) (* (* x x) x))



