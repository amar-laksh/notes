#lang racket

(provide square
         cube
         head
         tail
         )

(define head car)
(define (tail x) (head (cdr x)))
(define rest cdr)
(define replicate (λ (n x) (if (= n 0) '() (cons x (replicate (- n 1) x)))))


(define (square x) (* x x))
(define (cube x) (* (* x x) x))
