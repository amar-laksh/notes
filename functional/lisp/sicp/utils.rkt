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

(define make-tag (λ (tag child) ( list  (list '< tag '>) child (list '< '/ tag '>)) ))
(define get-tag (λ (tag) (tail (head tag))))
(define get-child (λ (tag) (tail tag)))
(define get-child-by-name (λ (tag name) (if (null? tag) '() (if (eq? (get-tag tag) name) (get-child tag) (get-child-by-name (get-child tag) name) ))))
(get-child-by-name (make-tag 'html (make-tag 'body (make-tag 'p "hello world"))) 'p )

