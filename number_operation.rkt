#lang racket

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define plus
  (lambda (x y)
    (cond ((zero? y) x)
    (else (plus (add1 x) (sub1 y))))))

(eq? 10 (plus 10 0))
(eq? 10 (plus 3 7))

(define minus
  (lambda (x y)
    (cond ((zero? y) x)
      (else (minus (sub1 x) (sub1 y))))))

(eq? 3 (minus 10 7))
(eq? 0 (minus 10 10))
(eq? 10 (minus 10 0))

(define tup?
  (lambda (x)
    (cond ((null? x) #t)
      (else 
        (and
          (pair? x)
          (number? (car x)) 
          (tup? (cdr x)))))))

(eq? #t (tup? '(1 2 3)))
(eq? #f (tup? '(1 a 3)))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
      (else
        (plus (car tup) (addtup (cdr tup))))))
)

(eq? 6 (addtup '(1 2 3)))
(eq? 0 (addtup '()))

(define multipy
  (lambda (x y)
    (cond ((zero? y) 0)
      (else
        (plus x (multipy x (sub1 y)))))))

(eq? 6 (multipy 2 3))
(eq? 0 (multipy 2 0))

(define tup-plus
  (lambda (tup1 tup2)
    (cond 
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2)) (tup-plus (cdr tup1) (cdr tup2)))))))

(equal? '(3 5 7) (tup-plus '(1 2 3) '(2 3 4)))
(equal? '(6 2 3) (tup-plus '(1 2 3) '(5)))