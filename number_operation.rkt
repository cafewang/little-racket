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

;works for non-negative numbers only
(define > 
  (lambda (x y)
    (cond 
      ((zero? x) #f)
      ((zero? y) #t)
      (else (> (sub1 x) (sub1 y))))))

(eq? #f (> 5 5))
(eq? #t (> 5 4))
(eq? #f (> 4 5))

(define divide
  (lambda (x y)
    (cond
      ((< x y) 0)
      (else (add1 (divide (- x y) y))))))

(eq? 2 (divide 10 5))
(eq? 0 (divide 4 5))
(eq? 1 (divide 7 5))

(define divide-and-remainder
  (lambda (x y)
    (cond
      ((< x y) (list 0 x))
      (else (cons (add1 (car (divide-and-remainder (- x y) y))) (cdr (divide-and-remainder (- x y) y)))))))

(equal? '(3 2) (divide-and-remainder 17 5))
(equal? '(1 0) (divide-and-remainder 17 17))
(equal? '(0 2) (divide-and-remainder 2 5))

(define length
  (lambda (x)
  (cond
    ((null? x) 0)
    (else (add1 (length (cdr x)))))))

(eq? 3 (length '(1 2 3)))
(eq? 0 (length '()))

(define pick
  (lambda (n lat)
    (cond
      ((eq? 1 n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(eq? 2 (pick 2 '(1 2 3 4)))
(eq? 1 (pick 1 '(1 2 3 4)))