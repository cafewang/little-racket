#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x)))
)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define lat2?
  (lambda (l)
    (or
      (null? l)
      (and (atom? (car l)) (lat? (cdr l)))
    )))

(eq? #t (lat? '()))
(eq? #t (lat? '(a b c)))
(eq? #f (lat? '(a (b) c)))

(eq? #t (lat2? '()))
(eq? #t (lat2? '(a b c)))
(eq? #f (lat2? '(a (b) c)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(equal? #t (member? 'a '(a b c)))
(equal? #f (member? 'd '(a b c)))

(define member2?
  (lambda (a lat)
    (and
      (not (null? lat))
      (or
        (eq? a (car lat))
        (member2? a (cdr lat))))))

(eq? #t (member2? 'a '(a b c)))
(eq? #f (member2? 'd '(a b c)))

(define all-of?
  (lambda (a lat)
    (or
      (null? lat)
      (and
        (eq? a (car lat))
        (all-of? a (cdr lat))))))

(eq? #t (all-of? 'a '()))
(eq? #t (all-of? 'a '(a a a)))
(eq? #f (all-of? 'a '(a b a)))
