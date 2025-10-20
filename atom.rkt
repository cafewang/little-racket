#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x)))
)

(eq? true (atom? 'a))
(eq? true (atom? 123))
(eq? false (atom? '()))
(eq? false (atom? '(('a) 1)))

(equal? '(1 2 3) (car '((1 2 3) 1)))
(equal? 1 (car '(1 (2 3) 1)))

(equal? '(1) (cdr '((1 2 3) 1)))
(equal? '((2 3)) (cdr '(1 (2 3))))

(equal? '(a b) (cons 'a '(b)))
(equal? '((a b) c) (cons '(a b) '(c)))

(eq? 'a (car '(a b)))
(eq? (car '(wtf wtf)) (car (cdr '(wtf wtf))))