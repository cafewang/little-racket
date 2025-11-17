#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x)))
)

(define multi-remove&col
  (lambda (x ls col)
    (cond
      ((null? ls) (col '() '()))
      ((eq? (car ls) x) (multi-remove&col x (cdr ls) (lambda (removed kept) (col (cons (car ls) removed) kept))))
      (else (multi-remove&col x (cdr ls) (lambda (removed kept) (col removed (cons (car ls) kept))))))))

(eq? #t (multi-remove&col 'a '(a b a c a) (lambda (removed kept) (equal? kept '(b c)))))
(multi-remove&col 'a '(a b a c a) (lambda (removed kept) kept))