#lang racket

(define multi-remove
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      ((eq? a (car lat)) (multi-remove a (cdr lat)))
      (else (cons (car lat) (multi-remove a (cdr lat)))))))

(equal? (multi-remove 'a '(a b a c a)) '(b c))
(equal? (multi-remove 'd '(a b c)) '(a b c))

(define multi-insertR
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multi-insertR new old (cdr lat)))))
      (else (cons (car lat) (multi-insertR new old (cdr lat)))))))

(equal? (multi-insertR 'd 'a '(b c d)) '(b c d))
(equal? (multi-insertR 'd 'a '(a b c)) '(a d b c))
(equal? (multi-insertR 'd 'b '(a b b)) '(a b d b d))

