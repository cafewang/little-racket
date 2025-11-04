#lang racket

(define (atom? x)
  (and (not (pair? x)) (not (null? x)))
)

; x should be an atom
; ls should be a list
(define remove-all*
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((atom? (car ls))
        (cond 
          ((eq? x (car ls)) (remove-all* x (cdr ls)))
          (else (cons (car ls) (remove-all* x (cdr ls))))))
      (else (cons (remove-all* x (car ls)) (remove-all* x (cdr ls)))))))

(equal? (remove-all* 'x '(a x b x c)) '(a b c))
(equal? (remove-all* 'x '(a (x) b x c)) '(a () b c))
(equal? (remove-all* 'x '(a (b c))) '(a (b c)))
