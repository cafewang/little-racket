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

; x should be an atom
; ls should be a list
(define occur*
  (lambda (x ls)
    (cond
      ((null? ls) 0)
      ((atom? (car ls))
        (+
          (cond 
            ((eq? x (car ls)) 1)
            (else 0))
          (occur* x (cdr ls))))
      (else (+ (occur* x (car ls)) (occur* x (cdr ls)))))))

(eq? (occur* 'x '(a x b x c)) 2)
(eq? (occur* 'x '(a (x) b x c)) 2)
(eq? (occur* 'x '(a (b c))) 0)

(define subst*
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((atom? (car ls))
        (cond
          ((eq? old (car ls)) (cons new (subst* new old (cdr ls))))
          (else (cons (car ls) (subst* new old (cdr ls))))))
      (else (cons (subst* new old (car ls)) (subst* new old (cdr ls)))))))

(equal? (subst* 'x 'a '(a b a (c a) a)) '(x b x (c x) x))
(equal? (subst* 'x 'a '(a (b c) a (d (e f) a) a)) '(x (b c) x (d (e f) x) x))
(equal? (subst* 'x 'g '(a (b c) a (d (e f) a) a)) '(a (b c) a (d (e f) a) a))


(define eqlist?
  (lambda (ls1 ls2)
    (cond
      ((and (null? ls1) (null? ls2)) #t)
      ((or (null? ls1) (null? ls2)) #f)
      ((and (atom? (car ls1)) (atom? (car ls2)))
        (and (eq? (car ls1) (car ls2)) (eqlist? (cdr ls1) (cdr ls2))))
      ((or (atom? (car ls1)) (atom? (car ls2))) #f)
      (else (and (eqlist? (car ls1) (car ls2)) (eqlist? (cdr ls1) (cdr ls2)))))))

(eqlist? '(a (b c) a (d (e f) a) a) '(a (b c) a (d (e f) a) a))
(eqlist? '(a (b c)) '(a (b c)))

(define eqexp?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
        (eq? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(eqexp? '(a (b c) a (d (e f) a) a) '(a (b c) a (d (e f) a) a))
(eqexp? 'a 'a)

(define remove
  (lambda (exp1 exp2)
    (cond
      ((null? exp2) '())
      ((eqexp? exp1 (car exp2)) (remove exp1 (cdr exp2)))
      (else (cons (car exp2) (remove exp1 (cdr exp2)))))))

(eqexp? (remove '(c a) '(a b a (c a) a)) '(a b a a))
(eqexp? (remove 'a '(a (b c) a (d (e f) a) a)) '((b c) (d (e f) a)))

(define remove*
  (lambda (exp1 exp2)
    (cond
      ((null? exp2) '())
      ((eqexp? exp1 (car exp2)) (remove* exp1 (cdr exp2)))
      ((or (null? (car exp2)) (atom? (car exp2))) (cons (car exp2) (remove* exp1 (cdr exp2))))
      (else (cons (remove* exp1 (car exp2)) (remove* exp1 (cdr exp2)))))))

(eqexp? (remove* '(c a) '(a b a (c a) a)) '(a b a a))
(eqexp? (remove* 'a '(a (b c) a (d (e f) a) a)) '((b c) (d (e f))))
(eqexp? (remove* '(a b) '(a (b c) (a b) (d (e f) (a b)) a)) '(a (b c) (d (e f)) a))
(eqexp? (remove* '() '(a () (() b c) (d (()e f)))) '(a (b c) (d (e f))))
