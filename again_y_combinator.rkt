#lang racket

; start from here, it's length function
(((lambda (apply-self) (apply-self apply-self))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((length length) (cdr l)))))))) '(a b c d e))

; same same
(((lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((length length) (cdr l)))))))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((length length) (cdr l)))))))
) '(a b c d e))

; still the same
((lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (((lambda (length)
                    (lambda (l)
                      (cond
                        ((null? l) 0)
                        (else (add1 ((length length) (cdr l))))))) (lambda (length)
                                                                    (lambda (l)
                                                                      (cond
                                                                        ((null? l) 0)
                                                                        (else (add1 ((length length) (cdr l)))))))) (cdr l))))))
'(a b c d e))

; the above one is the final form of function length, it expands to itself again and again
; then we want to extract the recursive part, so we could make other recursive functions
; let's start afresh
(((lambda (apply-self) (apply-self apply-self))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((length length) (cdr l)))))))) '(a b c d e))

; we extract (length length) part out of lambda (l)
; (((lambda (apply-self) (apply-self apply-self))
;   (lambda (mklength)
;     ((lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l))))))) (mklength mklength)))) '(a b c d e))

; the above function compiles but never stop, why?
; ((lambda (mklength)
;     ((lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l))))))) (mklength mklength)))
;   (lambda (mklength)
;     ((lambda (length)
;       (lambda (l)
;         (cond
;           ((null? l) 0)
;           (else (add1 (length (cdr l))))))) (mklength mklength)))
; )

; we can see that while the application goes, the expression expands longer and longer, it just don't stop
; ((lambda (length)
;   (lambda (l)
;     (cond
;       ((null? l) 0)
;       (else (add1 (length (cdr l))))))) ((lambda (mklength)
;                                           ((lambda (length)
;                                             (lambda (l)
;                                               (cond
;                                                 ((null? l) 0)
;                                                 (else (add1 (length (cdr l))))))) (mklength mklength)))
;                                           (lambda (mklength)
;                                             ((lambda (length)
;                                               (lambda (l)
;                                                 (cond
;                                                   ((null? l) 0)
;                                                   (else (add1 (length (cdr l))))))) (mklength mklength)))))

; so we turn (mklength mklength) into (lambda (x) ((mklength mklength) x)), which does not trigger expansion
(((lambda (apply-self) (apply-self apply-self))
  (lambda (mklength)
    ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l))))))) (lambda (x) ((mklength mklength) x))))) '(a b c d e))

; yes, it did work!
; then we simplify the symbols a little bit
(((lambda (f) (f f))
  (lambda (f)
    ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l))))))) (lambda (l) ((f f) l))))) '(a b c d e))

; we try to extract lambda (length) out, so we could make it a parameter to build other recusive functions
; rp means recursive part
(((lambda (rp) 
  ((lambda (f) (f f))
    (lambda (f)
      (rp (lambda (l) ((f f) l))))))
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))) '(a b c d e))

; let's try gcd, it works fine!
(((lambda (rp) 
    ((lambda (f) (f f))
      (lambda (f)
        (rp (lambda (l) ((f f) l))))))
  (lambda (gcd)
    (lambda (pair)
      (cond
        ((zero? (cdr pair)) pair)
        (else (gcd (cons (cdr pair) (modulo (car pair) (cdr pair))))))))) '(30 . 40))
