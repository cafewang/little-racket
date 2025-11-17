#lang racket

; apply '() to length0
((lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ('whatever (cdr l)))))) '())

; apply '(a) to length<=1
((lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (g)
                    (cond
                      ((null? g) 0)
                      (else (add1 ('whatever (cdr g)))))) (cdr l)))))) '(a))

; apply '(a b) to length<=2
((lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 ((lambda (g)
                    (cond
                      ((null? g) 0)
                      (else (add1 ((lambda (h)
                                      (cond
                                        ((null? h) 0)
                                        (else (add1 ('whatever (cdr h)))))) (cdr g)))))) (cdr l)))))) '(a b))

; another length0
(((lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))
  )
) 'whatever) '())

; another length<=1， exactly f(f(x)) where x = 'whatever f = λlength.(...)
(((lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))) 
    ((lambda (length)
      (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))) 'whatever)) '(a))

; extract pattern f(f(x))
(((lambda (mklength)
  (mklength (mklength 'whatever)))
    (lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))) '(a))

; easier length<=3
(((lambda (mklength)
  (mklength (mklength (mklength (mklength 'whatever)))))
    (lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))) '(a b c))