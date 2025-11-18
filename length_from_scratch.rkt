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


; yet another length0, inner length now take (car l) as parameter and return a function (lambda l) which the else branch won't compile, becuase it applys (cdr l) to (cdr l)
(((lambda (mklength) (mklength mklength))
    (lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))) '())

; make length<=1 by length0
(((lambda (mklength) (mklength (mklength mklength)))
    (lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))) '(a))

; the same as the above one
(((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 ((length length) (cdr l)))))))
    (lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))) '(a))

; we want to make it non-stop, try the one below
(((lambda (mklength) (mklength mklength))
    (lambda (mklength)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 ((mklength mklength) (cdr l)))))))) '(a b c d e))

; the same as the above one, (mklength mklength) expands to the whole expression
(((lambda (mklength)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mklength mklength) (cdr l)))))))
  (lambda (mklength)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mklength mklength) (cdr l)))))))) '(a b c d e))