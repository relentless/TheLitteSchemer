#lang scheme/base

; Exercises and practice based on the book The Little Schemer (4th ed)

; prerequisite fuction definitions
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

;(atom? "a")
;(atom? 1)
;(atom? '())
;(atom? '(1 2 3))
;(atom? (quote ()))

; CAR is defined only for non-empty lists

;(car '(1 2 3))
;(car 'a) ; contract violation
;(car '()) ; contract violation

; CDR is defined only for non-empty lists

;(cdr '(1 2 3))
;(cdr 'a) ; contract violation
;(cdr '()) ; contract violation

; CONS takes two arguments.  The second is a list.

;(cons 'a '(1 2 3))
;(cons 'a 'b); ?? works, but gives (a . b)

; NULL? checks for an empty list

;(null? '())
;(null? '(1 2))
;(null? 'a); not really a valid question as 'a not a list, but returns #f

; EQ? takes two non-numeric atoms

;(eq? 'a 'a)
;(eq? 'a 'b)
;(eq? 1 1); not really a valid question, but gives #t
;(eq? 0.1 0.1); not really a valid question, but gives #t
;(eq? 1 2); not really a valid question, but gives #f
;(eq? '(1 2) '(1 2))

(define (lat? x)
  (cond
    ((null? x) #t)
    ((atom? (car x)) (lat? (cdr x)))
    (else #f)))

;(lat? '())
;(lat? '(1 2))
;(lat? 'a) ; not a valid question, apparently
;(lat? '('(1 2) 3))

(define (member? a lat)
  (cond
    ((null? lat) #f)
    ((eq? (car lat) a) #t)
    (else (member? a (cdr lat)))))

;(member? 1 '(1 2 3))
;(member? 2 '(1 2 3))
;(member? 1 'a); not valid

(define (rember a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (cdr lat))
    (else (cons (car lat) 
                (rember a (cdr lat))))))

;(rember? 1 '(1 2 3))
;(rember? 2 '(1 2 3))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

;(firsts '())
;(firsts '((1 2 3) (4 5 6)))

(define insertR 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new 
                                       (cdr lat))))
      (else (cons (car lat) 
                  (insertR new old (cdr lat)))))))

;(insertR 3 2 '(1 2 4))

(define insertL 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) 
                  (insertL new old (cdr lat)))))))

;(insertL 2 3 '(1 3 4))

(define subst 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) 
                  (subst new old (cdr lat)))))))

;(subst 2 99 '(1 99 3))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) 
                  (multirember a (cdr lat)))))))

;(multirember 1 '(1 2 1 3 1 4 1))

(define add1
  (lambda (n)
    (+ n 1)))

;(add1 99)

(define sub1
  (lambda (n)
    (- n 1)))

;(sub1 100)

(define o+
  (lambda (m n)
    (cond
      ((zero? n) m)
      ;(else (o+ (add1 m) (sub1 n)))))) ; my version
      (else (add1 (o+ m (sub1 n))))))) ; book version

;(o+ 5 2)

(define o-
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (sub1 (o- m (sub1 n)))))))

;(o- 5 2)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup)
                (addtup (cdr tup)))))))

;(addtup '(1 2 3 4 5))

(define x
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (o+ m (x m (sub1 n)))))))

;(x 7 2)

; my initial version
;(define tup+
;  (lambda (tup1 tup2)
;    (cond
;      ((and (null? tup1) (null? tup2)) '())
;      (else (cons (+ (car tup1)
;                     (car tup2))
;                  (tup+ (cdr tup1) (cdr tup2)))))))

;(tup+ '(1 2 3 4) '(10 20 30 40))

; full version from book
;(define tup+ 
;  (lambda (tup1 tup2) 
;    (cond 
;      ((and (null? tup1) (null? tup2)) (quote ())) 
;      ((null? tup1) tup2) 
;      ((null? tup2) tup1) 
;      (else (cons (+ (car tup1) (car tup2))
;                  (tup+ (cdr tup1) (cdr tup2)))))))

; my simplified full version
(define tup+ 
  (lambda (tup1 tup2) 
    (cond
      ((null? tup1) tup2) 
      ((null? tup2) tup1) 
      (else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

;(tup+ '(1 2 3 4) '(10 20 30))
;(tup+ '(1 2 3) '(10 20 30 40))

(define >
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (> (sub1 m) (sub1 n))))))

;(> 123 456)
;(> 321 123)
;(> 3 3)

(define <
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (< (sub1 m) (sub1 n))))))

;(< 123 456)
;(< 321 123)
;(< 3 3)

(define =
  (lambda (m n)
    (and (not (< m n)) 
         (not (> m n)))))

;(= 123 456)
;(= 321 123)
;(= 3 3)

(define pow
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (x m (pow m (sub1 n)))))))

;(pow 1 1)
;(pow 2 3)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

;(length '(bip bop bap))

(define pick
  (lambda (n lat)
    (cond
      ((= n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

;(pick 2 '(bip bop bap))

(define rempick
  (lambda (n lat)
    (cond
      ((= n 1) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))
  
(rempick 2 '(bip bop bap))