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
  
;(rempick 2 '(bip bop bap))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

;(no-nums '(1 hi "mum" 32 19))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat)
                                 (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

;(all-nums '(1 hi "mum" 32 19))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((and (atom? a1) (atom? a2)) (eq? a1 a2)))))

;(eqan? 1 'a)
;(eqan? 1 1)
;(eqan? 1 2)
;(eqan? 'a 'a)
;(eqan? 'a 'b)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else 
       (cond 
         ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))

;(occur 1 '(1 2 1 3 1 4 1 5 1))

; mine
(define one?
  (lambda (a)
    (and (number? a) (= 1 a))))

; from book - only supports numbers
;(define one?
;  (lambda (a) (= 1 a)))

;(one? 1)
;(one? 2)
;(one? 'a)

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick2 (sub1 n) (cdr lat)))))))
    
;(rempick2 2 '(bish bash bop))

; my version - more concise, but 4 top-level questions.  Seems like an optimisation of the book version.

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? l) (cond
                   ((eq? a l) '())
                   (else l)))
      ((eq? a (car l)) (rember* a (cdr l)))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

; book version

;(rember* 'cup 
;(define rember*
;  (lambda (a l)
;    (cond
;      ((null? l) '())
;      ((atom? (car l)) (cond
;                   ((eq? a (car l)) (rember* a (cdr l)))
;                   (else (cons (car l)
;                               (rember* a (cdr l))))))
;      (else (cons (rember* a (car l))
;                  (rember* a (cdr l)))))))

;(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons old 
                                                  (cons new 
                                                        (insertR* new old (cdr l)))))
                         (else (cons (car l)
                                     (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

;(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond
                         ((eq? a (car l)) (add1 (occur* a (cdr l))))
                         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

;(occur* 'banana '((banana)(split((((banana ice)))(cream(banana))sherbet))(banana)(bread)(banana brandy)))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons new 
                                                  (subst* new old (cdr l))))
                         (else (cons (car l)
                                     (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

;(subst* 'orange 'banana '((banana)(split((((banana ice)))(cream(banana))sherbet))(banana)(bread)(banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
                         ((eq? old (car l)) (cons new 
                                                  (cons old 
                                                        (insertL* new old (cdr l)))))
                         (else (cons (car l)
                                     (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

;(insertL* 'pecker 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))

; my version - extra conditionals more complicated than plain logic of book version

;(define member*
;  (lambda (a l)
;    (cond
;      ((null? l) #f)
;      ((atom? (car l)) (cond
;                         ((eq? a (car l)) #t)
;                         (else (member* a (cdr l)))))
;      (else (cond 
;              ((member* a (car l)) #t)
;              (else (member* a (cdr l))))))))

; book version

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or
                         (eq? a (car l))
                         (member* a (cdr l))))
      (else (or 
              (member* a (car l))
              (member* a (cdr l)))))))

;(member* 'chips '((potato)(chips((with)fish)(chips))))
;(member* 'peas '((potato)(chips((with)fish)(chips))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

;(leftmost '((potato) (chips ((with) fish) (chips))))
;(leftmost '())

; my version
(define eqlist?
  (lambda (l1 l2)
     (cond
       ((null? l1) (null? l2))
       ((atom? (car l1)) (cond
                           ((null? l2) #f)
                           ((atom? (car l2)) (and (eqan? (car l1)(car l2))
                                                  (eqlist? (cdr l1) (cdr l2))))
                           (else #f)))
       (else (cond
               ((null? l2) #f)
               ((atom? (car l2)) #f)
               (else (and (eqlist? (car l1) (car l2))
                          (eqlist? (cdr l2) (cdr l2)))))))))

(eqlist? '(strawberry cream ice) '(strawberry ice cream))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
(eqlist? '(beef ((salami)) (and (soda))) '(beef ((sausage)) (and (soda))))