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
;(define eqlist?
;  (lambda (l1 l2)
;     (cond
;       ((null? l1) (null? l2))
;       ((atom? (car l1)) (cond
;                           ((null? l2) #f)
;                           ((atom? (car l2)) (and (eqan? (car l1)(car l2))
;                                                  (eqlist? (cdr l1) (cdr l2))))
;                           (else #f)))
;       (else (cond
;               ((null? l2) #f)
;               ((atom? (car l2)) #f)
;               (else (and (eqlist? (car l1) (car l2))
;                          (eqlist? (cdr l2) (cdr l2)))))))))

; My version since realising it should be based on top-level conditions.
; This works and is more concise than the above answer, but 
; still has many less top-level conditions than the book version.
;(define eqlist?
;  (lambda (l1 l2)
;     (cond
;       ((null? l1) (null? l2))
;       ((and (atom? (car l1)) 
;             (atom? (car l2))) (and (eqan? (car l1) (car l2)) 
;                                    (eqlist? (cdr l1) (cdr l2))) )
;       ((or (atom? (car l1)) (atom? (car l2))) #f)
;       (else (and (eqlist? (car l1) (car l2)) 
;                  (eqlist? (cdr l1) (cdr l2)))))))

; Let's try for 9 top-level conditions!
; Yes! Exactly like book.  Seems a bit unnecessary though, doesn't use even basic 
; optimisations of logic.
;(define eqlist?
;  (lambda (l1 l2)
;     (cond
;       ((and (null? l1) (null? l2)) #t)
;       ((and (null? l1) (atom? (car l2))) #f)
;       ((null? l1) #f)
;       ((and (atom? (car l1)) (null? l2)) #f)
;       ((and (atom? (car l1))(atom? (car l2))) (and (eqan? (car l1) (car l2))
;                                                    (eqlist? (cdr l1) (cdr l2))))
;       ((atom? (car l1)) #f)
;       ((null? l2) #f)
;       ((atom? (car l2)) #f)
;       (else (and (eqlist? (car l1) (car l2))
;                  (eqlist? (cdr l1) (cdr l2)))))))
       
;(eqlist? '(strawberry cream ice) '(strawberry ice cream))
;(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
;(eqlist? '(beef ((salami)) (and (soda))) '(beef ((sausage)) (and (soda))))

; original

;(define equal?
;  (lambda (s1 s2)
;    (cond
;      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
;      ((atom? s1) #f)
;      ((atom? s2) #f)
;      (else (eqlist? s1 s2)))))

; simplified

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

;(equal? 'a 'a)
;(equal? 'a 'b)
;(equal? '() '())
;(equal? '(1 2 3) '(3 2 1))
;(equal? '(1 2 3) '(1 2 3))

; eqlist? using equal?
(define eqlist?
  (lambda (l1 l2)
     (cond
       ((and (null? l1) (null? l2)) #t)
       ((or (null? l1) (null? l2)) #f)
       (else (and (equal? (car l1) (car l2))
                  (eqlist? (cdr l1) (cdr l2)))))))
       
;(eqlist? '(strawberry cream ice) '(strawberry ice cream))
;(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
;(eqlist? '(beef ((salami)) (and (soda))) '(beef ((sausage)) (and (soda))))

(define numbered?
  (lambda (exp)
    (cond
      ((atom? exp) (number? exp))
      ((eq? (car (cdr exp)) '*) (and (numbered? (car exp))
                                     (numbered? (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '+) (and (numbered? (car exp))
                                     (numbered? (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '^) (and (numbered? (car exp))
                                     (numbered? (car (cdr (cdr exp)))))))))
      
;(numbered? '1)
;(numbered? 'hi)
;(numbered? '((1 + 1) ^ (2 * (3 ^ 4))))

(define value
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (car (cdr exp)) '*) (* (value (car exp))
                                     (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '+) (+ (value (car exp))
                                     (value (car (cdr (cdr exp))))))
      ((eq? (car (cdr exp)) '^) (pow (value (car exp))
                                     (value (car (cdr (cdr exp)))))))))

;(value '(2 ^ 4))
;(value '(2 + 4))
;(value '(2 * 4))
;(value '((1 + 1) ^ (2 * (3 + 4))))

(define value2
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (car exp) '*) (* (value2 (car (cdr exp)))
                             (value2 (car (cdr (cdr exp))))))
      ((eq? (car exp) '+) (+ (value2 (car (cdr exp)))
                             (value2 (car (cdr (cdr exp))))))
      ((eq? (car exp) '^) (pow (value2 (car (cdr exp)))
                               (value2 (car (cdr (cdr exp)))))))))

;(value2 '(^ 2 4))
;(value2 '(+ 2 4))
;(value2 '(* 2 4))
;(value2 '(^ (+ 1 1) (* 2 (+ 3 4))))

(define 1st-sub-exp
  (lambda (exp)
    (car (cdr exp))))

(define 2nd-sub-exp
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define operator
  (lambda (exp)
    (car exp)))

(define value3
  (lambda (exp)
    (cond
      ((atom? exp) exp)
      ((eq? (operator exp) '*) (* (value3 (1st-sub-exp exp))
                             (value3 (2nd-sub-exp exp))))
      ((eq? (operator exp) '+) (+ (value3 (1st-sub-exp exp))
                             (value3 (2nd-sub-exp exp))))
      ((eq? (operator exp) '^) (pow (value3 (1st-sub-exp exp))
                               (value3 (2nd-sub-exp exp)))))))

;(value3 '(^ 2 4))
;(value3 '(+ 2 4))
;(value3 '(* 2 4))
;(value3 '(^ (+ 1 1) (* 2 (+ 3 4))))

(define zero^?
  (lambda (n)
    (eq? n '())))

;(zero^? '())
;(zero^? '(()))

(define add1^
  (lambda (n)
    (cons '() n)))

;(add1^ '())
;(add1^ '(()))
;(add1^ '(() ()))

(define sub1^
  (lambda (n)
    (cdr n)))

;(sub1^ '(() ()) )
;(sub1^ '(()) )

(define +^
  (lambda (m n)
    (cond
      ((zero^? m) n)
      (else (+^ (sub1^ m) (add1^ n))))))

;(+^ '(()()) '(()()()))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

;(set? '(apples pears cider))
;(set? '())
;(set? '(apples pears apples))
;(set? '(pears apples apples))

; works properly, but book used simpler version which didn't bother maintaining the order
; (which is probably what you really want with a set)

;(define makeset
;  (lambda (lat)
;    (cond
;      ((null? lat) '())
;      (else (cond 
;              ((member? (car lat) (cdr lat)) (cons (car lat) 
;                                                   (rember (car lat) (makeset (cdr lat)))))
;              (else (cons (car lat) (makeset (cdr lat)))))))))

; simpler version:
;(define makeset
;  (lambda (lat)
;    (cond
;      ((null? lat) '())
;      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
;      (else (cons (car lat) (makeset (cdr lat)))))))

; with multirember:

; This is basically the same as my first version, but simplified.
; Use of Rember vs Multirember doesn't seem to make a difference.

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat) 
                  (multirember (car lat) (makeset (cdr lat))))))))

;(makeset '(apple peach pear peach plum apple apple lemon peach))

;(define subset?
;  (lambda (set1 set2)
;    (cond
;      ((null? set1) #t)
;      (else (and (member? (car set1) set2)
;                 (subset? (cdr set1) set2))))))

; shorter version

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

;(subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
;(subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

;(eqset? '(6 large chickens with wings) '(6 large chickens with wings))
;(eqset? '(6 large chickens with wings) '(6 large chicken wings))

;(define intersect?
;  (lambda (set1 set2)
;    (cond
;      ((null? set1) #f)
;      ((member? (car set1) set2) #t)
;      (else (intersect? (cdr set1) set2)))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

;(intersect?'(stewed tomatoes and macaroni) '(macaroni and cheese))
;(intersect?'(stewed tomatoes and macaroni) '(mac n cheese))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1)
                                       (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

;(intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
;(intersect '(stewed tomatoes and macaroni) '(mac n cheese))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

;(union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))

;(define intersectall
;  (lambda (lset)
;    (cond
;      ((equal? (cdr lset) '()) '())
;      (else (union (intersect (car lset) (cadr lset))
;                   (intersectall (cdr lset)))))))

; book version, which is better

(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset)) (car lset))      
      (else (intersect (car lset) (intersectall (cdr lset)))))))
                
     
;(intersectall '((6 pears and)(3 peaches and 6 peppers)(8 pears and 6 plums)(and 6 prunes with some apples)))

(define a-pair?
  (lambda (l)
    (eq? (length l) 2)))

;(a-pair? '(1 2))
;(a-pair? '(1))
;(a-pair? '((1) (2 3)))

(define first
  (lambda (p)
     (car p)))

(define second
  (lambda (p)
     (car (cdr p))))


(define build
  (lambda (s1 s2)
     (cons s1 (cons s2 '()))))

;(define p1 (build 1 2))
;(first p1)
;(second p1)

(define fun?
  (lambda (l)
    (set? (firsts l))))

;(fun? '((1 2)(3 4)(5 6)))
;(fun? '((1 2)(2 1)(1 2)))

; just consing things - creates some kind of pair type instead of lists,
; as no empty element at the end (prints as (8.a)

;(define revrel
;  (lambda (rel)
;    (cond
;      ((null? rel) '())
;      (else (cons (cons (cdr (car rel))
;                        (car (car rel))) 
;                  (revrel (cdr rel)))))))

; using build
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (cdr (car rel))
                         (car (car rel))) 
                  (revrel (cdr rel)))))))

;(revrel '((8 a)(pumpkin pie)(got sick)))


(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (cdr (car l)))
                  (seconds (cdr l)))))))

(define fullfun?
  (lambda (l)
    (set? (seconds l))))

;(fullfun? '((1 2)(1 4)(1 6)))
;(fullfun? '((1 2)(2 1)(1 2)))

