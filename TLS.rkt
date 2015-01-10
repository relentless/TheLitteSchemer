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

(define (rember? a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a) (cdr lat))
    (else (cons (car lat) 
                (rember? a (cdr lat))))))

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

(insertL 2 3 '(1 3 4))