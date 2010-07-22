#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define sub1
  (lambda (x)
    (- x 1)))

(define add1
  (lambda (x)
    (+ x 1)))


(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (x l)
    (cond
      ((null? l) #f)
      (else (or (equal? (car l) x)
                (member? x (cdr l)))))))

(define member*
  (lambda (x lat)
    (cond
      ((null? lat) #f)
      ((atom? (car lat))
       (cond
         ((eq? (car lat) x) #t)
         (else (member* x (cdr lat)))))
      (else (or (member* x (car lat))
                (member* x (cdr lat)))))))



(define rember-f
  (lambda (test?)
    (lambda (x l)
      (cond
        ((null? l) '())
        ((test? x (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) x (cdr l))))))))

(define rember-eq?
  (lambda (x l)
    ((rember-f eq?) x l)))

(define multirember
  (lambda (x lat)
    (cond
      ((null? lat) '())
      ((eq? x (car lat)) (multirember x (cdr lat)))
      (else (cons (car lat) (multirember x (cdr lat)))))))

(define rember*
  (lambda (x lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? (car lat) x) (rember* x (cdr lat)))
         (else (cons (car lat) (rember* x (cdr lat))))))
      (else (cons (rember* x (car lat)) 
                  (rember* x (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons (car lat) (cons new (insertR* new old (cdr lat)))))
         (else (cons (car lat) (insertR* new old (cdr lat))))))
      (else (cons (insertR* new old (car lat)) (insertR* new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define insertL*
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (cond
         ((eq? (car lat) old) (cons new (cons old (insertL* new old (cdr lat)))))
         (else (cons (car lat) (insertL* new old (cdr lat))))))
      (else (cons (insertL* new old (car lat)) (insertL* new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond 
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(define raise
  (lambda (x p)
    (cond
      ((zero? p) 1)
      (else (* x (raise x (sub1 p)))))))

(define length?
  (lambda (lat)
    (cond 
      ((null? lat) 0)
      (else (add1 (length? (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define occur
  (lambda (x lat)
    (cond 
      ((null? lat) 0)
      ((eq? x (car lat)) (add1 (occur x (cdr lat))))
      (else (occur x (cdr lat))))))

(define occur*
  (lambda (x lat)
    (cond
      ((null? lat) 0)
      ((atom? (car lat))
       (cond
         ((eq? (car lat) x) (add1 (occur* x (cdr lat))))
         (else (occur* x (cdr lat)))))
      (else (+ (occur* x (car lat)) (occur* x (cdr lat)))))))

(define leftmost
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
       (car lat))
      (else (leftmost (car lat))))))

(define numbered?
  (lambda (x)
    (cond 
      ((atom? x) (number? x))
      (else
       (and (numbered? (car x)) 
            (numbered? (car (cdr (cdr x)))))))))

(define value
  (lambda (exp)
    (define operator
      (lambda (lat)
        (car lat)))
    (define 1st-sub-exp
      (lambda (lat)
        (car (cdr lat))))
    (define 2nd-sub-exp
      (lambda (lat)
        (car (cdr (cdr lat)))))
    
    (cond 
      ((atom? exp) exp)
      ((eq? (operator exp) '+)
       (+ (value (1st-sub-exp exp))
          (value (2nd-sub-exp exp))))
      ((eq? (operator exp) 'X)
       (* (value (1st-sub-exp exp))
          (value (2nd-sub-exp exp))))
      ((eq? (operator exp) '↑)
       (raise (value (1st-sub-exp exp))
              (value (2nd-sub-exp exp)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))


(define makeset
  (lambda (lat)
    (cond 
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))


(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset? (cdr set1) set2))
      (else #f))))


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect? 
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2))))) 

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
                       (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define relation?
  (lambda (l-pair)
    (define relpair?
      (lambda (lp)
        (cond
          ((null? lp) #t)
          ((a-pair? (car lp)) (relpair? (cdr lp)))
          (else #f))))
    (cond
      ((null? l-pair) #t)
      ((set? l-pair) (relpair? l-pair))
      (else #f))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define reverse-pair
  (lambda (pair)
    (build (second pair)
           (first pair))))

(define reverse-relation
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (reverse-pair (car rel))
                  (reverse-relation (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (and (fun? fun)
         (fun? (reverse-relation fun)))))



(provide (all-defined-out))
