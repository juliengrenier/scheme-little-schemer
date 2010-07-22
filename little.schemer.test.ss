#lang scheme
(require (planet schematics/schemeunit:3) "little.schemer.ss")
(require (planet schematics/schemeunit:3/text-ui))



(define assert
  (lambda (expect actual)
    (check-equal? actual expect "test")))

(define assert-true
  (lambda (actual)
    (assert #t actual)))

(define assert-false
  (lambda (actual)
    (assert #f actual)))

(define little-schemer-tests
  (test-suite "Testing little schemer function"
              (test-case "test-null" (assert-true (null? (quote ()))))
              (test-case "test-atom" (assert-true (atom? 'harry)))
              (test-case "test-member-1" (assert-true (member? 'a '(a b c))))
              (test-case "test-member-2" (assert-true (member? '(a b) '((a b)))))
              (assert-false (member? 'x '(z b f)))
              (assert-true (member* 'x '(((a b d) x ( f g h)))))
              (assert-true (member* 'x '(((a b d) ( f g h x)))))
              (assert-false (member* 'y '(((a b d) ( f g h x)))))
              (assert '(b a c) (rember-eq? 'a '(a b a c)))
              (assert '(a b a c) (rember-eq? 'x '(a b a c)))
              (assert '(b c) (multirember 'a '(a b c a)))
              (assert '((b) (c d) (((d f e)))) (rember* 'a '((a b) (c d a) (((d f e a))))))
              (assert '(a d (g h)) (firsts '((a b c) (d e f) ((g h) i j))))
              (assert '(b a c) (insertR 'a 'b '(b c)))
              (assert '(b a c f g b a) (multiinsertR 'a 'b '(b c f g b)))
              (assert '((b a c) b a ((((b a f g))))) (insertR* 'a 'b '((b c) b ((((b f g)))))))
              (assert '(f a b c) (insertL 'a 'b '(f b c)))
              (assert '(f a b c a b) (multiinsertL 'a 'b '(f b c b)))
              (assert '((a b c) a b ((((a b f g))))) (insertL* 'a 'b '((b c) b ((((b f g)))))))
              (assert '(a c) (subst 'a 'b '(b c)))
              (assert '(a c a f) (multisubst 'a 'b '(b c b f)))
              (assert 48 (addtup '(12 14 20 2)))
              (assert '(11 11 11) (tup+ '(8 3 11) '(3 8 0)))
              (assert '(11 3 11) (tup+ '(8 3 11) '(3)))
              (assert '(11 3 11) (tup+ '(8) '(3 3 11)))
              (assert 8 (raise 2 3))
              (assert 3 (length? '(a b c)))
              (assert 'y (pick 2 '(x y z)))
              (assert '(a b d) (rempick 3 '(a b c d)))
              (assert 'potato (leftmost '((potato) (chips ((with) fish) (chips)))))
              (assert '(a b c) (no-nums '(a 1 b 2 c 3)))
              (assert '(1 2 3) (all-nums '(a 1 b 2 c 3)))
              (assert 5 (occur 'x '(x x x x x a b c)))
              (assert 6 (occur* 'x '((x x) x ((((x (x x))))))))
              
              (assert 'hot (leftmost '(((hot) (tuna (and))) cheese)))
              (assert '() (leftmost '()))
              
              
              (assert-true (numbered? 1))
              (assert-true (numbered? (quote (4 + 3)))) 
              
              (assert 5 (value '5))
              (assert 7 (value '(+ 3 4)))
              (assert 32 (value '(X 4 8)))
              (assert 128 (value '(↑ 2 7)))
              
              (assert-true (set? '(apple orange grape)))
              (assert-false (set? '(apple orange apple)))
              (assert-false (set? '((a b) (a b))))
              
              
              (assert '(orange apple) (makeset '(apple orange apple)))
              
              (assert-true (subset? '(apple orange) '(apple orange grape)))
              (assert-false (subset? '(apple orange bob) '(apple orange grape)))
              
              (assert-true (eqset? '(apple orange) '(orange apple)))
              (assert-false (eqset? '(apple) '(orange)))
              (assert-true (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese)))
              (assert '(and macaroni) (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese)))
              (assert '(stewed tomatoes casserole macaroni and cheese) (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese)))
              (assert '(a) (intersectall '((a b c) (c a d e) (e f g h a b))))
              (assert-true (pair? '(pear pear)))
              (assert-true (pair? '(3 7)))
              (assert-true (pair? '((2) (pair))))
              (assert-true (a-pair? '(full (house))))
              (assert-true (relation? '((a b) (b c))))
              (assert-true (relation? '((4 3) (4 2) (7 6) (6 2) (3 4))))
              (assert-false (relation? '((a b) (a b))))
              (test-case "test-fun?-1" (assert-true (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))))
              (test-case "test-fun?-2" (assert-false (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))))
              (test-case "test-reverse-pair" (assert '(a b) (reverse-pair '(b a))))
              (test-case "test-reverse-relation-1" (assert '((a 8) (pie pumpkin) (sick got)) (reverse-relation '((8 a) (pumpkin pie) (got sick)))))
              (test-case "test-fullfun?-1" (assert-false (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))))
              (test-case "test-fullfun?-2" (assert-true (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))))
              (test-case "test-fullfun?-3" (assert-true (fullfun? '((grape raisin) (plum prune) (stewed grape)))))
              
              (test-case "test-rember-f-1" (assert '(shrimp salad and salad) ((rember-f eq?) 'tuna '(shrimp salad and tuna salad))))
              (test-case "test-rember-f-2" (assert '(equal? eqan? eqlist? eqpair?) ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))))
              ))


(run-tests little-schemer-tests)