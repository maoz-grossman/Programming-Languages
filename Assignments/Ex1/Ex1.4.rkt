#lang pl
(: is-odd? : Natural -> Boolean)
;; Checks if a given natural number (>= 0) is an odd number
;;it does it by checking if the number it follows is an even number
(define (is-odd? x)
 (if (zero? x)
 false
 (is-even? (- x 1))))
(: is-even? : Natural -> Boolean)
;;Checks if a given number is an even number
;;It dows that by checking if the previous number is odd
(define (is-even? x)
 (if (zero? x)
 true
 (is-odd? (- x 1))))

#|the two functions work recursivly
If some nutural number n is (without loss of generasity) odd
then n-1 must be even. the process stops when we reach 0.
If we did an even iteation until we reach 0,
it means that the number is odd ,else n is even.
Normally when we count down we count without zero
that why 3 2 1 is an odd number of digit,
but 3 2 1 0 is an even number of digits.
|#


;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))


(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; Function that gets a predicate, function that returns a boolean variable
;; and list of elemnts that can be measured by the predicate,
;;and checks recursivly if all the elements of the list implement the predicate
(define (every? pred lst)
 (or (null? lst)
 (and (pred (first lst))
 (every? pred (rest lst)))))


;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; The function gets a list of natural number ,
;; and checks if all its elements are even using 'every?' function
(define (all-even? lst)
 (every? is-even? lst))


;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
Boolean))

#| 
;; Simmilary to 'every?' just the function gets two predicates
 and two list that the first one can be measred by the first predicate
and the second list by the second predicate, and the lists should be the same length
the function passes all of the elements of the two lists, and checks
 if the two lists implement their predicates.
only if the two lists indeed implement the predicate the function returns #t
|#
(define (every2? pred1 pred2 lst1 lst2)
 (or (null? lst1) ;; both lists assumed to be of same length
 (and (pred1 (first lst1))
 (pred2 (first lst2))
 (every2? pred1 pred2 (rest lst1) (rest lst2)))))

(test (every2? is-odd? is-even? '(1 3 5) '(2 4 6) ))
(test (every2? is-odd? is-even? '(1 4 5) '(2 4 6) ) => #f)