#lang pl

(: power : Number Number -> Number)
(define (power X pow)
  (if (= 0 pow)
      1
      (* X (power X (- pow 1)))))


(: square-list : (Listof Any) -> (Listof Number))
(define (square-list lst)
(if (null? lst)
     (error 'parse-sexpr "bad type of list in ~s" lst)
  (map(lambda (num)
             (if (number? num) (power num 2)
                  (error 'parse-sexpr "bad type of list in ~s" lst)))lst )))


(: sum-of-squares : (Listof Any) -> Number)
(define (sum-of-squares lst)
  (foldl + 0(square-list lst)))



;;Tests:
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(9)) => 81)
(test (sum-of-squares '(-9)) => 81)
(test (sum-of-squares '(1 2 a)) =error> "bad type of list in")
(test (sum-of-squares '()) =error> "bad type of list in")


