#lang pl

(: sum-of-squares : (Listof Number) -> Number)
;; Gets list of elements, checks if all the element are numbers or null
;;if that so returns the sum of their squeres (or 0 in case of null list)
(define (sum-of-squares lst)
  (foldl + 0 (map(lambda ([num : Number])(* num num ))lst )))
;; thanks to Aviel Yosef for the explanation about [num : Number]




;;Tests:
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(9)) => 81)
(test (sum-of-squares '(-9)) => 81)
(test (sum-of-squares '()) => 0)


