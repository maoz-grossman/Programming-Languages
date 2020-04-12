#lang pl


(: listLength : (Listof Any) -> Natural )
( define (listLength ls )
   (: helper-listLength : Natural (Listof Any) -> Natural )
   (define ( helper-listLength acc ls )
     (if (null? ls)
         acc
     (helper-listLength (+ 1 acc) (rest ls ))
      );end if
    );end define  helper-listLength
   (helper-listLength 0 ls )
 )

;; Iterate on all the element of a list and checks if apply the given predicate
(: every? : All(A) (A -> Boolean)(Listof A) -> Boolean )
(define (every? pred ls )
  (or (null? ls)
      (and (pred(first ls))
           (every? pred(rest ls))
           );;end and
      );;end or
  );; end define


#|//////////
    EX2
///////////|#

;;2.1:


;;Gets a coefficient the power of X, and returns its representing string.
(: Xexpr : Number Number -> String)
(define (Xexpr coef pow)
  (cond 
    [(= coef 0) ""]
    [(= coef 1) (getXPowExpr pow)]
    [(= pow 0) (number->string coef)]
    [else (string-append (number->string coef) (getXPowExpr pow))]
    );;end cond
  );; end define

;;Decide how X should look like acording to its power
(: getXPowExpr : Number -> String)
(define (getXPowExpr pow)
  (cond
    [(= pow 0) "1"]
    [(= pow 1) "x"]
    [else (string-append "x^" (number->string pow))]
    );;end cond
  );;end define


;;Decides what the signum of the coefficient (+/-)
(: getSignum : Number -> String)
(define (getSignum coef)
  (cond
    [(or(zero? coef) (> 0 coef)) ""]
    [else "+"]
    );;end cond
  );;end define


;;Get a list of numbers and return unfinished polynomial expression 
(: listToXexpr : (Listof Number) -> String)
(define (listToXexpr ls )
  (if (= (listLength ls) 1)
      ;;if the list contains only an element send him back with his Signum(+\-)
     ( string-append (getSignum (first ls)) (Xexpr (first ls) (- (listLength ls) 1) ))
     ;; Else take the first element of the list,add it to the temp string and keep going with the rest of the list
      (string-append (getSignum (first ls))
                     (Xexpr (first ls)(- (listLength ls) 1))
                     (listToXexpr (rest ls)));;end  string-append 
      );;end if
  );;end define


;;Gets a list of numbers and return a valid polynomial expression
(:  write-poly : (Listof Number)-> String)
(define (write-poly ls)
  (cond [(null? ls) ""]
        [(= (listLength ls) 1) (Xexpr (first ls) (- (listLength ls) 1))]
        [else (string-append
               (Xexpr (first ls) (- (listLength ls) 1) )
               (listToXexpr (rest ls));;using listToXexpr to get the rest
               );; end string-append
              ];;end else
        );;end if
  );;end define



;;2.2

(: power : Number Number -> Number)
(define (power X pow)
  (if (= 0 pow)
      1
      (* X (power X (- pow 1)))
      );;end if
  );; end define


;; Calculates the value of a given monom aX^3 when X is some real number b 
(: calculateMonom : Number Number Number -> Number)
(define (calculateMonom coef X pow )
  (* coef (power X pow))
  );end define

;; compute a polynom expression using helper function
(: compute-poly : Number (Listof Number)-> Number )
(define (compute-poly X ls)
  (: helper : Number (Listof Number)-> Number)
  (define (helper acc ls)
    (if
     (null? ls)
     acc
     (helper (+ acc (calculateMonom(first ls) X (- (listLength ls) 1))) (rest ls))
     );;end if
    );;end define
    (helper 0 ls)
  );;end define


#|////////////
     TESTs
/////////////|#

;;2.1
(test(getXPowExpr 0)=> "1")
(test(getXPowExpr 1)=> "x")
(test(getXPowExpr 15)=> "x^15")
(test(Xexpr 0 3)=> "")
(test(Xexpr -9 0)=> "-9")
(test(Xexpr 15 34)=> "15x^34")
(test(Xexpr -9 3)=> "-9x^3")
(test(Xexpr 1 0)=> "1")
(test(Xexpr 1 1)=> "x")
(test(Xexpr 1 23)=> "x^23")
(test(getSignum 10)=> "+")
(test(getSignum -10)=> "")
(test(getSignum 0)=> "")

(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(1)) => "1")
(test (write-poly '(7 -8 0 10)) => "7x^3-8x^2+10")
(test (write-poly '(-7 0 0 0)) => "-7x^3")
(test (write-poly '(0)) => "")
(test (write-poly '(-0)) => "")
(test (write-poly '(0 0 0 -0 0)) => "")


;;2.2

(test (power 2 2) => 4)
(test (power 3 3) => 27)
(test (power 5 3)=> 125)
(test (power 6 0) => 1)
(test (power 0 2) => 0)
(test (calculateMonom 3 2 2)=> 12)
(test (calculateMonom 0 2 2)=> 0)
(test (calculateMonom 1 2 2)=> 4)
(test (calculateMonom -9 -2 2)=> -36)
(test (calculateMonom 9 -2 3)=> -72)

(test (compute-poly 2 '()) => 0)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)

