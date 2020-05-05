; The ROL BNF and Parsing code:
#lang pl


;;Checks the length of the list, we saw this on class  
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


;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; The actual interpreter

#| BNF for the RegE language:
 <ROL> ::= {Len <num> <RegE>}
 <RegE> ::= {Reg <Bit-List>}
           |{Shl <RegE>}
           |{Or <RegE> <RegE>}
           |{And <RegE> <RegE>}
 <Bits-List>::=<Bits>|<Bits><Bits-List>
 <Bits> ::= {0} | {1} 
 |#


;; RegE abstract syntax trees
(define-type RegE
 [Reg Bit-List]
 [And RegE RegE]
 [Or RegE RegE]
 [Shl RegE])


;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
 (: list->bit-list : (Listof Any) -> Bit-List)
 ;; to cast a list of bits as a bit-list
 (define (list->bit-list lst)
 (cond [(null? lst) null]
 [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
 [else (cons 0 (list->bit-list (rest lst)))]))



 (: parse-sexpr : Sexpr -> RegE)
 ;; to convert the main s-expression into ROL
 (define (parse-sexpr sexpr)
 (match sexpr
 [(list 'reg-len '= ( number: len ) B) (if (> len 0) (parse-sexpr-RegL B len)
                    (error 'parse-sexpr "length too short in ~s" sexpr) )] ;; remember to make sure specified register length is at least 1
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))




 (: parse-sexpr-RegL : Sexpr Number -> RegE)
 ;; to convert s-expressions into RegEs
 (define (parse-sexpr-RegL sexpr reg-len)
 (match sexpr
 [(list (and a (or 1 0)) ... ) (if (eq? reg-len (listLength a)) (Reg (list->bit-list a))
 (error 'parse-sexpr "wrong number of bits in ~s" a))]
 [(list 'shl lst) (Shl (parse-sexpr-RegL lst reg-len))]
 [(list 'and lns rns ) (And (parse-sexpr-RegL lns reg-len) (parse-sexpr-RegL rns reg-len))]
 [(list 'or lns rns ) (Or (parse-sexpr-RegL lns reg-len) (parse-sexpr-RegL rns reg-len))]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



 (: parse : String -> RegE)
 ;; parses a string containing a RegE expression to a RegE AST
 (define (parse str)
 (parse-sexpr (string->sexpr str)))




 ;; tests
 (test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
 (test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
 (test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg
'(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
 (test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") =>
(Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
 (test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl
(Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
 (test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 4 {or {1 1 a 1} {0 1 1 1}}}") =error> "bad syntax in")
 (test (parse "{ reg-len = 0 {}}") =error> "length too short in")
  (test (parse "{ reg-len = 3 {1 2 1}}") =error> "bad syntax in")
 (test (parse "{  = 3 {1 1 1}}") =error> "bad syntax in")
