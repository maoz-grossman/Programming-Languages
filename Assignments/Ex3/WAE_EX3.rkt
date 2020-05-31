#lang pl
#|

<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>

|#



(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])



(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))



#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#



(: substW : WAE Symbol WAE -> WAE)
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]))

#|
 We need to find every free instance.
 In order to do so, we consider that an instance will become
 an IdW trpe or a symbol in a WithW type when using the parseW function.
 We also consider that a free instance will eventually become an IdW type.
 So when dealing with the WithW case, we use substW to continue our expression,
 But we extract the named part of the argument, and use it on freeInstanceList
 in order to consider every argument that can be in there only once.
 All the other cases are cases with a function, number or argument,
 so we can just implement eval with a twist of returning a list of arguments using append.
|#
(: freeInstanceList : WAE -> (Listof Symbol))
(define (freeInstanceList expr)
(cases expr
  [(NumW n) '()]
  [(AddW l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(SubW l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(MulW l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(DivW l r) (append (freeInstanceList l) (freeInstanceList r))]
  [(IdW sym) (list sym) ]
  [(WithW sym name body) (append (freeInstanceList (substW body sym (NumW 0))) (freeInstanceList name))]))

(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (parseW "{with {xxx 2} {with {x 3} {+ {- xx x} x}}}")) => '(xx))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (parseW "{with {x 5}{with {y {/ z 3}}{+ y y}}}" )) => '(z))
(test (freeInstanceList (parseW "{with {x 5}{with {y {* z 3}}{+ y y}}}" )) => '(z))
(test (parseW "{with {6 {+ 5 5}} {+ x x}}") =error> "bad `with' syntax in")
(test (parseW "{with {x {> 5 5}} {+ x x}}") =error> "bad syntax in")
(test (parseW "{with {x 5} {+ x {with {x 3} 10}}}") =>(WithW 'x (NumW 5) (AddW (IdW 'x) (WithW 'x (NumW 3) (NumW 10)))))
(test (parseW "{with {x 5} {+ x {with {x 3} x}}}")=>(WithW 'x (NumW 5) (AddW (IdW 'x) (WithW 'x (NumW 3) (IdW 'x)))))
(test (parseW "{with {x 5} x}")=>(WithW 'x (NumW 5) (IdW 'x)))





