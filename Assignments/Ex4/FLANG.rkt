  ;; The Flang interpreter

  #lang pl 4

#|
The hardest part about this task was
to divide the work between us
we then agreed to do as much as possible together
through Zoom

The second most difficult thing was
to understand the part of the function
 flang->bool
We understand that the function
should return a boolean value from a type variable FLANG
but how it connects to  logic-op we understood only after we implemented
eval.
We also got stuck in eval because we didn't implement it properly:
 [(If l m r)
       (let ([cond (flang->bool (eval l))])
         (if (eq? cond true) (eval m) (eval m)))]
It took us a day to figure out we wrote (eval m) twice
|#



  #|
  The grammar:
    <FLANG> ::= <num>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <FLANG> }
              | { call <FLANG> <FLANG> }
              | True ;; add rule for True ;; Rule 10
              | False ;; Rule 11
              | {= <FLANG> <FLANG>} ;; add rule for = ;; Rule 12
              | {> <FLANG> <FLANG>} ;; add rule for >;; Rule 13
              | {< <FLANG> <FLANG>} ;; add rule for <;; Rule 14
              | {not <FLANG>} ;; add rule for not;; Rule 15
              | {if {<FLANG>}{FLANG}{FLANG}} ;; add rule 16 for (the above) if expressions

  Evaluation rules:

 Formal Substitution rules:
 subst:
 N[v/x] = N
 {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
 {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
 {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
 {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
 y[v/x] = y
 x[v/x] = v
 {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
 {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
 {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
 {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x
 {fun {x} E}[v/x] = {fun {x} E}
 B[v/x] = B ;; B is Boolean
 {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
 {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
 {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
 { not E}[v/x] = {not E[v/x]}
 {if Econd {then-do Edo} {else-do Eelse}}[v/x]
 = {if Econd[v/x] {then-do Edo[v/x]} {else-do
 Eelse[v/x]}}

  |#

  (define-type FLANG
    [Num  Number]
    [Add  FLANG FLANG]
    [Sub  FLANG FLANG]
    [Mul  FLANG FLANG]
    [Div  FLANG FLANG]
    [Id   Symbol]
    [With Symbol FLANG FLANG]
    [Fun  Symbol FLANG]
    [Call FLANG FLANG]
    [Bool Boolean]
    [Bigger FLANG FLANG]
    [Smaller FLANG FLANG]
    [Equal FLANG FLANG]
    [Not FLANG ]
    [If FLANG FLANG FLANG])

  (: parse-sexpr : Sexpr -> FLANG)
  ;; to convert s-expressions into FLANGs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num n)]
      ['True (Bool true)]
      ['False (Bool false)]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name)) body)
          (Fun name (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
      [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'not exp) (Not (parse-sexpr exp))]
      [(cons 'if more)
       (match sexpr
         [(list 'if cond (list 'then-do to-do ) (list 'else-do  else-to-do))
          (If (parse-sexpr cond) (parse-sexpr to-do) (parse-sexpr else-to-do))]
         [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



  (: parse : String -> FLANG)
  ;; parses a string containing a FLANG expression to a FLANG AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))




  (: subst : FLANG Symbol FLANG -> FLANG)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to)
    (cases expr
      [(Num n) expr]
      [(Add l r) (Add (subst l from to) (subst r from to))]
      [(Sub l r) (Sub (subst l from to) (subst r from to))]
      [(Mul l r) (Mul (subst l from to) (subst r from to))]
      [(Div l r) (Div (subst l from to) (subst r from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Call l r) (Call (subst l from to) (subst r from to))]
      [(Fun bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]
      [(Bool b) expr]
      [(Equal l r) (Equal (subst l from to) (subst r from to))]
      [(Bigger l r) (Bigger (subst l from to) (subst r from to) )]
      [(Smaller l r)(Smaller (subst l from to) (subst r from to) )]
      [(Not b) (Not (subst b from to))]
      [(If cond to-do else-do)(If (subst cond from to) (subst to-do from to) (subst else-do from to))]))


(: Num->number : FLANG -> Number)
    (define (Num->number e)
      (cases e
        [(Num n) n]
        [else (error 'Num->number "expected a number, got: ~s" e)]))

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
  ;; gets a Racket numeric binary operator, and uses it within a FLANG
  ;; `Num' wrapper
  (define (arith-op op expr1 expr2)
    (Num (op (Num->number expr1) (Num->number expr2))))


 (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
 ;; gets a Racket Boolean binary operator (on numbers), and applies it
 ;; to two `Num' wrapped FLANGs
 (define (logic-op op expr1 expr2)
 (Bool (op (Num->number expr1) (Num->number expr2))))
(: flang->bool : FLANG -> Boolean)
 ;; gets a Flang E (of any kind) and returns a its appropiate
 ;; Boolean value -- which is true if and only if E does not
 ;; represent false
 ;; Remark: the `flang->bool` function will also be top-level
 ;; since it's used in more than one place.
 (define (flang->bool e)
 (cases e
        [(Bool b) b]
        [else (error 'flang->bool "expects a boolean, got: ~s" e)]))







  (: eval : FLANG -> FLANG)
  ;; evaluates FLANG expressions by reducing them to *expressions*
  (define (eval expr)
    (cases expr
      [(Num n) expr]
      [(Add l r) (arith-op + (eval l) (eval r))]
      [(Sub l r) (arith-op - (eval l) (eval r))]
      [(Mul l r) (arith-op * (eval l) (eval r))]
      [(Div l r) (arith-op / (eval l) (eval r))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (eval named-expr)))]
      [(Id name) (error 'eval "free identifier: ~s" name)]
      [(Fun bound-id bound-body) expr]
      [(Call fun-expr arg-expr)
       (let([fval (eval fun-expr)])
         (cases fval
           [(Fun bound-id bound-body)
            (eval (subst bound-body
                         bound-id
                         (eval arg-expr)))]
           [else (error 'eval "`call' expects a function, got: ~s"
                              fval)]))]
      [(Bool b) expr]
      [(Equal l r) (logic-op = (eval l) (eval r))]
      [(Bigger l r) (logic-op > (eval l) (eval r))]
      [(Smaller l r) (logic-op < (eval l) (eval r))]
      [(If l m r)
       (let ([cond (flang->bool (eval l))])
         (if (eq? cond true) (eval m) (eval r)))]
      [(Not exp) (Bool (not (flang->bool (eval exp))))]))
 



 (: run : String ->(U Number Boolean FLANG))
  ;; evaluate a FLANG program contained in a string
  (define (run str)
    (let ([result (eval (parse str))])
      (cases result
        [(Num n) n]
        [(Bool b) b]
        [else result])))

  ;; tests
  (test (run "{call {fun {x} {+ x 1}} 4}")
        => 5)
  (test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
        => 4)
    (test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
        => 7)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 True}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
        =error> "eval: `call' expects a function, got:")

(test (run "5") => 5)
  (test (run "{+ 5 5}") => 10)
  (test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
  (test (run "{with {x {+ 5 5}} {* x x}}") => 100)
  (test (run "{with {x {+ 3 5}} {/ x 2}}") => 4)
  (test (run "{with {x 5} {+ x x}}") => 10 )
  (test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
  (test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
  (test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
  (test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
  (test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
  (test (run "{with {x 5} {with {y x} y}}") => 5)
  (test (run "{with {x 5} {with {x x} x}}") => 5)
  (test (run "{with {x 1} y}") =error> "free identifier")
(test (run "{with {6 {+ 5 5}} {+ x x}}") =error> "bad `with' syntax in")
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{if {= 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True}
 {if c {then-do {> 2 1}} {else-do 2}}}")
 => true)
(test (run "{with {foo {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0}
 {if {> x 0} {/ 2 x} x}}")
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
 (test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (flang->bool (Bool false))=> false)
(test (flang->bool (Bool true))=> true)
(test (eval ( parse
          " {> 2 3}"))=> (Bool false))
(test (flang->bool (eval (parse "{> 2 3}")))=> false)
(test (flang->bool (eval (Smaller (Num 2) (Num 1)) ))=> false)
(test (run "False") => false )
(test (logic-op = (Num 2) (Num 3)) => (Bool false))
(test (flang->bool (Num 2)) =error> "flang->bool: expects a boolean, got")
(test (run "{with {x 0} {not {> x 2}}}") => true)
(test (eval (Num 0)) => (Num 0))
(test (eval (Bool true)) => (Bool true))
(test (eval (Not(Bool true))) => (Bool false))
(test  (run "{with {x  True } x}")=> true )
(test  (run "{with {x  True } False}")=> false )
(test  (run "{with {x  True } {not x}}")=> false )
(test (run "{not {not True}}")=> true)
(test (run "{call {fun {} {+ x 1}} 4}") =error> "parse-sexpr: bad `fun' syntax in"  )
(test  (run "{with {x {not False }} x}")=> true )
(test (run "{call {x {4}} 4 }") =error> "parse-sexpr: bad syntax in")
(test  (run "{with {x {= 2 2}} x}")=> true )
(test  (run "{with {x {< 2 3}} x}")=> true )
(test  (run "{with {x {> 2 3}} x}")=> false )
(test  (run "{with {x 2} {> x 3}}")=> false )
(test  (run "{with {x 2} {= x 3}}")=> false )
(test  (run "{with {x 2} {< x 3}}")=> true )