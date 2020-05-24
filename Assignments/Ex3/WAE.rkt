  #lang pl

  #| BNF for the WAE language:
       <WAE> ::= <num>
               | { + <WAE> <WAE> }
               | { - <WAE> <WAE> }
               | { * <WAE> <WAE> }
               | { / <WAE> <WAE> }
               | {SQRT <WAE> <WAE>}
               | { with { <id> <WAE> } <WAE> }
               | <id>
  |#

  ;; WAE abstract syntax trees
  (define-type WAE
    [Num  Number]
    [Add  WAE WAE]
    [Sub  WAE WAE]
    [Mul  WAE WAE]
    [Div  WAE WAE]
    [Id   Symbol]
    [Sqrt WAE]
    [With Symbol WAE WAE])

  (: parse-sexpr : Sexpr -> WAE)
  ;; to convert s-expressions into WAEs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(number: n)    (Num n)]
      [(symbol: name) (Id name)]
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (With name (parse-sexpr named) (parse-sexpr body))]
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'sqrt e) (Sqrt (parse-sexpr e))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))




  (: parse : String -> WAE)
  ;; parses a string containing a WAE expression to a WAE AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  #| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>, `y' is a
     *different* <id>)
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

  (: subst : WAE Symbol WAE -> WAE)
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
      [(Sqrt E) (Sqrt(subst E from to))]
      [(Id name) (if (eq? name from) to expr)]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))

  #| Formal specs for `eval':
       eval(N)         = N
       eval({+ E1 E2}) = eval(E1) + eval(E2)
       eval({- E1 E2}) = eval(E1) - eval(E2)
       eval({* E1 E2}) = eval(E1) * eval(E2)
       eval({/ E1 E2}) = eval(E1) / eval(E2)
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
  |#

 (: sqrt-to-list : Real -> Real)
 (define (sqrt-to-list num)
   (if (negative? num)
       (error 'sqrt-to-list "`sqrt' requires a nonnegative
input: ~s" num)
       (sqrt num)))

  (: eval : WAE -> Real)
  ;; evaluates WAE expressions by reducing them to numbers
  (define (eval expr)
    (cases expr
      [(Num n) n]
      [(Add l r) (+ (eval l) (eval r))]
      [(Sub l r) (- (eval l) (eval r))]
      [(Mul l r) (* (eval l) (eval r))]
      [(Div l r) (/ (eval l) (eval r))]
      [(Sqrt e) ( sqrt-to-list (eval e))]
      [(With bound-id named-expr bound-body)
       (eval (subst bound-body
                    bound-id
                    (Num (eval named-expr))))]
      [(Id name) (error 'eval "free identifier: ~s" name)]))

  (: run : String -> Number)
  ;; evaluate a WAE program contained in a string
  (define (run str)
    (eval (parse str)))

  ;; tests
  (test (run "5") => 5)
  (test (run "{+ 5 5}") => 10)
  (test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
  (test (run "{with {x {+ 5 5}} {* x x}}") => 100)
  (test (run "{with {x {+ 3 5}} {/ x 2}}") => 4)
  (test (run "{with {x 5} {+ x x}}") => 10)
  (test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
  (test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
  (test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
  (test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
  (test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
  (test (run "{with {x 5} {with {y x} y}}") => 5)
  (test (run "{with {x 5} {with {x x} x}}") => 5)
  (test (run "{with {x 1} y}") =error> "free identifier")
  (test (run "{sqrt 9}") => 3)
  (test (run "{sqrt 1}") => 1)
(test (run "{sqrt 0}") => 0)
(test (run "{sqrt -1}") =error> "`sqrt' requires a nonnegative
input")
(test (run "{with {x {+ 5 20}} {sqrt x}}") => 5)
(test (run "{with {6 {+ 5 5}} {+ x x}}") =error> "bad `with' syntax in")
(test (run "{with {x {> 5 5}} {+ x x}}") =error> "bad syntax in")


#|
{call {fun {x}
        {* x x}}
      5}


sqr = {fun {x}
        {* x x}}
{+ {sqr 5}
   {sqr 6}}

[parameter: 'x
 body:  (Mul (Id 'x) (Id 'x))
 ]

((lambda (x)
         (* x x)) 5)
(f 5)
|#
