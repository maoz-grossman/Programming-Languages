;; The Flang interpreter, using environments

#lang pl

#|
 #| The grammar:
<FLANG> ::= <num>
 | { with { <id> <FLANG> } <FLANG> }
 | <id>
 | { fun { <id> } <FLANG> } ;;a function may have asingle formal parameter
 | { fun { <id> <id> } <FLANG> } ;; or two formal parameters
 | { call <FLANG> <FLANG> } ;;a function has either a single actual parameter
 | { call <FLANG> <FLANG> <FLANG> } ;; or two actual parameters
|#


   eval: Evaluation rules:
 eval(N,env) = N
 eval(x,env) = lookup(x,env)
 eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
 eval({fun {x1} E},env) = <{fun {x1} E}, env>
 eval({fun {x1 x2} E},env) = <{fun {x1 x2} E}, env>
 eval({call E-op E1},env1)
       = eval(Ef,extend(x1,eval(E1,env),envf))
                  if eval(E-op,env) = <{fun {x} Ef}, envf>
       = error! otherwise
 eval({call E-op E1 E2},env1)
      = eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),envf))
                  if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
      = error! Otherwise
  |#

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]; Never created by user
  [Sub  FLANG FLANG]; Never created by user
  [Mul  FLANG FLANG]; Never created by user
  [Div  FLANG FLANG]; Never created by user
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Fun2  Symbol Symbol FLANG]
  [Call FLANG FLANG]
  [Call2 FLANG FLANG FLANG])

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
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
       [(list 'fun (list (symbol: name) (symbol: name2)) body)
        (Fun2 name name2 (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]

 #|
    [(list  '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list  '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list  '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list  '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
|#
    [(cons 'call more)
     (match sexpr
       [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
       [(list 'call fun arg1 arg2)(Call2 (parse-sexpr fun)(parse-sexpr arg1)(parse-sexpr arg2))])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV Symbol FLANG ENV]
  [FunV2 Symbol Symbol FLANG ENV])

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))
  

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (: NumV->number : VAL -> Number)
  (define (NumV->number v)
    (cases v
      [(NumV n) n]
      [else (error 'arith-op "expects a number, got: ~s" v)]))
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : FLANG ENV -> VAL)
;; evaluates FLANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(Num n) (NumV n)]
    [(Add l r) (arith-op + (eval l env) (eval r env))]
    [(Sub l r) (arith-op - (eval l env) (eval r env))]
    [(Mul l r) (arith-op * (eval l env) (eval r env))]
    [(Div l r) (arith-op / (eval l env) (eval r env))]
    [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id bound-body)
     (FunV bound-id bound-body env)]
    [(Fun2 bound-id1 bound-id2 bound-body)
     (FunV2 bound-id1 bound-id2 bound-body env)]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         [(FunV2 bound-id bound-id2 bound-body f-env)
          (error 'eval "expected two arguments, got one in: ~s"
                 fval)]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]

    
    [(Call2 fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
       [(FunV2 bound-id1 bound-id2 bound-body f-env)
        (eval bound-body
              (Extend bound-id2 (eval arg-expr2 env)
                                      (Extend bound-id1 (eval arg-expr1 env) f-env)))]
         [(FunV bound-id bound-body f-env)
          (error 'eval "expected a single argument, got two in: ~s"
                 fval)]
         [else  (error 'eval "`call' expects a function, got: ~s"
                       fval)]))]))


(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
(Extend '+ (FunV2 'x 'y (Add (Id 'x) (Id 'y)) (EmptyEnv))
          (Extend '- (FunV2 'x 'y (Sub (Id 'x) (Id 'y)) (EmptyEnv))
                  (Extend '* (FunV2 'x 'y (Mul (Id 'x) (Id 'y)) (EmptyEnv))
                          (Extend '/ (FunV2 'x 'y (Div (Id 'x) (Id 'y)) (EmptyEnv))(EmptyEnv))))))



(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))])
    (cases result
      [(NumV n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)])))

(test (run "5") => 5)
(test (run "{call + 5 5}") => 10)
(test (run "{with {x {call + 5 5}} {call + x x}}") => 20)
(test (run "{with {x {call + 5 5}} {call * x x}}") => 100)
(test (run "{with {x {call + 3 5}} {call / x 2}}") => 4)
(test (run "{with {x 5} {call + x x}}") => 10 )
(test (run "{with {x {call + 5 5}} {with {y {call - x 3}} {call + y y}}}") => 14)
(test (run "{with {x 5} {with {y {call - x 3}} {call + y y}}}") => 4)
(test (run "{with {x 5} {call + x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {call + x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {call + x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{with {x 1} y}") =error> "lookup: no binding for y")
(test (run "{with {6 {call + 5 5}} {call + x x}}") =error> "bad `with' syntax in")
(test (run "{call {fun {x} {call + x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {call + x 3}}}
                {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {call + x 3}}}
                {with {add1 {fun {x} {call + x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{call {fun {} {call + x 1}} 4}") =error> "parse-sexpr: bad `fun' syntax in"  )
(test (run "{call {x {4}} 4 }") =error> "parse-sexpr: bad syntax in")
(test (run "{with {add3 {fun {x} {call + x 3}}}
                {with {add1 3}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      =error> "eval: `call' expects a function, got:")
(test (run "{with {x 5} {call + x {with{add3 {fun {x} {call + x 3}}} add3}}}") =error> "expects a number, got")
(test (run "{call {call {fun {x} {call x 1}}
                          {fun {x} {fun {y} {call + x y}}}}
                    123}")
      => 124)

(test (run "{call {fun {x} {call + x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {call + x 3}}}
                {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {call + x 3}}}
                {with {add1 {fun {x} {call + x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
                {with {foo {fun {x} {call + x 1}}}
                  {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
                {with {f {fun {y} {call + x y}}}
                  {with {x 5}
                    {call f 4}}}}")
      => 7) ;; the example we considered for subst-caches
(test (run "{call {with {x 3}
                      {fun {y} {call + x y}}}
                    4}")
      => 7)
(test (run "{with {add3 {fun {x} {call + x 3}}} {with {x 3} add3}}") =error> "evaluation returned a non-number")
(test (run "{call + 4 5}") => 9)
 (test (run "{with {add3 {fun {x} {call + x 3}}}
 {call add3 1}}")
 => 4)
(test (run "{with {x 3}
 {with {f {fun {y} {call + x y}}}
 {with {x 5}
 {call f 4}}}}")
 => 7)
 (test (run "{call {fun {x y} {call + x { call - y 1}}} 4 2}") => 5)
 (test (run "{with {first {fun {x y} x}}
 {with {second {fun {x y} y}}
 {call first {call second 2 123} 124}}}")
 => 123)
 (test (run "{+ 4 5}") =error> "parse-sexpr: bad syntax in")
 (test (run "{* 4 5}") =error> "parse-sexpr: bad syntax in")
 (test (run "{with {add3 {fun {x} {call + x 3}}}
 {call add3 1 2}}")
 =error> "expected a single argument, got two in: ")
 (test (run "{with {add3 {fun {x stam} {call + x 3}}}
 {call add3 1}}")
 =error> "expected two arguments, got one in: ")
(test (run "{with {add3 {fun {x y} {call + x 3}}}
                {with {add1 3}
                  {with {x 3}
                    {call add1 {call add3 x} 2}}}}")
      =error> "eval: `call' expects a function, got:")