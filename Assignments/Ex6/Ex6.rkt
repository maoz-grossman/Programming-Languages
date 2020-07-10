#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect  <SOL>  <SOL>}
        |  { union  <SOL>  <SOL> } 
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET]
  [Smult Number SOL]
  [Inter SOL SOL]
  [Union SOL SOL]
  [Id    Symbol]
  ;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
  [Fun   Symbol Symbol SOL]
  [CallS SOL SOL SOL]
  [CallD SOL SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
;; checks if a number is an elment in the List
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5)))=> #t)
(test (not (ismember? 1 '( 3 2 3 5 6)))=> #t)
(test (ismember? 1 '(3 4 5 1 3 4))=> #t)
(test (ismember? 1 '(1))=> #t)

(: remove-duplicates : SET  -> SET)
;;returns the list without repetition of elements
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l) (rest l)) (remove-duplicates (rest l))]
        [else (cons (first l) (remove-duplicates (rest l)))]))

  
(: create-sorted-set : SET -> SET)
;; Returns sorted list in ascending order, and without repetition
(define (create-sorted-set l)
  (remove-duplicates (sort l <)))
  
(: set-union : SET SET -> SET)
;;Appends two list, removes repitations and sort the result 
(define (set-union A B) 
  (create-sorted-set (append A B)))

(: set-intersection : SET SET -> SET)
;;Finds the intrsection between two lists
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  ;; Returns a list with the elements of lst for which pred produces a true value.
  ;;The pred procedure is applied to each element from first to last.
  (create-sorted-set (filter mem-filter B)))

(test (set-union '(1 2 3 4 4 )'(4 5)) => '(1 2 3 4 5))
(test (set-union '(1 3 4 2 4 4 )'()) => '(1 2 3 4 ))
(test (set-intersection '(1 2 3 3 4 3 )'(3 4)) => '(3 4))
(test (set-intersection '(1 2 3 3 4 3 )'()) => '())



;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    ;;checks if it is a list of numbers if that so calls the Set consturctor to returns sol object
    [(list (number: ns) ...)  (Set(create-sorted-set  ns))] ;; sort and remove-duplicates
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        ;;like {with {<Symbol:name> {<Sol:named>} {SOL:body}}
        ;;-> {call {fun{name}{{SOL-body}} {arg:<Sol:named>}}}
        (CallS (Fun name name (parse-sexpr body)) (parse-sexpr named) (parse-sexpr named))] ;;; there is no With constructor replace with existing constructors
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name1) (symbol: name2)) body)
        (if (eq? name1 name2)
            (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice
            (Fun name1 name2 (parse-sexpr body)))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
    ;;Like '{call-static <function> arg1 arg2}'
    [(list 'call-static fun arg1 arg2)  (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    ;;Like '{call-dynamic <function> arg1 arg2}' 
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

    


(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

  
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters

  

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
              eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),envf))
                     if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
      = error! Otherwise
    eval({call-dynamic E-op E1 E2},env)
             =eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),env))
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise

env= general environment
envf= function environment
|#

;; Types for environments, values, and a lookup function

  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

  (define-type VAL
    [SetV SET]
    [FunV Symbol Symbol SOL ENV])

  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

  (: SetV->set : VAL -> SET)
    (define (SetV->set v)
      (cases v
        [(SetV S) S]
        [else (error 'SetV->set "expects a set, got: ~s" v)]))
  
  (: smult-set : Number VAL -> VAL)
  ;;multiply the Set with a Scalar(Number) 
  (define (smult-set n s)
    (: mult-op : Number -> Number)
    (define (mult-op k)
      (* k n))
    (SetV (map mult-op ( SetV->set s))))

 (: set-op : (SET SET -> SET) VAL VAL -> VAL )
  ;; gets a binary SET operator, and uses it within a SetV
  ;; wrapper
  (define (set-op op val1 val2)
     (SetV (op (SetV->set val1) (SetV->set val2))))

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
  ;; evaluates SOL expressions by reducing them to set values
  (define (eval expr env)
    (cases expr
      [(Set S) (SetV S)]
      [(Smult n set) (smult-set n (eval set env))]
      [(Inter l r) (set-op set-intersection (eval l env) (eval r env))]
      [(Union l r) (set-op set-union (eval l env) (eval r env))]
      [(Id name) (lookup name env)]
      [(Fun bound-id1 bound-id2 bound-body)
       (FunV bound-id1 bound-id2 bound-body env)]
      ;;eval for static type functions
      [(CallS fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
           (eval bound-body
                (Extend bound-id2 (eval arg-expr2 env)
                        (Extend bound-id1 (eval arg-expr1 env) f-env)))]
           [else (error 'eval "`call-static' expects a function, got: ~s"
                              fval)]))]
      ;;eval for dynamic functions
      [(CallD fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
           (eval bound-body
                (Extend bound-id2 (eval arg-expr2 env)
                        (Extend bound-id1 (eval arg-expr1 env) env)))]
           [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                              fval)]))]))


(: createGlobalEnv : -> ENV)
;;I'll save 3 functions in our envieorment: 
;;Globals Funv functions throughout the program
;;in a global ENV which will be recived from the function
;;so the user would have some premade functions to use
;;Here I added cons function which takes two Vals and returns a function (p)
;;we can use the recived function to execute other functions like first and second
(define (createGlobalEnv)
  (Extend 'second (eval(parse "{fun {p spare-param}{call-static p {fun{a b} b}{}}}") (EmptyEnv))
          (Extend 'first (eval(parse "{fun {p spare-param} {call-static p {fun{a b} a}{}}}") (EmptyEnv))
                  (Extend 'cons (eval (parse "{fun {f s} {fun {foo spare-param}{call-static foo f s}}}") (EmptyEnv))
                          (EmptyEnv)))))

  (: run : String -> (U SET VAL))
  ;; evaluate a SOL program contained in a string
  (define (run str)
    (let ([result (eval (parse str) (createGlobalEnv))])
       (cases result
         [(SetV S) S]
         [else result])))


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))


(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))

(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
 {with {foo {fun {x y} {intersect x y}}}
 {call-static p foo {}}}}")
 => '(2 3))

(test (SetV->set (eval (parse "{fun {x y} x}")(EmptyEnv)))=error> "SetV->set: expects a set, got")
 (test (run"{with {Call-static x 1 2 }}" ) =error> "parse-sexpr: bad `with' syntax in" )
(test (run "{with {x {}} {Call-static x 1 2 }}") =error> "parse-sexpr: bad syntax in" )
(test (run "{call-dynamic {1} {1} {1}}") =error> "eval: `call-dynamic' expects a function, got")
(test (lookup 'y (EmptyEnv)) =error> "lookup: no binding for" )
(test (run "{fun {x y} x}") => (FunV
 'x
 'y
 (Id 'x)
 (Extend
  'second
  (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'b)) (Set '())) (EmptyEnv))
  (Extend
   'first
   (FunV 'p 'spare-param (CallS (Id 'p) (Fun 'a 'b (Id 'a)) (Set '())) (EmptyEnv))
   (Extend 'cons (FunV 'f 's (Fun 'foo 'spare-param (CallS (Id 'foo) (Id 'f) (Id 's))) (EmptyEnv)) (EmptyEnv))))))


