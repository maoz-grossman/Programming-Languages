#lang pl

#|///////////
     Ex3
////////////|#

#| In a similar way we immplied the Chomsky normal form in class(lecture 3),
  I tried to make a context-free grammar for the KeyStack:
  KeyStack -> Empty , Push
  Push -> Symbol String KeyStac
  A sentence is complited only after we reach to Empty
|#
(define-type KeyStack
  [ EmptyKS ] ;;keyStack -> Empty 
  [Push Symbol String KeyStack] ;; KeyStack -> Push -> Symbol String KeyStack
  );;end define


#|
 The pop-stack was actually easier than search-stack, so I start with it/
 There are only two possible cases for that one:
1. we asked to pop an Empty KeyStack, and we cant execute that so we return false
2. we asked to pop the head of the stack and the stack is not allready empty,
   so we pop the head and return the rest of the stack
|#
(: pop-stack : KeyStack -> (U KeyStack Boolean))
(define (pop-stack KS)
  (cases KS
    [( EmptyKS ) #f];;The Stack is Empty
    [(Push sym str ks) ks]);; The is not Empty so we return the second KeyStack the head of the stack obtains
  );;end define

#|
The search-stack is pretty similar to the previous function
only here we look for a specific value
so we need to iterates all the 'cells' of the stack untill we find what we
are looking for, or we reache the end of the stack (aka the stack is Empty)
|#
(: search-stack : Symbol KeyStack -> (U Boolean String) )
(define (search-stack symb KS)
  (cases KS
  [(EmptyKS) #f];; if we reached the end of the stack and we didnt find the value return false
  [(Push sym str ks);; If didnt reach the end of the stack, check if the value is in the stack's cell
   (if (eq? sym symb)
       str
       (search-stack symb ks))];; if not (else) check the next cell recursively
    );; end cases
  );;end define



#|////////////
    TESTs
/////////////|#

(test (EmptyKS) => (EmptyKS))

(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>
 (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))
=> (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))

(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A"
(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))

(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'a "A" (EmptyKS))) => (EmptyKS))

(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => #f)

(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a
"A" (EmptyKS))))) => "AAA")
(test (search-stack 'e (Push 'a "AAA" (Push 'b "B" (Push 'c "C" (Push 'd "D" (Push 'f "F" (Push 'e "you did it!!" (EmptyKS))))))))=> "you did it!!" )
(test (search-stack 'g (Push 'a "AAA" (Push 'b "B" (Push 'c "C" (Push 'd "D" (Push 'f "F" (Push 'e "you did it!!" (EmptyKS))))))))=> #f )

