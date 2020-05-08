#lang pl
#|

2.2.1
The problem  with such expression:
{* {+ {set 1} {set 2}} get}
is that this expression is ambiguity,
that is you can get two diffrent resoults from the same expression.
The value of 'get may be 6 or 3 depends on the 'set operations order.
A possible solution may be to require the BNF to be unambgiuous:
<MAE> ::= <AE>      
 | { <operator> set <MAE> <MAE> }
 | { <operator>  <MAE> get }
 | { set <MAE> }
 | get
<operator> ::=  + | - | * | /
The AE is the same one we saw in class which includes <num>.
Now we can't sets values  without predefine order.
the expression will become:
{* {+ set 1 {set 2}} get}



2.2.2
  <MAE>::={seq {set <AE>} <Second>}
          |{seq <AE>}

  <Second>::= <GAE>
             |{set <GAE>}<Second>
             
   <GAE>::=<AE>
          |{<operator> <GAE> <GAE>}
          | get

   <operator>::= + | - | * | /


1. A MAE expression is a non-empty sequence of sub-expressions. The
main expression is wrapped with curly brackets.
Indeed: "{seq {set ... }" ,"{set <AE>}" ,"{set <GAE>}"  and "{<operator> <GAE> <GAE>}" 

2. now we can't operate get at first, we have to use <AE*>
and if we want to use get we must define it first with 'set
that is we must use
 {seq {set <AE>}     <Second>}
so The first sub-expression does not contain a 'get operation.

3.The last sub-expression does not contain a 'set operation,
because we concatenate <Second> to it:
{set <GAE>}<Second>

4. Each sub-expression – apart from the last one – is wrapped with curly
brackets and starts with a set operation.
(except from a cases like {seq {- {/ 16 2} 2 }} which is considered valid by the assignment definition)

5. Each sub-expression may contain multiple operations (similar to the AE
expressions), but only a single set operation (appearing as the leftmost
operation).
6. The set operator has exactly one operand.
yes by "{set <GAE>}" and the definition of <GAE>


|#


