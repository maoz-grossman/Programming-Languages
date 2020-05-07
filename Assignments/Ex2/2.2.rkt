#lang pl
#|
The problem  with such expression:
{* {+ {set 1} {set 2}} get}
is that this expression is ambiguity,
that is you can get two diffrent resoults from the same expression.
The value of 'get may be 6 or 3 depends on the 'set operations order.
A possible solution may be to require the BNF to be unambgiuous:
<MAE> ::= <WAE>      
 | { <operator> set <MAE> <MAE> }
 | { <operator>  <MAE> get }
 | { set <MAE> }
 | get
|<operator> ::=  + | - | * | /
The WAE is the same one we saw in class which includes <num>.
Now we can't sets values  without predefine order.
the expression will become:
{* {+ set 1 {set 2}} get}
|#


