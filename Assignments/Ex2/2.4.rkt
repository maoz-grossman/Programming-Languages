#lang pl


#|
Ex2.4::

<BINTREE>::{NODE <BINTREE> <BINTREE>}
           |{LEAF <num>}

|#

 (define-type BINTREE
   [Node  BINTREE  BINTREE]
   [Leaf Number])

(: tree-map : All(A) (Number -> Number) BINTREE -> BINTREE)
(define (tree-map F Btree)
  (cases Btree
    [(Node L R) (Node (tree-map F L) (tree-map F R)) ]
    [(Leaf num) (Leaf (F num))] ))

(: tree-fold : (All(A) (A A -> A) (Number -> A) BINTREE -> A))
;;It consumes a combiner function,
;;a numeric function, and an input BINTREE. It returns a value that is created
(define (tree-fold func nfunc Btree)
  (cases Btree
  [(Node L R) (func (tree-fold func nfunc L)(tree-fold func nfunc R))]
  [(Leaf num) (nfunc num) ]))

(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
 (tree-fold (inst append Number) (inst list Number)
tree))

  

(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse Btree)
   (: switch-nodes : BINTREE BINTREE -> BINTREE)
  (define (switch-nodes L_node R_node)
    (Node R_node L_node))
  (tree-fold switch-nodes Leaf Btree))


;;Tests;;
(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))=> '(1 2 3) )

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2)                                          
(Leaf 3))))=> (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

(test (tree-flatten (Node (Node (Node (Leaf 3) (Leaf 4))
(Node (Leaf 5) (Leaf 6))) (Node (Leaf 7) (Leaf 8))))=> '(3 4 5 6 7 8 ) )

(test (equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))
 (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))))=> #t )

(test (tree-reverse (Node (Leaf 2) (Leaf 3)))  =>  (Node (Leaf 3) (Leaf 2)))
(test (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))  => (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)))
