#lang pl

#|////////////////
       EX1
/////////////////|#

#|Checks if a string ends with a given substring|#
(: stringEndWith : String String -> Boolean)
(define (stringEndWith S1 S2)
  (let ([S1Len (string-length S1)]
        [S2Len (string-length S2)])
    (cond[(< S1Len S2Len) #f];;Is S1 ends with S2?
        [else (string=? (substring S1 (- S1Len S2Len)) S2)])))


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


;;Checks if a List of Strings contains a string with the suffix of 'pl'
(: HasPlSuffix : (Listof String) -> Boolean)
(define ( HasPlSuffix ls )
  (if
   (and (= (listLength ls) 1)(false? (stringEndWith (first ls) "pl" )) )
      #f
      (or (stringEndWith (first ls) "pl" ) (HasPlSuffix (rest ls)))
   );;end if
  );;end define

;; Returns the first string from the list that contains the given substring as a suffix
(: getStringContainSubstring : (Listof String) String  Integer -> String )
(define (getStringContainSubstring ls str i)
  (cond
     [(stringEndWith (list-ref ls i) str) (list-ref ls i)]
     [(>= i (- (listLength ls) 1) ) "nada"]
     [else ( getStringContainSubstring ls str (+ i 1))]
      );;end cond
  );;end define


#| Consumes a list of strings
and returns the first string that contains the string "pl" as a suffix – if one such
exists, and returns #f otherwise|#
(: plSuffixContained : (Listof String) -> ( U String #f) )
(define ( plSuffixContained ls)
  (cond
    [(eq? (HasPlSuffix ls) #f) #f]
    [else  (getStringContainSubstring ls "pl" 0) ]
    );;end cond
  );;end define





#|////////////
     TESTS
//////////////|#

#|Ex1 Tests|#
(test(stringEndWith "yoyo" "yo") => #t)
(test(stringEndWith "yoyo" "y") => #f)
(test(stringEndWith "y" "yo") => #f)
(test (listLength (list "abba" "buba" 3 4 'c 453) ) => 6)
(test (listLength '() ) => 0)
(test (HasPlSuffix (list "abba" "buba" "polvo" "lomlpl" "ppop" "klkl") ) => #t)
(test (HasPlSuffix '("pllp" "plyy" "ppp" "lpTT" "lol")) => #f)
(test (HasPlSuffix '("pllp" "plPL" "ppp" "lpTT" "lol")) => #f)
(test (HasPlSuffix '("pl" "plyy" "PlPl" "lpTT" "lol")) => #t)
(test (HasPlSuffix '("yyyt" "TplT" "plTT" "PlPl" "plplpl")) => #t)
(test ( getStringContainSubstring '("pl" "plyy" "PlPl" "lpTT" "lol") "pl" 0) => "pl")
(test ( getStringContainSubstring '("plyy" "PlPl" "lpTT" "lol") "pl" 0) => "nada")
;;  plSuffixContained
(test( plSuffixContained '("y" "yo")) => #f)
(test( plSuffixContained '("pllp" "plyy" "ppp" "lpTT" "lol")) => #f)
(test( plSuffixContained '("pllp" "plPL" "ppp" "lpTT" "lol")) => #f)
(test( plSuffixContained '("pl" "plyy" "PlPl" "lpTT" "lol")) => "pl")
(test( plSuffixContained '("yyyt" "TplT" "plTT" "PlPl" "plplpl")) => "plplpl")
(test( plSuffixContained '("yyyt" "TplT" "" "PlPl" "plplpl")) => "plplpl")

