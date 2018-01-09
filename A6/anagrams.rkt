;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname anagrams) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 06, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;2a)
;;(sort-chars listofchar) sorts a list of characters
;;in alphabetical order
;;sort-chars: (listof chars) -> (listof chars)
;;Examples:
(check-expect (sort-chars empty) empty)
(check-expect (sort-chars (list #\d #\o #\g)) (list #\d #\g #\o))

(define (insert char sortedcharlist)
  (cond
    [(empty? sortedcharlist) (cons char empty)]
    [(char<=? char (first sortedcharlist))(cons char sortedcharlist)]
    [else (cons (first sortedcharlist) (insert char (rest sortedcharlist)))]
    ))

(define (sort-chars listofchar)
  (cond
    [(empty? listofchar) empty]
    [else (insert (first listofchar)(sort-chars (rest listofchar)))]
    ))

;;Tests:
(check-expect (sort-chars (string->list "bob")) (string->list "bbo"))
(check-expect (sort-chars (string->list "ashley")) (string->list "aehlsy"))

   
;;2b)
;;(anagrams/sort? string1 string2) determines if two strings
;;are anagrams of each other
;;anagrams/sort?: Str Str -> Bool
;;Examples:
(check-expect (anagrams/sort? "listen" "silent") true)
(check-expect (anagrams/sort? "dad" "dad") true)

(define (anagrams/sort? string1 string2)
 (cond
   [(equal? (sort-chars (string->list string1))
             (sort-chars (string->list string2))) true]
   [else false]
   ))

;;Tests:
(check-expect (anagrams/sort? "dad" "mom") false)
(check-expect (anagrams/sort? "esketit" "steven") false)

;;2c)

;;(freq-count/acc mylist myitem count-so-far) counts the
;;number of times an item (myitem) appears in a list (mylist)
;;freq-count/acc: (listof Any) Any Nat -> Nat 
(define (freq-count/acc mylist myitem count-so-far)
 (cond
   [(empty? mylist) count-so-far]
   [(equal? (first mylist) myitem) (freq-count/acc
                                    (rest mylist) myitem
                                    (+ 1 count-so-far))]
   [else (freq-count/acc (rest mylist) myitem count-so-far)]
   ))

;;(listbuilder mylist newlist) creates a new list of pairs
;;(new list) with the elements in mylist and the number of times
;;they appear in mylist
;;listbuilder: (listof Any) (listof Any) -> (listof Any)
(define (listbuilder mylist newlist)
  (cond
    [(empty? mylist) newlist]
    [(alreadycounted? (first mylist) newlist)
     (listbuilder (rest mylist) newlist)]
    [else
     (listbuilder (rest mylist) 
      (cons (cons (first mylist) (list (freq-count/acc mylist
                                                 (first mylist) 0))) newlist ))]
    ))

;;(alreadycounted? element newlist) checks if an element
;;has already been counted and attached into newlist
;;alreadycounted? Any (listof Any) -> Bool
(define (alreadycounted? element newlist)
  (cond
    [(empty? newlist) false]
    [(equal? element (first (first newlist))) true]
    [else (alreadycounted? element (rest newlist))]
  ))
         
;;(freq-count mylist) produces a list containing the number
;;of times an element appears in a given list (mylist)
;;freq-count mylist: (listof Any) -> (listof Any)
;;Examples:
(check-expect (freq-count empty) empty)
(check-expect (freq-count '(1 1 2 2)) '((2 2)(1 2)))

(define (freq-count mylist)
  (listbuilder mylist empty))

;;2d)
;;(inlist? element list) checks if an element is in a list
;;inlist?: Any (listof Any) -> Bool
(define (inlist? element list)
  (cond
    [(empty? list) false]
    [(equal? element (first list)) true]
    [else (inlist? element (rest list))]
    ))

;;(freq-equiv? list1 list2) determines if two lists are
;;rearrangements of each other
;;freq-equiv? (listof (list Any Nat) (listof (list Any Nat) -> Bool
;;Examples:
(check-expect (freq-equiv? empty empty) true)
(check-expect (freq-equiv? '((red 2) (blue 3)) '((blue 3) (red 2))) true)

(define (freq-equiv? list1 list2)
  (cond
    [(or (empty? list1)(empty? list2)) true]
    [(not (inlist? (first (freq-count list1)) (freq-count list2))) false]
    [else (freq-equiv? (rest (freq-count list1)) (freq-count list2))]

    ))

;;Tests:
(check-expect (freq-equiv? '(1 2 3) '(2 3 4)) false)
(check-expect (freq-equiv? '((red 2) (blue 3)) '((red 3)(blue 2))) false)

;;2e)
;;(anagrams/count? string1 string2) determines if two strings are anagrams
;;of each other
;;anagrams/count?: Str Str -> Bool
;;Examples:
(check-expect (anagrams/count? "listen" "silent") true)
(check-expect (anagrams/count? "dad" "dad") true)

(define (anagrams/count? string1 string2)
  (cond
    [(freq-equiv? (freq-count (string->list string1))
                  (freq-count (string->list string2))
                  ) true]
    [else false]
    ))

;;Tests:
(check-expect (anagrams/count? "dad" "mom") false)
(check-expect (anagrams/count? "esketit" "steven") false)
