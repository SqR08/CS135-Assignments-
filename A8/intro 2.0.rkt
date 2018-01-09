;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |intro 2.0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;2a)
;;(keep-ints mylist) produces a list that contains only the integers
;;of a given list
;;keepints: listof Any ->listof Int
;;Examples:
(check-expect (keep-ints empty) empty)
(check-expect (keep-ints (list 1 3 6 'bob)) (list 1 3 6))

(define (keep-ints mylist)
  (filter integer? mylist))

;;2b)
;;(contains? elem mylist) determine whether an element (elem) is
;;in mylist
;;contains?: Any listof Any -> Bool
;;Examples:
(check-expect (contains? 5 empty) false)
(check-expect (contains? 5 (list 5)) true)

(define (contains? elem mylist)
  (cond [(not (empty? (filter (lambda (x) (equal? elem x)) mylist))) true]
        [else false]))

;;2c)
;;(lookup-al k alst) produces the value corresponding to key k,
;;or false if k not present
;;lookup-al: Num (list of (list Num Str) → (anyof Str false)
;;Examples:
(check-expect (lookup-al 10 (list (list 10 5) (list 11 5))) 5)
(check-expect (lookup-al 9 (list (list 10 5) (list 11 5))) false)

(define (lookup-al k alst)
  (cond [(empty? (filter (lambda (x) (equal? k (first x))) alst)) false]
        [else (second (first (filter (lambda (x) (equal? k (first x))) alst)))]))

;;2d)
;;(extract-keys myAL) produces a list of all the keys
;;in myAL
;;extract-keys: AL -> (listof Num)
;:Examples:
(check-expect (extract-keys (list (list 10 5) (list 11 5))) (list 10 11))
(check-expect (extract-keys (list (list 4 5) (list 6 5))) (list 4 6))

(define (extract-keys myAL)
  (map (lambda (x) (first x)) myAL))

;;2e)
;;(sum-positive listofint) produces the sum of the positive integers in 
;;listofint
;;sum-positive: (listof Int) -> Nat
;;Examples:
(check-expect (sum-positive (list 5 -3 4)) 9)
(check-expect (sum-positive (list -3 -3)) 0)

(define (sum-positive listofint)
  (foldr + 0 (filter (lambda (x) (> x 0)) listofint)))

;;2f)
;; (countup-to n b) produces a list from n...b
;; countup-to: Int Int → (listof Int)
;; requires: n <= b
;;Examples:
(check-expect (countup-to 6 6) (list 6))
(check-expect (countup-to 6 8) (list 6 7 8))

(define (countup-to n b)
  (build-list (+ (- b n) 1) (lambda (x) (+ n x))))

;;2g)
;;(shout listofstrings) produces listofstrings
;;but with every string in the list capitalized
;;shout: (listof Str) -> (listof Str)
;;Examples:
(check-expect (shout '("get" "off" "my" "lawn")) '("GET" "OFF" "MY" "LAWN"))
(check-expect (shout '("get" "off" "my" "bed")) '("GET" "OFF" "MY" "BED"))

(define (shout listofstrings)
(map list->string (map (lambda (x) (map (lambda (y) (char-upcase y)) x)) (map string->list listofstrings))))

;;2h)
;;(make-validator mylist) produces a predicate function
;;that consumes a single item and determines if the item
;;appears in mylist
;;Examples:
(check-expect ((make-validator '(red blue green)) 'red) true)
(check-expect ((make-validator '(red blue green)) 'yellow) false)

(define (make-validator mylist)
  (local [(define (predicate item)
          (member? item mylist))]

    predicate))



