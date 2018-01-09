;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 08, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;2a)
(define (keep-ints mylist)
  (filter integer? mylist))

;;2b)
(define (contains? elem mylist)
  (cond
    [(not (empty? (filter (lambda (x) (equal? x elem))  mylist))) true]
    [else false]
    ))

;;2c)

;;Examples:
(check-expect (lookup-al 10 (list (list 10 5) (list 11 5))) 5)

(define (lookup-al k alst)
  (second (first (filter (lambda (x) (equal? (first x) k)) alst))))

;;2d)

;:Examples:
(check-expect (extract-keys (list (list 10 5) (list 11 5))) (list 5 5))

(define (extract-keys myAL)
  (map secondfn myAL))

;;Function that takes the second of a list
(define (secondfn mylist)
(second mylist))


;;2e)
;;Examples:
(check-expect (sum-positive (list 5 -3 4)) 9)
                          
(define (sum-positive listofint)
  (foldr + 0 (filter positive? listofint)))

;;2f)
;;Examples:
(check-expect (countup-to 6 8) (list 6 7 8))

(define (countup-to n b)
  (map (lambda (x) (+ (- x 1) n)) (build-list (+ (- b n) 1) add1)))

;;2g)
;;Examples:
(check-expect (shout '("get" "off" "my" "lawn")) '("GET" "OFF" "MY" "LAWN"))

(define (shout listofstrings)
 (cond
   [(empty? listofstrings) empty]
   [else (cons (shout2 (string->list (first listofstrings)))
         (shout (rest listofstrings)))]
   ))

(define (shout2 listofcharacters)
  (list->string (map char-upcase listofcharacters)))

;;2h)
;;Examples:

(define (make-validator mylist)
  (local
    [(define (predicate item)
     (contains? item mylist))]
    predicate))





        