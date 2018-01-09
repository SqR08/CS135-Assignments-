;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 04, Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a04lib.rkt")


;; The unit structure is defined in a04lib.rkt.  The require
;; statement, above, is all that's needed to have it take
;; effect here.  The following comment is here just so the
;; type definitions that follow make sense.

;; (define-struct unit (type serial))

;; -------- Q4a --------------
;; A Unit-Type is a (make-unit-type Sym)

;; A Unit is a (make-unit Sym Nat)

;; A Train is one of:
;; empty
;; (cons Unit (listof Unit))


;; -------- Q4b --------------

;; string->train works by using the built in string->list function as a wrapper
;; function to convert a string into a list of the characters (unit-types)
;; within. This list of unit-types is used by the main function loc-> train
;; which takes the list of unit-types and a list of serial numbers
;; (default serial numbers) to make a list of units which is a train.

;;4c)
;;(headed-by? mytrain myunit-type determines if the first unit
;;of a train (mytrain) is the given unit-type (myunit-type)
;;headed-by? Train Unit-Type -> Bool
;;Examples
(check-expect (headed-by? empty 'L) false)
(check-expect (headed-by? (cons (make-unit 'L 2) empty) 'L) true)

(define (headed-by? mytrain myunit-type)
  (cond
    [(empty? mytrain) false]
    [(symbol=? (unit-type (first mytrain)) myunit-type) true]
    [else false]
    ))

;;Tests:
(check-expect (headed-by? (cons (make-unit 'L 2) empty) 'P) false)


;;4d)
;;(ends-with-caboose? mytrain) determine if there is exactly
;;one caboose and it is the last unit of a train (mytrain)
;;ends-with-caboose?: Train -> Bool
;;Examples:
(check-expect (ends-with-caboose? empty) false)
(check-expect (ends-with-caboose? (cons (make-unit 'C 2) empty)) true)

(define (ends-with-caboose? mytrain)
  (cond
    [(empty? mytrain) false]
    [(symbol=? (unit-type (first mytrain)) 'C)
       (cond
         [(empty? (rest mytrain)) true] 
         [else false])]
    [else (ends-with-caboose? (rest mytrain))]
    ))
    
;;Tests
(check-expect (ends-with-caboose? (cons (make-unit 'D 2) empty)) false)
(check-expect (ends-with-caboose? (cons (make-unit 'C 2)
                                       (cons (make-unit 'C 5) empty))) false)

;;4e)
;;(remove-unit t s) removes the unit of a train (t)
;;which has serial number (s)
;;remove-unit: Train Nat -> Train
;;Examples:
(check-expect (remove-unit empty 5) empty)
(check-expect (remove-unit (cons (make-unit 'B 2)
                                 (cons (make-unit 'C 5) empty)) 5)
                                 (cons (make-unit 'B 2) empty))

(define (remove-unit t s)
 (cond
   [(empty? t) empty]
   [(not (= (unit-serial (first t)) s))
     (cons (first t) (remove-unit (rest t) s))]
   [(= (unit-serial (first t)) s) (remove-unit (rest t) s)]
   ))

;;Tests:
(check-expect (remove-unit (cons (make-unit 'L 2) empty) 2) empty)

;;4f)

;;(iscar? mytrain) determines if the first unit of a
;;train (mytrain) is a car
;;iscar?: Train -> Bool
;;Examples:
(check-expect (iscar? (cons (make-unit 'P 2) empty)) true)

(define (iscar? mytrain)
 (or (symbol=? (unit-type (first mytrain)) 'P)
     (symbol=? (unit-type (first mytrain)) 'T)
     (symbol=? (unit-type (first mytrain)) 'B)))

;;(proper-train? mytrain) determines if a train (mytrain)
;;is a proper train, meaning that it is a Train with
;;zero or more locomotives followed by zero or more cars
;;followed by zero or more cabooses
;;proper-train? Train -> Bool
;;Examples:
(check-expect (proper-train? empty) true)
(check-expect (proper-train? (cons (make-unit 'L 2)
                                   (cons (make-unit 'P 3)
                                         (cons (make-unit 'C 5) empty)))) true)

(define (proper-train? mytrain)
  (cond
    [(empty? mytrain) true]
    [(symbol=? (unit-type (first mytrain)) 'L)
      (cond
        [(empty? (rest mytrain)) true]
        [ (or (symbol=? (unit-type (first (rest mytrain))) 'L)
              (iscar? (rest mytrain))
              (symbol=? (unit-type (first (rest mytrain))) 'C))
                  (proper-train? (rest mytrain))]
        [else false])]
    [(iscar? mytrain)
     (cond
        [(empty? (rest mytrain)) true]
        [ (or (iscar? (rest mytrain))
              (symbol=? (unit-type (first (rest mytrain))) 'C))
                  (proper-train? (rest mytrain))]
        [else false])]
    [(symbol=? (unit-type (first mytrain)) 'C) 
    (cond
        [(empty? (rest mytrain)) true]
        [(symbol=? (unit-type (first (rest mytrain))) 'C) 
         (proper-train? (rest mytrain))]
        [else false])]
     [else
      (cond
           [(empty? (rest mytrain)) false]
           [else (proper-train? (rest mytrain))])]
    ))

;;Tests:
(check-expect (proper-train? (cons (make-unit 'L 2)
                                         (cons (make-unit 'L 5) empty))) true)
(check-expect (proper-train? (cons (make-unit 'L 2)
                                         (cons (make-unit 'P 5) empty))) true)
(check-expect (proper-train? (cons (make-unit 'L 2) empty)) true)
(check-expect (proper-train? (cons (make-unit 'L 2)
                                         (cons (make-unit 'D 5) empty))) false)


(check-expect (proper-train? (cons (make-unit 'P 2)
                                         (cons (make-unit 'P 5) empty))) true)
(check-expect (proper-train? (cons (make-unit 'P 2)
                                         (cons (make-unit 'C 5) empty))) true)
(check-expect (proper-train? (cons (make-unit 'P 2) empty)) true)
(check-expect (proper-train? (cons (make-unit 'P 2)
                                         (cons (make-unit 'L 5) empty))) false)

(check-expect (proper-train? (cons (make-unit 'C 2)
                                         (cons (make-unit 'C 5) empty))) true)
(check-expect (proper-train? (cons (make-unit 'C 2) empty)) true)
(check-expect (proper-train? (cons (make-unit 'C 2)
                                         (cons (make-unit 'D 5) empty))) false)
(check-expect (proper-train? (cons (make-unit 'D 2)
                                         (cons (make-unit 'D 5) empty))) false)



