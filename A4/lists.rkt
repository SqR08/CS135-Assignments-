;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 4 Problem 2
;; ***************************************************
;;

;;2a)
;;(sum-positive loi) produces the sum of the positive integers in a
;;list of integers (loi)
;;sum-positive: listof Int -> Nat
;;Examples:
(check-expect (sum-positive empty) 0)
(check-expect (sum-positive (cons 5 (cons -3 (cons 4 empty)))) 9)

(define (sum-positive loi)
  (cond
    [(empty? loi) 0]
    [else (+
          (cond
            [(> (first loi) 0) (first loi)]
            [(not (> (first loi) 0)) 0])
            (sum-positive (rest loi)))]
    ))

;;Tests:
(check-expect (sum-positive (cons -3 (cons -3 (cons -3 empty)))) 0)
(check-expect (sum-positive (cons 3 (cons 3 (cons 3 empty)))) 9)

;;2b)
;;(contains? elem loa) determine whether an element (elem) is
;;in a list (loa)
;;contains?: Any listof Any -> Bool
;;Examples:
(check-expect (contains? empty empty) false)
(check-expect (contains? 5 (cons 5 empty)) true)

(define (contains? elem loa)
 (cond
   [(empty? loa) false]
   [(equal? elem (first loa)) true]
   [else  (contains? elem (rest loa))]
   ))

;;Tests: 
(check-expect (contains? 'string (cons 5 (cons 'sym empty))) false)

;;2c)
;;(has-duplicate? loa) determines whether any element in a list
;appears more than once
;;has-duplicate?: listof Any -> Bool
;;Examples:
(check-expect (has-duplicate? empty) false)
(check-expect (has-duplicate? (cons 5 (cons 5 empty))) true)

(define (has-duplicate? loa)
 (cond
   [(empty? loa) false]
   [(contains? (first loa) (rest loa)) true]
   [else (has-duplicate? (rest loa))]
   ))

;;Tests:
(check-expect (has-duplicate? (cons 5 (cons 3 (cons 5 empty)))) true)
(check-expect (has-duplicate? (cons 5 (cons 3 (cons 2 empty)))) false)

;;2d)
;;(keep-ints loa) produces a list that contains only the integers
;;of a given list
;;keepints: listof Any ->listof Int
;;Examples:
(check-expect (keep-ints empty) empty)
(check-expect (keep-ints (cons 5 (cons 'bob (cons 3 empty))))
              (cons 5 (cons 3 empty)))

(define (keep-ints loa)
 (cond
   [(empty? loa) empty]
   [(integer? (first loa)) (cons (first loa) (keep-ints (rest loa)))]
   [else (keep-ints (rest loa))]
   ))

;;Tests:
(check-expect (keep-ints (cons 5 (cons 4 (cons 3 empty))))
              (cons 5 (cons 4 (cons 3 empty))))
(check-expect (keep-ints (cons 'test (cons 'test empty))) empty)              