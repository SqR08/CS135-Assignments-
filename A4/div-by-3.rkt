;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname div-by-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 4 Problem 3
;; ***************************************************
;;

;;3a)
;;A Nat3 is one of:
;;0, 1, 2
;;(add3 Nat)

;;3b)
(define (my-nat3-fn n)
  (cond
    [(zero? n)...]
    [(= 1 n)...]
    [(= 2 n)...]
    [else (... (my-nat-fn (- n 3))...)]))

;;3c)
;;(div-by-3? n) determines a Nat3 (n) and determines whether
;;its divisible by 3
;;(div-by-3?: Nat3 -> Bool
;;Examples:
(check-expect (div-by-3? 0) true)
(check-expect (div-by-3? 4) false)

(define (div-by-3? n)
  (cond
    [(zero? n) true]
    [(= 1 n) false]
    [(= 2 n) false]
    [else  (div-by-3? (- n 3))]))

;;Tests:
(check-expect (div-by-3? 3) true)
(check-expect (div-by-3? 5) false)
  