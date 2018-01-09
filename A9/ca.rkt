;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ca) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 09, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;1a)
;;(apply-rule a b c r) applies the rule r to the row of
;;cells a b c, and produces the resulting cell
;;apply-rule: (anyof 0 1) (anyof 0 1) (anyof 0 1) Nat -> Nat
;;requires: 0 <= r <= 255
;;Examples:
(check-expect (apply-rule 1 1 1 86) 0)
(check-expect (apply-rule 1 1 0 86) 1)

(define (apply-rule a b c r)
  (cond [(odd? (floor (/ r (expt 2 (config a b c))))) 1]
        [else 0])) 
  
(define (config a b c)
  (cond [(and (= a 1) (= b 1) (= c 1)) 7]
        [(and (= a 1) (= b 1)) 6]
        [(and (= a 1) (= c 1)) 5]
        [(= a 1) 4]
        [(and (= b 1) (= c 1)) 3]
        [(= b 1) 2]
        [(= c 1) 1]
        [else 0]))

;;Tests:
(check-expect (apply-rule 1 0 1 86) 0)
(check-expect (apply-rule 1 0 0 86) 1)

                                      
;;1b)
;;(next-row row rule) produces a new row of squares
;;with the rule appleid to every square of the row
;;next-row: (listof (anyof 0 1)) Nat -> (listof (anyof 0 1))
;;requires: 0 <= r <= 255
;;Examples:
(check-expect (next-row (list 1) 86) (list 1))
(check-expect (next-row (list 1 0) 86) (list 1 1))

(define (next-row row rule)
  (next-row2 (append (list 0) row (list 0)) rule))

(define (next-row2 row rule)
  (cond [(empty? (rest (rest (rest row))))
        (list (apply-rule (first row) (second row) (third row) rule))]
        [else (cons (apply-rule (first row) (second row) (third row) rule)
                    (next-row2 (rest row) rule))]))

;;Tests:
(check-expect (next-row (list 0 1) 86) (list 1 1))
(check-expect (next-row (list 1 0 1) 86) (list 1 0 1))

;;1c)
;;(iterate f b n) produces a list containing the results of
;;applying a function f (n-1) times to b
;;iterate: (Any -> Any) Any Nat -> (listof Any)
;;requires: n > 0
;;Examples:
(check-expect (iterate sqr 2 4) (list 2 4 16 256))
(check-expect (iterate add1 0 3) (list 0 1 2))

(define (iterate f b n)
  (cond [(= n 1) (list b)]
        [else (cons b (iterate f (f b) (sub1 n)))]
        ))

;;Tests:
(check-expect (iterate sub1 3 4) (list 3 2 1 0))


;;1d)
;;(run-automaton row rule n) produces a list of n generations
;;of rows with the rule applied to each subsequent row
;run-automaton: (anyof 0 1) Nat Nat -> (listof (anyof 0 1))
;;Examples:
(check-expect (run-automaton (list 1 0 1) 86 1) (list (list 1 0 1)))
(check-expect (run-automaton (list 1 0 1) 100 2)
              (list (list 1 0 1) (list 1 1 1)))
 
(define (run-automaton row rule n)
  (iterate (lambda (x) (next-row row rule)) row n))

;;Tests:
(check-expect (run-automaton (list 1 0 1 0) 69 5)
              (list (list 1 0 1 0) (list 1 0 1 0) (list 1 0 1 0)
                    (list 1 0 1 0) (list 1 0 1 0)))