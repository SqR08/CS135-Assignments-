;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname settheory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 5 Problem 1
;; ***************************************************
;;

;; A NumSet is a (listof Num)
;; requires: the numbers are strictly increasing

;;1a)
;;(union set1 set2) produces the union of the 2
;;sets of numbers
;;union: NumSet NumSet -> NumSet
;;Examples:
(check-expect (union empty empty) empty)
(check-expect (union empty '(1 2)) '(1 2))

(define (union set1 set2)
  (cond
    [(and (empty? set1) (empty? set2)) empty]
    [(and (cons? set1) (empty? set2)) set1]
    [(and (empty? set1) (cons? set2)) set2]
    [(and (cons? set1)(cons? set2))
    (cond[(= (first set1)(first set2))
          (cons (first set1) (union (rest set1) (rest set2)))]
         [(< (first set1)(first set2))
          (cons (first set1)(union (rest set1) set2))]
         [else (cons (first set2) (union set1 (rest set2)))]
     )]
    ))

;;Tests:
(check-expect (union '(1 2) empty) '(1 2))
(check-expect (union '(1 2) '(2 3)) '(1 2 3))
(check-expect (union '(1 2) '(3 4)) '(1 2 3 4)) 
(check-expect (union '(2 3) '(1 2)) '(1 2 3))

;;1b) 
;;(intersection set1 set2) produces the intersection
;;of the 2 sets of numbers
;;intersect: NumSet NumSet -> NumSet
;;Examples:
(check-expect (intersection empty empty) empty)
(check-expect (intersection empty '(1 2)) empty)

(define (intersection set1 set2)
  (cond
    [(not (and (cons? set1)(cons? set2))) empty]
    [(and (cons? set1)(cons? set2))
     (cond
       [(= (first set1)(first set2))
       (cond
        [(empty? (rest set1)) (cons (first set1) empty)]
        [else (cons (first set1) (intersection (rest set1)(rest set2)))])]
        
       [(< (first set1)(first set2)) (intersection (rest set1) set2)]
       [else (intersection set1 (rest set2))]
       )]
    ))

;;Tests:
(check-expect (intersection '(1 2) '(3 4 5)) empty)
(check-expect (intersection '(1 2) empty) empty)
(check-expect (intersection '(1 2) '(1 2)) '(1 2))
(check-expect (intersection '(3 4) '(2 3)) '(3))
(check-expect (intersection '(1) '(1 2)) '(1))

;;1c)
;;(difference set1 set2) produces the difference
;;between 2 sets of numbers
;;difference: NumSet NumSet -> NumSet
;;Examples:
(check-expect (difference empty empty) empty)
(check-expect (difference '(1 2) empty) '(1 2))

(define (difference set1 set2)
 (cond
   [(empty? set1) empty]
   [(empty? set2) set1]
   [(and (cons? set1)(cons? set2))
    (cond
      [(= (first set1)(first set2)) 
        (cond
          [(empty? (rest set2))(rest set1)]
          [else (difference (rest set1) (rest set2))])]
      [(< (first set1)(first set2))
        (cond
          [(empty? (rest set1)) (cons (first set1) empty)]
          [else (cons (first set1) (difference (rest set1) set2))])]    
      [else
        (cond
          [(empty? (rest set2)) set1]
          [else (difference set1 (rest set2))])]
      )]
   ))

;;Tests:
(check-expect (difference '(1 2) '(2 3)) '(1))
(check-expect (difference '(1 2 4) '(2 3)) '(1 4))
(check-expect (difference '(1) '(2 3)) '(1))
(check-expect (difference '(3 4) '(1)) '(3 4))
(check-expect (difference '(3 4) '(1 2)) '(3 4))

;;1d) 
;;(symmetric-difference set1 set2) consumes 2
;;numsets (set1 and set2) and produces a numset
;;containing all the numbers that are in one set
;;or the other but not both
;;symmetric-difference NumSet NumSet -> NumSet
;;Examples:
(check-expect (symmetric-difference empty empty) empty)
(check-expect (symmetric-difference empty '(1 2)) '(1 2))

(define (symmetric-difference set1 set2)
  (cond
    [(and (empty? set1)(empty? set2)) empty]
    [(and (empty? set1)(cons? set2)) set2]
    [(and (cons? set1)(empty? set2)) set1]
    [else
       (difference (union set1 set2) (intersection set1 set2))]
    ))
(check-expect (symmetric-difference '(2 3) empty) '(2 3))  
(check-expect (symmetric-difference '(1 2) '(2 3)) '(1 3))
(check-expect (symmetric-difference '(4 5) '(1 2 3 4)) '(1 2 3 5))

             
             

