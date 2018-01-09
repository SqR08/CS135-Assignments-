;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 2 Problem 3
;; ***************************************************
;;

;; (weighted-exam-calc midterm1 midterm2 final)
;; calculates your weighted exam grade given your first midterm mark
;; second midterm mark and final exam mark
(define (weighted-exam-calc midterm1 midterm2 final)
 ( / (+ (* midterm1 10) (* midterm2 20) (* final 50)) 80))
;; weighted-exam-calc midterm1 midterm2 final: Num Num Num -> Num
;; Examples
(check-expect (weighted-exam-calc 90 60 50) 115/2)
(check-expect (weighted-exam-calc 40 40 90) 285/4)

;; (final-cs135-grade-calc midterm1 midterm2 final assign p) calculates your
;; final grade based on your midterm1 mark, midterm2 mark, final exam mark
;; assignments mark and participation mark
(define (final-cs135-grade-calc midterm1 midterm2 final assign partic)
    ( / (+ (+ (+ (+ (* midterm1 10) (* midterm2 20))
        (* final 45)) (* assign 20)) (* partic 5)) 100))
;;final-cs135-grade-calc midtermidterm1 midterm2 final assign partic:
;;Num Num Num Num Num -> Num
;; Examples
(check-expect (final-cs135-grade-calc 80 80 80 80 80) 80)
(check-expect (final-cs135-grade-calc 95 88 87 83 92) 1749/20)

;; (final-cs135-grade midterm1 midterm2 final assign particip) computes
;;  your final grade based on your your weighted exam mark of your midterm1
;; exam, midterm2 exam, and final exam mark, and your assignment mark,
;; and participation mark

;; final-cs135-grade midtermidterm1 midterm2 final assign partic:
;; Num Num Num Num Num -> Num
;; requires 0 >= midtermidterm1 midterm2 final assign partic <= 100

;; Examples
(check-expect (final-cs135-grade 70 65 82 80 100) 779/10)
(check-expect (final-cs135-grade 70 65 82 40 100) 46)

(define (final-cs135-grade midterm1 midterm2 final assign partic)
(cond 
 [(not (or (< assign 50) (< (weighted-exam-calc midterm1 midterm2 final) 50)))
  (final-cs135-grade-calc midterm1 midterm2 final assign partic)]

  [(or (< assign 50) (< (weighted-exam-calc midterm1 midterm2 final) 50))
    (cond
      [ (< (final-cs135-grade-calc midterm1 midterm2 final assign partic) 46)
           (final-cs135-grade-calc midterm1 midterm2 final assign partic)]
      [ else 46]
      )]
  ))

;;Tests
(check-expect (final-cs135-grade 90 95 90 87 100) 909/10)
(check-expect (final-cs135-grade 100 100 100 20 100) 46)
(check-expect (final-cs135-grade 50 46 30 20 100) 367/10)