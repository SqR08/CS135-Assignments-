;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 1 Problem 3
;; ***************************************************
;;

;; 3(a) (final-cs135-grade m1 m2 f a) consumes the first midterm grade,
;;   the second midterm grade, the final exam grade, and the overall 
;;   assignment grade and produces the final grade in the course

;; final-cs135-grade m1 m2 f a: Num Num Num Num -> Num
;; requires 0 >= m1 m2 f a <= 100 

;;Example:
(check-expect (final-cs135-grade 70 65 82 80) 779/10)
(check-expect (final-cs135-grade 80 75 86 90) 847/10)

(define (final-cs135-grade m1 m2 f a)
  ( / (+ (+ (+ (+ (* m1 10) (* m2 20))
        (* f 45)) (* a 20)) (* 5 100)) 100))

;;Tests:
(check-expect (final-cs135-grade 67 85 99 93) 1837/20)
(check-expect (final-cs135-grade 0 0 0 0) 5)

;; 3(b) (cs-135-final-exam-grade-needed m1 m2 a) consumes the first midterm
;;        grade the second midterm gade, and the overall assignment grade 
;;          and produces the minimum final exam grade needed to pass the course

;; cs-135-final-exam-grade-needed m1 m2 a: Num Num Num -> Num
;; requires 0 >= m1 m2 a <= 100

;;Example:
(check-expect (cs135-final-exam-grade-needed 50 60 60) 520/9)
(check-expect (cs135-final-exam-grade-needed 40 30 45) 80)

(define (cs135-final-exam-grade-needed m1 m2 a)
  ( / (- (- (- (- 6000 (* 10 m1))
      (* 20 m2)) (* 20 a))
         (* 5 100)) 45))

;;Tests:
(check-expect (cs135-final-exam-grade-needed 0 0 0) 1100/9)
(check-expect (cs135-final-exam-grade-needed 99 99 99) 110/9)

