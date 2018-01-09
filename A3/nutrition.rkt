;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 3 Problem 2
;; ***************************************************
;;

(define-struct nutri-fact (name serving fat carbs sugar protein))
;; A Nutri-Fact is a (make-nutri-fact Str Num Num Num Num Num)
;; requires: 0 < serving
;; fat + carbs + protein <= serving
;; 0 <= sugar <= carbs
;; 0 <= fat, protein

;;2a)
;;my-nutri-fact-fn: Nutri-fact -> Any
(define (my-nutri-fact-fn nutri-info)
  (... (nutri-fact-name nutri-info)...
       (nutri-fact-serving nutri-info)...
       (nutri-fact-fat nutri-info)...
       (nutri-fact-carbs nutri-info)...
       (nutri-fact-sugar nutri-info)...
       (nutri-fact-protein nutri-info)...))

;;2b)
;;(scale-fn old-serving new-serving) consumes an old-serving and
;; a new-serving and produces a number that scales the old-serving
;; to the new serving
;;scale-fn: Nutri-fact Num -> Num
(define (scale-fn old-serving new-serving)
 (/ new-serving (nutri-fact-serving old-serving)))
;;Examples
(check-expect (scale-fn (make-nutri-fact "cheerios" 90 90 90 90 90) 180) 2)
(check-expect (scale-fn (make-nutri-fact "apples" 70 90 30 60 50) 35) 1/2)

;;(resize old-info new-serving) consumes old-info for a Nutri-fact
;; and a new-serving and produces a new Nutri-fact with the new
;; serving size an all of the other numerical nutrient fields
;; changed to reflect the new serving size
;;resize: Nutri-fact Num -> Nutri-fact
;;Examples
(check-expect (resize (make-nutri-fact "cheerios" 90 90 90 90 90) 180)
              (make-nutri-fact "cheerios" 180 180 180 180 180))
(check-expect (resize (make-nutri-fact "apples" 70 90 30 60 50) 35)
              (make-nutri-fact "apples" 35 45 15 30 25))

(define (resize old-info new-serving)
  (make-nutri-fact
       (nutri-fact-name old-info)
       new-serving
       (* (scale-fn old-info new-serving) (nutri-fact-fat old-info))
       (* (scale-fn old-info new-serving) (nutri-fact-carbs old-info))
       (* (scale-fn old-info new-serving) (nutri-fact-sugar old-info))
       (* (scale-fn old-info new-serving) (nutri-fact-protein old-info))))

;:Tests
(check-expect (resize (make-nutri-fact "cheerios" 90 90 90 90 90) 270)
              (make-nutri-fact "cheerios" 270 270 270 270 270))
(check-expect (resize (make-nutri-fact "apples" 70 90 30 60 50) 140)
              (make-nutri-fact "apples" 140 180 60 120 100))

;;2c)

;;Defining constants for number of calories in a gram of a nutrient
(define caloriesinfat 9)
(define caloriesincarbs 4)
(define caloriesinprotein 4)

;;calories nutri-info) consumes a nutri-fact (nutri-info) and
;;produces the number of calories there are in a serving
;;calories: Nutri-fact -> Num
;;Examples
(check-expect (calories (make-nutri-fact "cheerios" 90 90 90 90 90)) 1530)
(check-expect (calories (make-nutri-fact "apples" 70 90 30 60 50)) 1130)

(define (calories nutri-info)
     (+
       (* caloriesinfat (nutri-fact-fat nutri-info))
       (* caloriesincarbs (nutri-fact-carbs nutri-info))
       (* caloriesinprotein (nutri-fact-protein nutri-info))))

;;Tests
(check-expect (calories (make-nutri-fact "cheerios" 80 80 75 90 90)) 1380)
(check-expect (calories (make-nutri-fact "apples" 10 10 150 38 42)) 858)

;;2d)
;;(choose-for-diet nutri-info1 nutri-info2) consumes 2 nutri-facts 
;;(nutro-info1 and nutri-info2) and produces the one that is most
;;appropriate for your friend's diet based on a set of preferences
;;choose-for-diet: Nutri-fact Nutri-fact -> Nutri-Fact
;;Examples
(check-expect (choose-for-diet (make-nutri-fact "Candy" 90 80 30 45 30)
                               (make-nutri-fact "Chicken" 90 80 30 10 55))
                               (make-nutri-fact "Chicken" 90 80 30 10 55))
(check-expect (choose-for-diet (make-nutri-fact "Strawberry" 79 80 30 5 30)
                               (make-nutri-fact "Fries" 45 80 45 10 55))
                               (make-nutri-fact "Strawberry" 79 80 30 5 30))
               
(define (choose-for-diet nutri-info1 nutri-info2)
 (cond
   [(< (/ (nutri-fact-sugar nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-sugar nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info1]
   [(> (/ (nutri-fact-sugar nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-sugar nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info2]
   [(> (/ (nutri-fact-protein nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-protein nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info1]
   [(< (/ (nutri-fact-protein nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-protein nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info2]
   [(< (/ (nutri-fact-carbs nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-carbs nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info1]
   [(> (/ (nutri-fact-carbs nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-carbs nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info2]
   [(< (/ (nutri-fact-fat nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-fat nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info1]
   [(> (/ (nutri-fact-fat nutri-info1) (nutri-fact-serving nutri-info1))
       (/ (nutri-fact-fat nutri-info2) (nutri-fact-serving nutri-info2)))
       nutri-info2]
   ))

;;Tests
(check-expect (choose-for-diet (make-nutri-fact "Candy" 90 80 30 45 30)
                               (make-nutri-fact "Chicken" 90 80 30 45 55))
                               (make-nutri-fact "Chicken" 90 80 30 45 55))
(check-expect (choose-for-diet (make-nutri-fact "Strawberry" 79 80 30 90 30)
                               (make-nutri-fact "Fries" 79 80 45 90 10))
                               (make-nutri-fact "Strawberry" 79 80 30 90 30))
(check-expect (choose-for-diet (make-nutri-fact "Candy" 90 80 30 45 55)
                               (make-nutri-fact "Chicken" 90 80 20 45 55))
                               (make-nutri-fact "Chicken" 90 80 20 45 55))
(check-expect (choose-for-diet (make-nutri-fact "Strawberry" 79 10 30 90 30)
                               (make-nutri-fact "Fries" 79 80 45 90 30))
                               (make-nutri-fact "Strawberry" 79 10 30 90 30))
(check-expect (choose-for-diet (make-nutri-fact "Candy" 90 80 20 45 55)
                               (make-nutri-fact "Chicken" 90 70 20 45 55))
                               (make-nutri-fact "Chicken" 90 70 20 45 55))
(check-expect (choose-for-diet (make-nutri-fact "Strawberry" 79 10 45 90 30)
                               (make-nutri-fact "Fries" 79 80 45 90 30))
                               (make-nutri-fact "Strawberry" 79 10 45 90 30))

;;2e)
;;(valid-nutri-fact? nutri-info) consumes an arbritrary value (nutri-info)
;;and produces true if it is a valid nutri-fact according to the
;;data definition
;;valid-nutri-fact?: Any -> Bool
;;Examples
(check-expect (valid-nutri-fact? 'cheese) false)
(check-expect (valid-nutri-fact? true) false)

(define (valid-nutri-fact? nutri-info)
  (nutri-fact? nutri-info))

;;Tests
(check-expect (valid-nutri-fact? (make-nutri-fact "Strawberry" 79 10 45 90 30))
                                 true)
(check-expect (valid-nutri-fact? (make-nutri-fact "Fries" 79 80 45 90 30))
                                                  true)

         