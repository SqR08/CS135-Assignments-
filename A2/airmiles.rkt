;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname airmiles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 2 Problem 2
;; ***************************************************
;;

;; (calc-airmiles dollars card-type store-type) determines the number of
;; airmiles earned for a given purchase given $ of dollars spent(dollars),
;; card-type(card-type), sponsor type(store-type)
;; calc-airmiles dollars card-type sponsor?: Num Sym Bool -> Num)
;; requires dollars > 0, card-type either 'standard or'premium
;; sponsor? either true or false
;; Examples
(check-expect (calc-airmiles 30 'standard #f) 1)
(check-expect (calc-airmiles 25 'premium #t) 2)


(define (calc-airmiles dollars card-type sponsor?)
  (cond
    [ (and sponsor? (symbol=? card-type 'standard))
           (floor (/ dollars 15))]
    [ (and sponsor? (symbol=? card-type 'premium))
           (floor (/ dollars 10))]
    [ (and (not sponsor?) (symbol=? card-type 'standard))
           (floor (/ dollars 20))]
    [ (and (not sponsor?) (symbol=? card-type 'premium))
           (floor (/ dollars 15))] ))

;;Tests
(check-expect (calc-airmiles 10.72 'standard #t) 0)
(check-expect (calc-airmiles 22 'standard #f) 1)
(check-expect (calc-airmiles 5 'premium #t) 0)
(check-expect (calc-airmiles 40 'premium #f) 2)