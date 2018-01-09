;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 2 Problem 1
;; ***************************************************
;;

;; 1(a)

;;Defining predicates to test the code

;; (define (p2? x)
;; (odd? x))
;; (define (p1? x)
;; (< x 5))

(define (q1a x)
  (cond
  [ (and (p2? x) (p1? x)) 'left]
  [ (p2? x) 'down]
  [ (p1? x) 'up]
  [ else 'right])
  )

;; 1(b)

(define (q1b x)
  (cond
    [(and (p1? x) (p2? x) (p1? (+ x 1))) 'up]
    [(and (p1? x) (p2? x) (p2? (* 2 x))) 'down]
    [(and (p1? x) (p2? x)) 'right]
    [(and (p1? x) (p2? 2)) 'down]
    [(p1? x) 'up]
    [(and (p1? 0) (p2? x)) 'left]
    [(p1? 0) 'right]
    [else 'down]))

;; 1(c)

(define (q1c x)
  (cond
    [(and (p1? x) (p2? x)) 'up]
    [(not (p1? x)) 'up]
    [(and (p1? x) (not (p2? x))) 'down]))

     