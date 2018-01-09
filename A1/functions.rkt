;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 1 Problem 1
;; ***************************************************
;;


;; 1(a) Manhattan distance 
(define (distance x1 y1 x2 y2)
  ( + (abs (- x1 x2)) (abs (- y1 y2))))

;; 1(b) Stirling's upper bound
(define (Stirling n)
  ( * (expt n (+ n 1/2)) (exp(- 1 n))))

;; 1(c) Logit
(define (logit p)
  (log (/ p (- 1 p))))

;; 1(d) Even temperment
(define (freq base-frequency interval)
  (* base-frequency (expt 2 (/ interval 12)))) 

;; 1(e) Black-Scholes formula
(define (d1 maturity rate volatility spot-price strike-price)
  (* (/ 1 (* volatility (integer-sqrt maturity)))
  (* maturity (+ rate (/ (expt volatility 2) 2)
                 (log (/ spot-price strike-price)))))) 

;; 1(f) Ballistic motion
(define (height initial-velocity time)
(- (* initial-velocity time) (* 1/2 9.8 (sqr time))))


               