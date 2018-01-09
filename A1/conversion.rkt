;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 1 Problem 2
;; ***************************************************
;;

;; 2(a)
;; (m/s->mph m/s) consumes a speed in the units m/s and produces the same
;;   speed in units of mph

;; m/s->mph: Num -> Num
;; requires m/s >= 0

;; Example:
(check-expect (m/s->mph 2) 6250/1397)
(check-expect (m/s->mph 20) 62500/1397)

;; Defining constants
(define s/h 3600) ; Defining number of seconds in an hour
(define m/mile 1609.344) ; Defining amount of metres in a mile

(define (m/s->mph m/s)
 (/ (* m/s s/h) m/mile))

;;Tests:
(check-expect (m/s->mph 0) 0)
(check-expect (m/s->mph -2) -6250/1397)


;; 2(b)
;; (mph->S/mfn mph) consumes a speed in the units mph and produces the same
;;   speed in units of S/mfn

;; mph->S/mfn: Num -> Num
;; requires mph >= 0

;; Example:
(check-expect (mph->S/mfn 2) 1064448/1675)
(check-expect (mph->S/mfn 5) 532224/335)

;; Defining constants
(define h/s (/ 1 s/h))   ; Definining hours per second as the inverse of seconds
                         ; per hour as defined in 2(a)
(define s/mfn 1209.6)    ; Defining the number of seconds per milifortnight
(define m/smoots 1.7018) ; Defining the number of metres per smoot

(define (mph->S/mfn mph)
  (/ (* mph h/s s/mfn m/mile) m/smoots))

;;Tests:
(check-expect (mph->S/mfn 0) 0)
(check-expect (mph->S/mfn 1/2) 266112/1675)



;; 2(c)
;; (mpg->L/100km mpg) consumes a fuel efficiency in mpg and produces the
;;   same efficiency in units of L/100km

;; mpg->L/100km: Num -> Num
;; requires mpg > 0

;; Example:
(check-expect (mpg->L/100km 2) 112903/960)
(check-expect (mpg->L/100km 5) 112903/2400)

;; Defining constants
(define g/L (/ 1 3.785411784)) ;Defining number of gallons in a litre
(define 100km/m (/ 1 100000))  ;Defining number of 100km in a metre

(define (mpg->L/100km mpg)
  (/ 1 (* mpg m/mile g/L 100km/m))) 

;;Tests:
(check-expect (mpg->L/100km -2) -112903/960)
(check-expect (mpg->L/100km 3/4) 112903/360)
