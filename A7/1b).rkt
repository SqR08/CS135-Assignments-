;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |1b)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
	(define x "Pancakes")
(define (f x y z)
  (local
    [(define x 7)
     (define y (+ x 2))
     (define z (- x 2))]
    (* y z)))
(f x x x)