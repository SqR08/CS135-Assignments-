;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |the og version of 1a|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (p2? x)
 (odd? x))
 (define (p1? x)
 (< x 5))



;;(a)
(define (q1a x)
(cond
[(p2? x)
(cond
[(p1? x) 'left]
[else 'down])]
[else
(cond
[(p1? x) 'up]
[else 'right])]))

;;(b)
(define (q1b x)
(cond [(p1? x)
(cond [(p2? x)
(cond
[(p1? (+ x 1)) 'up]
[(p2? (* 2 x)) 'down]
[else 'right])]
[else
(cond
[(p2? 2) 'down]
[else 'up])])]
[(p1? 0)
(cond
[(p2? x) 'left]
[else 'right])]
[else 'down]))

;;(c)
(define (q1c x)
(cond
[(cond
[(p1? x) (p2? x)]
[else true])
'up]
[else 'down]))