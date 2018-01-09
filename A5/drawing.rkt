;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname drawing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 05, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the data definitions and functions in
;; the file drawinglib.rkt.
(require "drawinglib.rkt")

;; A demonstration of the drawing in the assignment.
(define samplepic (list
                   (make-square135 (make-posn 0 0) 50 '(255 0 0))
                   (make-square135 (make-posn 50 50) 50 '(0 0 255))
                   (make-circle135 (make-posn 50 50) 25 '(0 255 0))))

;; Type this line into the interactions window to see the picture:
;; (draw-picture samplepic 100 100)

;; --------------------------
;; Place your functions here!
;; --------------------------

;;3a)
(define example-drawing
  (list
   (make-square135 (make-posn 50 50) 50 '(255 255 0))
   (make-square135 (make-posn 75 75) 50 '(255 255 0))
   (make-square135 (make-posn 100 100) 50 '(255 255 0)) 
   (make-circle135 (make-posn 50 50) 25 '(255 255 0))
   (make-circle135 (make-posn 75 75) 25 '(255 255 0))
   (make-circle135 (make-posn 100 100) 25 '(255 255 0))
))

;;3b)
;;(cull mydrawing m n) produces a new drawing containing
;;the first m squares and the first n circles in the
;;original drawing
;;cull: Drawing Nat Nat -> Drawing
;;Examples:
(check-expect (cull example-drawing 1 2)
   (list
   (make-square135 (make-posn 50 50) 50 '(255 255 0))
   (make-circle135 (make-posn 50 50) 25 '(255 255 0))
   (make-circle135 (make-posn 75 75) 25 '(255 255 0))))
(check-expect (cull example-drawing 1 0)
   (list (make-square135 (make-posn 50 50) 50 (list 255 255 0))))            

(define (cull mydrawing m n)
  (cond
    [(and (= m 0) (= n 0)) empty]
    [(and (= m 0) (square135? (first mydrawing)))
      (cull (rest mydrawing) m n)]
    [(and (= n 0) (circle135? (first mydrawing)))
      (cull (rest mydrawing) m n)]
    [(and (not (= m 0)) (square135? (first mydrawing)))
      (cons (first mydrawing) (cull (rest mydrawing) (- m 1) n))]
    [(and (not (= n 0))(circle135? (first mydrawing)))
      (cons (first mydrawing) (cull (rest mydrawing) m (- n 1)))]
   ))

;;Tests:
(check-expect (cull example-drawing 2 1)
  (list (make-square135 (make-posn 50 50) 50 (list 255 255 0))
        (make-square135 (make-posn 75 75) 50 (list 255 255 0))
        (make-circle135 (make-posn 50 50) 25 (list 255 255 0)))) 

;;3c)
;;(maxwdith n) computes the maxwidth that
;;rings can have given the number of rings (n)
;;maxwidth: Nat -> Num
(define (maxwidth n)
  (/ 100 n))

;;(draw-ring n maxwidth) draws n number of rings
;;with width of maxwidth
;;draw-ring: Nat Num -> Drawing
(define (draw-ring n maxwidth)
 (cond
  [(= n 0) empty]
  [(even? n)
  (cons (make-circle135 (make-posn 100 100) (* n maxwidth) '(255 0 0))
       (draw-ring (- n 1) maxwidth))]
  [(odd? n)
    (cons (make-circle135 (make-posn 100 100) (* n maxwidth) '(255 255 255))    
       (draw-ring (- n 1) maxwidth))]
  ))


;;(bullseye n) consumes a number n and produces a
;;drawing n alternatating red and white discs
;;centred on the point (100, 100)
;;bullseye: Nat -> Drawing
(define (bullseye n)
  (draw-ring n (maxwidth n)))

;3d)
(define (buildcolumns rows columns columncount c1 c2) ;;COLUMNCOUNT COUNTS WITHIN THE COLUMN (IE KEEPS GOING 1 ROW DOWN EACH TIME)
(cond
  [(= columncount columns) empty]
  [else (append (buildrows rows columncount 0 c1 c2)
                (buildcolumns rows columns (add1 columncount) c1 c2))]
  ))

(define (buildrows rows columns rowcount c1 c2)       
(cond
  [(= rowcount rows) empty] 
  [(even? (+ columns rowcount))
               (cons (make-square135 (make-posn (* 10 rowcount) (* 10 columns)) 10 c1)
                     (buildrows rows columns (add1 rowcount) c1 c2))]
  [else
   (cons (make-circle135 (make-posn (+ 5 (* 10 rowcount)) (+ 5 (* 10 columns))) 5 c2)
                      (buildrows rows columns (add1 rowcount) c1 c2))]
  ))


;;(checkerboard n c1 c2) produces a checkerboard
;;with n rows and n columns that alternates between
;;colours c1 and c2
;;checkerboard Nat Colour Colour -> Drawing
(define (checkerboard n c1 c2)
  (buildcolumns n n 0 c1 c2))


