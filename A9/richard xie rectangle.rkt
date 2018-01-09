;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |richard xie rectangle|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruiqi Xie (20714366)
;; CS135 Fall 2017
;; Assignment 09, Question 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "rectanglelib.rkt")

(define-struct cell (num used?))
;; A Cell is a (make-cell Nat Bool)

;; A Grid is a (listof (listof Cell))
;; requires: the grid contains a non-empty list of non-empty lists,
;;  all the same length.

(define-struct rect (x y w h))
;; A Rect is a (make-rect Nat Nat Nat Nat)

(define-struct state (grid rects))
;; A State is a (make-state Grid (listof Rect))


;; Here are a couple of constants that can be used to define
;; the puzzle in the assignment, and a random larger puzzle.

(define puzz '((0 0 0 0 0 5 0)
               (0 0 0 0 0 2 2)
               (0 3 0 6 3 2 0)
               (4 0 0 0 0 0 0)
               (0 0 0 4 0 4 0)
               (2 0 6 0 2 4 0)
               (0 0 0 0 0 0 0)))

(define big-puzz '((4 0 7 0 0 0 0 0 0 0 0 21 0)
                   (0 3 2 0 0 0 0 0 0 0 0 0 2)
                   (0 0 0 0 0 0 0 2 3 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 5)
                   (0 2 0 0 0 0 0 4 0 0 0 0 0)
                   (0 0 3 0 0 0 0 0 0 0 0 0 0)
                   (3 0 0 0 0 5 2 4 0 0 0 0 0)
                   (0 0 0 0 0 2 0 6 0 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 24 0)
                   (0 0 0 0 4 0 4 0 0 0 4 0 0)
                   (0 0 3 0 0 0 0 0 0 0 8 0 2)))

;; Question a)

;; (map2d f listoflov) consumes a function and a list of list of values
;;   and produces a new list of list of values, where f has been
;;   applied to every value
;; map2d: (X->Y) (listof (listof X)) -> (listof (listof X))
;; Examples:
; how about the empty case?
(check-expect (map2d add1 '((1 2 3 4) (5 6))) '((2 3 4 5) (6 7)))
(check-expect (map2d (lambda (x) (+ 0.5 x)) '((1 1) (1 2))) '((1.5 1.5) (1.5 2.5)))

(define (map2d f listoflov)
  (map (lambda (x) (map f x)) listoflov))

;; Tests:
(check-expect (map2d floor '((1.5 2.5 3.6) (3.902348))) '((1 2 3) (3)))
(check-expect (map2d (lambda (x) (* x 0)) '((234 234 234) (1342))) '((0 0 0) (0)))

;; Question b)

;; (construct-puzzle listoflon) consumes a listof (listof Nat) and produces an
;;    initial state
;; construct-puzzle: (listof (listof Nat)) -> State
;; Examples:
(check-expect (construct-puzzle '((0 5) (8 9)))
              (make-state (list (list (make-cell 0 false) (make-cell 5 false))
                                (list (make-cell 8 false) (make-cell 9 false)))
                          empty))

(define (construct-puzzle listoflon)
  (make-state (map2d (lambda (x) (make-cell x false)) listoflon) empty))

;; Tests:
(check-expect (construct-puzzle '((10 20) (1 0)))
              (make-state (list (list (make-cell 10 false) (make-cell 20 false))
                                (list (make-cell 1 false) (make-cell 0 false)))
                          empty))
(check-expect (construct-puzzle '((5 6) (7 8)))
              (make-state (list (list (make-cell 5 false) (make-cell 6 false))
                                (list (make-cell 7 false) (make-cell 8 false)))
                          empty))

;; Question c)

;; (solved? state) consumes a state and produces true is the puzzle is
;;   fully solved, and false otherwise
;; solved?: State -> Bool

(define (solved? state)
  (local [(define (select-used loc)
            (cond [(empty? loc) true]
                  [(false? (select-used (first loc))) false]
                  [(list? (first loc)) (select-used (rest loc))]
                  [(false? (cell-used? (first loc))) false]
                  [else (select-used (rest loc))]))]
            (select-used (state-grid state)))) 

;; Question d)

;; (get-first-unused grid) consumes a grid and produces the coordinates of
;;    the topmost, leftmost cell that isn't used
;; Grid -> (list Nat Nat)

(define (get-first-unused grid)
  (local [(define (find-row grid x)
            (local [(define result (find-column (first grid) y 0))]
              (cond [(false? result) (find-row (rest grid) (add1 y))]
                    [else result])))]
    (find-row grid 0)))


;; Question e)

;; (neighbours state) consumes a state and produces a list of states that
;;    might follow the given state after adding a rectangle
;; neighbours: State -> (listof State)

;;  (define (neighbours state)
;;  (

;; Question f)

;; (solve-rectangle-puzzle listoflon) consumes a description of an initial
;;   puzzle and attempts to solve the puzzle. It either produces a list of
;;   rectangles that solve the puzzle, and false if there is no solution.
;; solve-rectangle-puzzle: (listof (listof Nat))
;;                         -> (anyof (listof Rect) false)


