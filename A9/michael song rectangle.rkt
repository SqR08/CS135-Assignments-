;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Michael Song (20705416)
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

(define test-puzz '((2 0 0 2)
                    (2 0 2 0)))

(define test-puzz1 '((0 0 0 0 0 5 0)
                     (0 0 0 0 0 2 2)))
  


;; Part a)

;; (map2d f my-nested-list) maps f onto all elements inside the lists of my-nested-list
;; map2d: (X -> Y) (listof (listof X)) -> (listof (listof Y))
;; Example:
(check-expect (map2d add1 '((3 4 5) (10 9 8))) '((4 5 6) (11 10 9)))

(define (map2d f my-nested-list)
  (map (lambda (x) (map f x)) my-nested-list))

;; Test:
(check-expect (map2d sqr '((1 2 3) (4 5 6))) '((1 4 9) (16 25 36)))


;; Part b)

;; (construct-puzzle nested-num-list produces a State
;;   representing the initial State of a puzzle with no
;;    rectangles in it
;; construct-puzzle: (listof (listof Nat)) -> State
;; requires: the listof Nat are all equal length
;; Example:
(check-expect (construct-puzzle test-puzz)
             (make-state (list (list (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false))
                               (list (make-cell 2 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false)))
                          empty))

(define (construct-puzzle nested-num-list)
  (local [;; (set-up n) produces an unused cell with the number n
          ;; set-up: Nat -> Cell
          (define (set-up n) (make-cell n false))]
  (make-state (map2d set-up nested-num-list) empty)))

;; Test:
(check-expect (construct-puzzle test-puzz1)
              (make-state (list
                           (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 5 false) (make-cell 0 false))
                           (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 2 false)))
                          empty))


;; Part c)

;; (solved? my-state) produces true if all the cells in my-state are used
;;  and false otherwise
;; solved?: State -> Bool
;; Example:
(check-expect (solved? (make-state (list (list (make-cell 2 true) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false))
                                         (list (make-cell 2 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false))) empty))
              false)

;; Note: the listof Rectangles in the States should technically not be empty, but it does not affect the computation

(define (solved? my-state)
  (local [;; (fully-used? my-cell-list) produces true if all the cells in my-cell-list
          ;;   are used and false otherwise
          ;; fully-used?: (listof Cells) -> Bool
          (define (fully-used? my-cell-list)
            (cond [(empty? my-cell-list) true]
                  [(false? (cell-used? (first my-cell-list))) false]
                  [else (fully-used? (rest my-cell-list))]))
          ;; (fully-used-grid? my-grid) produces true if all the cells in my-grid
          ;;   are used and false otherwise
          ;; fully-used?: Grid -> Bool
          (define (fully-used-grid? my-grid)
            (cond [(member? false (map fully-used? my-grid)) false]
                  [else true]))]
  (fully-used-grid? (state-grid my-state))))

;; Test:
(check-expect (solved? (make-state (list (list (make-cell 2 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true))
                                         (list (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))) empty))
              true)


;; Part d)

;; (get-first-unused my-grid) finds the topmost, leftmost
;;  unused cell in my-grid (with at least one unused cell)
;;   and produces a list of the x and y coordinates of that cell
;; get-first-unused: Grid -> (listof Nat)
;; Example:
(check-expect (get-first-unused (list (list (make-cell 2 true) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false))
                                      (list (make-cell 2 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false))))
                                (list 1 0))

(define (get-first-unused my-grid)
(local [(define (get-first-unused/acc my-grid acc/x acc/y)
          (local [;; (fully-used? my-cell-list) produces true if all the cells in my-cell-list
                  ;;   are used and false otherwise
                  ;; fully-used?: (listof Cells) -> Bool
                  (define (fully-used? my-cell-list)
                    (cond [(empty? my-cell-list) true]
                          [(false? (cell-used? (first my-cell-list))) false]
                          [else (fully-used? (rest my-cell-list))]))
                  ;; (first-used-cell my-cell-list) provides a list of the x and
                  ;;   y coordinates of the first used cell in my-cell-list
                  ;; first-used-cell: (listof Cells) -> (listof Nat)
                  ;; requires: there exists a used cell in my-cell-list
                  (define (first-used-cell/acc my-cell-list acc/x y)
                    (cond [(equal? false (cell-used? (first my-cell-list)))
                           (list acc/x y)]
                          [else (first-used-cell/acc (rest my-cell-list) (add1 acc/x) y)]))]  
            (cond [(fully-used? (first my-grid))
                   (get-first-unused/acc (rest my-grid) 0 (add1 acc/y))]
                  [else (first-used-cell/acc (first my-grid) 0 acc/y)])))]
  (get-first-unused/acc my-grid 0 0)))

;; Test:
(check-expect (get-first-unused (list (list (make-cell 2 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true))
                                         (list (make-cell 2 true) (make-cell 0 true) (make-cell 2 false) (make-cell 0 true))))
              (list 2 1))


;; Part e)

(define test-state (make-state
                    (list (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
                          (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                          (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                          (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                          (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                          (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
                          (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
                    (list (make-rect  0 0 1 4)
                          (make-rect  1 0 1 3)
                          (make-rect  2 0 2 1)
                          (make-rect  4 0 2 1)
                          (make-rect  6 0 1 7))))

(define small-test-state (make-state
                          (list (list (make-cell 2 true) (make-cell 0 false) (make-cell 0 false))
                                (list (make-cell 0 true) (make-cell 0 false) (make-cell 0 false))
                                (list (make-cell 0 true) (make-cell 3 true) (make-cell 0 true)))
                          (list (make-rect 0 0 1 2)
                                (make-rect 0 2 3 1))))

(define smaller-test-state (make-state
                            (list (list (make-cell 0 false) (make-cell 0 false))
                                  (list (make-cell 1 true) (make-cell 0 false)))
                            (list (make-rect 0 1 1 1))))
                                 

;; (dimensions my-grid) produces a list of the width and the height of my-grid
;; dimensions: Grid -> (list Nat Nat)
;; Example:
(check-expect (dimensions (list (list (make-cell 2 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true))
                                (list (make-cell 2 true) (make-cell 0 true) (make-cell 2 false) (make-cell 0 true))))
              (list 4 2))

(define (dimensions my-grid)
  (list (length (first my-grid)) (length my-grid)))

;; Test:
(check-expect (dimensions (list (list (make-cell 2 true) (make-cell 0 true) (make-cell 0 true)))) (list 3 1))


;; (shrink-dimensions my-grid) produces the dimensions of a grid assuming that
;;  the top-left corner of the first unused cell in my-grid is the origin of the new-grid
;; shrink-dimensions: Grid -> (list Nat Nat)
;; Example:
(check-expect (shrink-dimensions (list (list (make-cell 2 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true))
                                       (list (make-cell 2 true) (make-cell 0 true) (make-cell 2 false) (make-cell 0 true))))
              (list 2 1))

(define (shrink-dimensions my-grid)
  (list (- (first (dimensions my-grid)) (first (get-first-unused my-grid)))
        (- (second (dimensions my-grid)) (second (get-first-unused my-grid)))))

;; Test:
(check-expect (shrink-dimensions (list (list (make-cell 2 true) (make-cell 0 true))
                                       (list (make-cell 0 false) (make-cell 2 true))
                                       (list (make-cell 0 false) (make-cell 1 true))
                                       (list (make-cell 2 true) (make-cell 0 true))))
              (list 2 3))


;; (make-lists m n) makes a list of lists of two natural numbers;
;; the first element containing 1.. 2...m, the second element containing 1...n
;; make-lists: Nat Nat -> (listof (list Nat Nat))
;; Example:
(check-expect (make-lists 2 3)
              (list (list 2 3) (list 1 3) (list 2 2) (list 1 2) (list 2 1) (list 1 1)))

(define (make-lists m n)
  (make-lists/acc m n m))

(define (make-lists/acc m n m/acc)
  (cond [(= n 0) empty]
        [(= m/acc 0) (make-lists/acc m (sub1 n) m)]
        [else (cons (list m/acc n) (make-lists/acc m n (sub1 m/acc)))]))

;; Test:
(check-expect (make-lists 4 2)
              (list (list 4 2) (list 3 2) (list 2 2) (list 1 2)
                    (list 4 1) (list 3 1) (list 2 1) (list 1 1)))
                   

;; (all-possible-rectangles my-grid) produces a list of possible rectangles
;;  that are contained within my-grid, with the top-left corner being the first
;;   unused cell in my-grid
;; all-possible-rectangles: Grid -> (listof (list Nat Nat Nat Nat))
;; Example:
(check-expect (all-possible-rectangles (list (list (make-cell 2 true) (make-cell 0 false) (make-cell 0 true) (make-cell 2 true))
                                             (list (make-cell 2 true) (make-cell 0 true) (make-cell 2 false) (make-cell 0 true))))
              (list (list 1 0 3 2) (list 1 0 2 2) (list 1 0 1 2) (list 1 0 3 1) (list 1 0 2 1) (list 1 0 1 1)))

(define (all-possible-rectangles my-grid)
  (map (lambda (x) (append (list (first (get-first-unused my-grid))
                               (second (get-first-unused my-grid))
                               (first x)
                               (second x))))
       (make-lists (first (shrink-dimensions my-grid)) (second (shrink-dimensions my-grid)))))

;; Tests:
(check-expect (all-possible-rectangles (list (list (make-cell 2 true) (make-cell 0 true))
                                             (list (make-cell 0 true) (make-cell 2 true))
                                             (list (make-cell 0 false) (make-cell 1 true))
                                             (list (make-cell 2 true) (make-cell 0 true))))
          (list (list 0 2 2 2) (list 0 2 1 2) (list 0 2 2 1) (list 0 2 1 1)))

(check-expect (all-possible-rectangles (state-grid test-state))
              (list (list 2 1 5 6) (list 2 1 4 6) (list 2 1 3 6) (list 2 1 2 6) (list 2 1 1 6) (list 2 1 5 5) (list 2 1 4 5) (list 2 1 3 5)
                    (list 2 1 2 5) (list 2 1 1 5) (list 2 1 5 4) (list 2 1 4 4) (list 2 1 3 4) (list 2 1 2 4) (list 2 1 1 4) (list 2 1 5 3)
                    (list 2 1 4 3) (list 2 1 3 3) (list 2 1 2 3) (list 2 1 1 3) (list 2 1 5 2) (list 2 1 4 2) (list 2 1 3 2) (list 2 1 2 2)
                    (list 2 1 1 2) (list 2 1 5 1) (list 2 1 4 1) (list 2 1 3 1) (list 2 1 2 1) (list 2 1 1 1)))

(check-expect (all-possible-rectangles (state-grid small-test-state))
              (list (list 1 0 2 3) (list 1 0 1 3) (list 1 0 2 2) (list 1 0 1 2) (list 1 0 2 1) (list 1 0 1 1)))

(check-expect (all-possible-rectangles (state-grid smaller-test-state))
              (list (list 0 0 2 2) (list 0 0 1 2) (list 0 0 2 1) (list 0 0 1 1)))


;; (retrieve-cell x y my-state) produces the cell in my-state with coordinates
;;   x y and their top left corner
;; retrieve-cell: Nat Nat State -> Cell
;; requires: x and y represents a valid cell in my-state
;; Example:
(check-expect (retrieve-cell 4 5 test-state) (make-cell 10 false))

(define (retrieve-cell x y my-state)
  (local [;; (get-cell x cell-list) produces the (x+1)th cell in cell-list
          ;; get-cell : Nat (listof Cells) -> Cell
          ;; requires: there exists an (x+1)th cell in cell-list
          (define (get-cell x cell-list)
            (cond [(= x 0) (first cell-list)]
                  [else (get-cell (sub1 x) (rest cell-list))]))
          ;; (get-cell-data x y my-state grid/acc) produces the cell in grid/acc
          ;; (the grid of my-state) with coordinates x y as their top left corner
          ;; get-cell-data: Nat Nat State Grid Grid -> Cell
          ;; requires: x and y represent a valid cell in my-state
          (define (get-cell-data x y my-state grid/acc)
            (cond [(= 0 y) (get-cell x (first grid/acc))]
                  [else (get-cell-data x (sub1 y) my-state (rest grid/acc))]))]
    (get-cell-data x y my-state (state-grid my-state))))
         
;; Test:
(check-expect (retrieve-cell 1 2 test-state) (make-cell 3 true))


;; (make-more-lists m n) makes a list of lists of two natural numbers;
;; the first element containing 0,1.. 2...m, the second element containing 0...n
;; make-lists: Nat Nat -> (listof (list Nat Nat))
;; Example:
(check-expect (make-more-lists 2 3)
              (list (list 2 3) (list 1 3) (list 0 3) (list 2 2) (list 1 2) (list 0 2)
                    (list 2 1) (list 1 1) (list 0 1) (list 2 0) (list 1 0) (list 0 0)))

(define (make-more-lists m n)
  (make-more-lists/acc m n m))

(define (make-more-lists/acc m n m/acc)
  (cond [(= n -1) empty]
        [(= m/acc -1) (make-more-lists/acc m (sub1 n) m)]
        [else (cons (list m/acc n) (make-more-lists/acc m n (sub1 m/acc)))]))

;; Test:
(check-expect (make-more-lists 4 2)
              (list (list 4 2) (list 3 2) (list 2 2) (list 1 2) (list 0 2) (list 4 1) (list 3 1) (list 2 1) (list 1 1)
                    (list 0 1) (list 4 0) (list 3 0) (list 2 0) (list 1 0) (list 0 0)))


;; (find-corners my-rect) produces a list of all
;;   the top-left corners of cells contained within my-rect
;; find-corners: (list Nat Nat Nat Nat) -> (listof (listof Nat))
;; Example:
(check-expect (find-corners '(2 1 5 6))
                 (list '(2 1) '(3 1) '(4 1) '(5 1) '(6 1)
                       '(2 2) '(3 2) '(4 2) '(5 2) '(6 2)
                       '(2 3) '(3 3) '(4 3) '(5 3) '(6 3)
                       '(2 4) '(3 4) '(4 4) '(5 4) '(6 4)
                       '(2 5) '(3 5) '(4 5) '(5 5) '(6 5)
                       '(2 6) '(3 6) '(4 6) '(5 6) '(6 6)))

(define (find-corners my-rect)
  (local [(define all-vertices (make-more-lists (+ (first my-rect) (third my-rect) -1)
                                                (+ (second my-rect) (fourth my-rect) -1)))
          ;; (filter-vertices x y vertice-list) filters out any vertices in vertice-list with an x
          ;;  value smaller than x and a y value smaller than y
          ;; filter-vertices: Nat Nat (listof (list Nat Nat)) -> (listof (list Nat Nat))
          (define (filter-vertices x y vertice-list)
            (cond [(empty? vertice-list) empty]
                  [(or (< (first (first vertice-list)) x)
                       (< (second (first vertice-list)) y))
                   (filter-vertices x y (rest vertice-list))]
                  [else (cons (first vertice-list) (filter-vertices x y (rest vertice-list)))]))]
    (reverse (filter-vertices (first my-rect) (second my-rect) all-vertices))))

;; Test:
(check-expect (find-corners '(2 1 3 2))
              (list '(2 1) '(3 1) '(4 1)
                    '(2 2) '(3 2) '(4 2)))
(check-expect (find-corners '(1 0 2 3))
              (list '(1 0) '(2 0)
                    '(1 1) '(2 1)
                    '(1 2) '(2 2)))


;; (area my-rect) finds the area of my-rect based on the number of cells in my-rect
;; area: (list Nat Nat Nat Nat) -> Nat
;; Example:
(check-expect (area '(2 1 3 2)) 6)

(define (area my-rect)
  (length (find-corners my-rect)))

;; Tests:
(check-expect (area '(2 1 5 6)) 30)
(check-expect (area '(1 0 2 3)) 6)


;; (make-rectangles my-rect my-state) lists all the cells within my-rect in my-state
;; make-rectangles: (list Nat Nat Nat Nat) -> (listof Cell)
;; Example:
(check-expect (make-rectangles '(2 1 3 2) test-state)
              (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false)
                    (make-cell 6 false) (make-cell 0 false) (make-cell 0 false)))

(define (make-rectangles my-rect my-state)
  (map (lambda (x) (retrieve-cell (first x) (second x) my-state))
       (find-corners my-rect)))

;; Test:
(check-expect (make-rectangles '(2 1 2 4) test-state)
              (list (make-cell 0 false) (make-cell 0 false)
                    (make-cell 6 false) (make-cell 0 false)
                    (make-cell 0 false) (make-cell 0 false)
                    (make-cell 0 false) (make-cell 2 false)))


;; (make-all-rectangles my-state) lists all the possible rectangles
;;   in my-state, beginning from the first unused cell, in cell form
;; make-all-rectangles: State -> (listof (listof Cell))
;; Example:
(check-expect (make-all-rectangles small-test-state)
              (list (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 3 true) (make-cell 0 true))
                    (list (make-cell 0 false) (make-cell 0 false) (make-cell 3 true))
                    (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false))
                    (list (make-cell 0 false) (make-cell 0 false))
                    (list (make-cell 0 false) (make-cell 0 false))
                    (list (make-cell 0 false))))

(define (make-all-rectangles my-state)
  (map (lambda (x) (make-rectangles x my-state))
       (all-possible-rectangles (state-grid my-state))))

;; Test:
(check-expect (make-all-rectangles smaller-test-state)
              (list (list (make-cell 0 false) (make-cell 0 false) (make-cell 1 true) (make-cell 0 false))
                    (list (make-cell 0 false) (make-cell 1 true))
                    (list (make-cell 0 false) (make-cell 0 false))
                    (list (make-cell 0 false))))


;; (not-used? my-rectangle) determines produces true if no cells in my-rectangle
;;  have been used, and false otherwise
;; not-used?: (listof Cell) -> Bool
;; requires: my-rectangle is non-empty
;; Example:
(check-expect (not-used? (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 3 true) (make-cell 0 true))) false)

(define (not-used? my-rectangle)
  (cond [(empty? my-rectangle) true]
        [(equal? true (cell-used? (first my-rectangle))) false]
        [else (not-used? (rest my-rectangle))]))

;; Test:
(check-expect (not-used? (list (make-cell 0 false) (make-cell 0 false))) true)


;; (count-numbers my-rectangle) counts the number of cells in my-rectangle containing non-zero numbers
;; count-numbers: (listof Cell) -> Nat
;; Examples:
(check-expect (count-numbers (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 3 true) (make-cell 0 true))) 1)
(check-expect (count-numbers (list (make-cell 0 false) (make-cell 0 false))) 0)

(define (count-numbers my-rectangle)
  (cond [(empty? my-rectangle) 0]
        [(= (cell-num (first my-rectangle)) 0) (count-numbers (rest my-rectangle))]
        [else (+ 1 (count-numbers (rest my-rectangle)))]))

;; Test:
(check-expect (count-numbers (list (make-cell 0 false) (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 3 true) (make-cell 0 true))) 2)


;; (sum-cells my-rectangle) sums up the numbers within the cells of my-rectangle
;; sum-cells: (listof Cell) -> Nat
;; requires: my-rectangle is non-empty
;; Example:
(check-expect (sum-cells (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false)
                               (make-cell 6 false) (make-cell 0 false) (make-cell 0 false))) 6)

(define (sum-cells my-rectangle)
  (cond [(empty? my-rectangle) 0]
        [else (+ (cell-num (first my-rectangle)) (sum-cells (rest my-rectangle)))]))

;; Test:
(check-expect (sum-cells (list (make-cell 0 false) (make-cell 0 false))) 0)
(check-expect (sum-cells (list (make-cell 2 false) (make-cell 2 false))) 4)


;; (legal-rectangle? my-rectangle my-state) produces true if my-rectangle is a
;;   legal one in my-state and false otherwise
;; legal-rectangle?: (list Nat Nat Nat Nat) State -> Bool
;; Examples:
(check-expect (legal-rectangle? '(2 1 3 2) test-state) true)
(check-expect (legal-rectangle? '(0 0 1 2) smaller-test-state) false)
(check-expect (legal-rectangle? '(1 0 1 1) small-test-state) false)

(define (legal-rectangle? my-rectangle my-state)
  (local [(define cells (make-rectangles my-rectangle my-state))]
  (and (not-used? cells)
       (= (count-numbers cells) 1)
       (= (sum-cells cells) (area my-rectangle)))))

;; Tests:
(check-expect (legal-rectangle? '(2 1 1 6) test-state) true)
(check-expect (legal-rectangle? '(2 1 1 4) test-state) false)
(check-expect (legal-rectangle? '(2 1 4 1) test-state) true)


;; (all-legal-rectangles my-state) produces a list of all the legal rectangles
;;  with a top left corner at the first unused cell in my-state
;; all-legal-rectangles: State -> (listof Rect)
;; Example:
(check-expect (all-legal-rectangles test-state)
              (list '(2 1 1 6) '(2 1 2 3) '(2 1 3 2) '(2 1 4 1)))

(define (all-legal-rectangles my-state)
  (filter (lambda (x) (legal-rectangle? x my-state)) (all-possible-rectangles (state-grid my-state))))

;; Tests:
(check-expect (all-legal-rectangles small-test-state) empty)
(check-expect (all-legal-rectangles smaller-test-state) empty)


;; (change-cells my-rect my-grid) changes all the cells inside my-rect to be used
;; change-cells: (list Nat Nat Nat Nat) Grid -> Grid
;; Example:
(check-expect (change-cells '(2 1 3 2) (state-grid test-state))
              (list (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
                    (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 4 false) (make-cell 0 true))
                    (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 true))
                    (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                    (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                    (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
                    (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))))

(define (change-cells my-rect my-grid)
  (local [;; (change-used x width cell-list changes the (x+1)th cell in cell-list to used
          ;;  as well as the next (width-1) cells as well
          ;; change-used: Nat Nat (listof Cell) -> (listof Cell)
          (define (change-used x width cell-list)
            (cond [(and (= x 0) (= width 1))
                   (cons (make-cell (cell-num (first cell-list)) true) (rest cell-list))]
                  [(= x 0) (cons (make-cell (cell-num (first cell-list)) true) (change-used 0 (sub1 width) (rest cell-list)))]
                  [else (cons (first cell-list) (change-used (sub1 x) width (rest cell-list)))]))]
  (cond [(= 0 (fourth my-rect)) my-grid]
        [(= 0 (second my-rect))
         (cons (change-used (first my-rect) (third my-rect) (first my-grid))
               (change-cells (list (first my-rect) 0 (third my-rect) (sub1 (fourth my-rect))) (rest my-grid)))]
        [else (cons (first my-grid)
                    (change-cells (list (first my-rect) (sub1 (second my-rect)) (third my-rect) (fourth my-rect))
                                                    (rest my-grid)))])))
        
;; Test:
(check-expect (change-cells '(1 3 2 2) (state-grid test-state))
              (list (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
                    (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                    (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                    (list (make-cell 4 true) (make-cell 2 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                    (list (make-cell 0 false) (make-cell 0 true) (make-cell 0 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                    (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
                    (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))))


;; (change-state my-rect my-state) changes all the cells in my-rect to used
;;  as well as adds my-rect to the list of rectangles for my-state
;; change-state: (list Nat Nat Nat Nat) State -> State
;; Example: 
(check-expect (change-state '(2 1 3 2) test-state)
              (make-state
               (list (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
                     (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 4 false) (make-cell 0 true))
                     (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 true))
                     (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                     (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                     (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
                     (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
               (list (make-rect  0 0 1 4)
                     (make-rect  1 0 1 3)
                     (make-rect  2 0 2 1)
                     (make-rect  4 0 2 1)
                     (make-rect  6 0 1 7)
                     (make-rect  2 1 3 2))))

(define (change-state my-rect my-state)
  (make-state (change-cells my-rect (state-grid my-state))
              (append (state-rects my-state) (list (make-rect (first my-rect) (second my-rect) (third my-rect) (fourth my-rect))))))

;; Test:
(check-expect (change-state '(2 1 4 1) test-state)
              (make-state
                    (list (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
                          (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 4 true) (make-cell 0 true))
                          (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                          (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                          (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                          (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
                          (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
                    (list (make-rect  0 0 1 4)
                          (make-rect  1 0 1 3)
                          (make-rect  2 0 2 1)
                          (make-rect  4 0 2 1)
                          (make-rect  6 0 1 7)
                          (make-rect  2 1 4 1))))


;; (neighbours my-state) produces a list of new states that could
;;  possibly follow from after adding a legal rectangle to my-state, or
;;   empty if no such rectangles exist
;; neighbours: State -> (listof States)
;; Example:
(check-expect (neighbours small-test-state) empty)
(check-expect (neighbours smaller-test-state) empty)

(define (neighbours my-state)
  (map (lambda (x) (change-state x my-state)) (all-legal-rectangles my-state)))

;; Test:
(check-expect (neighbours test-state)
              (list
 (make-state
  (list
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 true) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 true) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
   (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
  (list (make-rect 0 0 1 4) (make-rect 1 0 1 3) (make-rect 2 0 2 1) (make-rect 4 0 2 1) (make-rect 6 0 1 7) (make-rect 2 1 1 6)))
 (make-state
  (list
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
   (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
  (list (make-rect 0 0 1 4) (make-rect 1 0 1 3) (make-rect 2 0 2 1) (make-rect 4 0 2 1) (make-rect 6 0 1 7) (make-rect 2 1 2 3)))
 (make-state
  (list
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 4 false) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
   (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
  (list (make-rect 0 0 1 4) (make-rect 1 0 1 3) (make-rect 2 0 2 1) (make-rect 4 0 2 1) (make-rect 6 0 1 7) (make-rect 2 1 3 2)))
 (make-state
  (list
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 4 true) (make-cell 0 true))
   (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
   (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
   (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
  (list (make-rect 0 0 1 4) (make-rect 1 0 1 3) (make-rect 2 0 2 1) (make-rect 4 0 2 1) (make-rect 6 0 1 7) (make-rect 2 1 4 1)))))



;; Part f)

;; (solve-rectangle-puzzle lolon) produces a list of rectangles that describe a solution if one exists
;;  or false if there is no solution
;; solve-rectangle-puzzle: (listof (listof Nat)) -> (anyof (listof Rect) false)
;; Examples:
(check-expect (solve-rectangle-puzzle (list (list 0 0) (list 1 0))) false)
(check-expect (solve-rectangle-puzzle (list (list 2 0 0) (list 0 0 0) (list 0 3 0))) false)

(define (solve-rectangle-puzzle lolon)
  (local [(define my-search (search solved? neighbours (construct-puzzle lolon)))]
    (cond [(false? my-search) false]
          [else (state-rects my-search)])))

;; Tests:
(check-expect (solve-rectangle-puzzle (list '(0 0 0 2 0 2 0)
                                            '(0 0 0 0 0 4 0)
                                            '(0 3 6 0 0 0 0)
                                            '(4 2 0 0 0 4 0)
                                            '(0 0 0 2 0 0 0)
                                            '(0 0 0 0 10 0 7)
                                            '(3 0 0 0 0 0 0)))
              (list
               (make-rect 0 0 1 4)
               (make-rect 1 0 1 3)
               (make-rect 2 0 2 1)
               (make-rect 4 0 2 1)
               (make-rect 6 0 1 7)
               (make-rect 2 1 2 3)
               (make-rect 4 1 2 2)
               (make-rect 1 3 1 2)
               (make-rect 4 3 2 2)
               (make-rect 0 4 1 3)
               (make-rect 2 4 2 1)
               (make-rect 1 5 5 2)))
              
(check-expect (solve-rectangle-puzzle (list '(0 2)
                                            '(2 0)))
              (list (make-rect 0 0 1 2) (make-rect 1 0 1 2)))
  
  
