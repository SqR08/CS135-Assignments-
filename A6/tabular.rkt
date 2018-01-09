;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 06, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

;;1a)
;;(mult-by mynum mytable) produces a table with
;;each number in mytable being multiplied by
;;mynum
;;mult-by: Num Table -> Table
;;Examples
(check-expect (mult-by 2 empty) empty)
(check-expect (mult-by 2 (list (list 1 2 3)(list 3 4 5)))
              (list (list 2 4 6)(list 6 8 10)))

(define (mult-by mynum mytable) 
 (cond
   [(empty? mytable) empty]
   [else (cons (list
          (* mynum (first (first mytable)))
          (* mynum (second (first mytable)))
          (* mynum (second (rest (first mytable)))))
          (mult-by mynum (rest mytable)))]
   ))

;;Tests
(check-expect (mult-by 0 (list (list 1 2 3)(list 3 4 5)))
              (list (list 0 0 0)(list 0 0 0)))

;;1b)

;;(atrow? rowcount row column mytable) keeps recursing until
;;we are at the row with the row number which is consumed (row)
;;atrow? Nat Nat Nat Table -> (anyof Bool Num)
(define (atrow? rowcount row column mytable)
 (cond
   [(empty? mytable) false]
   [(= rowcount row) (atcolumn? 0 column (first mytable))]
   [else (atrow? (+ 1 rowcount) row column (rest mytable))]
   ))

;;(atcolumn? columncount column myrow) keeps recursing until
;;we are at the column with the column number which is consumed (column)
;;and then produces that element
;;atcolumn: Nat Nat (listof Num) -> (anyof Bool Num)
(define (atcolumn? columncount column myrow)
  (cond
    [(empty? myrow) false]
    [(= columncount column)(first myrow)]
    [else (atcolumn? (+ 1 columncount) column (rest myrow))]
    ))

;;(get-elem row column mytable) produces the element
;;of a table that is exactly in the given row and column
;;get-elem: Nat Nat Table -> Num
;;Examples:
(check-expect (get-elem 3 3 (list (list 1 2 3)(list 3 4 5))) false)
(check-expect (get-elem 1 1 (list (list 1 2 3)(list 3 4 5))) 4)


(define (get-elem row column mytable)
  (atrow? 0 row column mytable))

;;Tests:
(check-expect (get-elem 0 0 (list (list 1 2 3)(list 3 4 5))) 1)
(check-expect (get-elem 1 5 (list (list 1 2 3)(list 3 4 5))) false)
(check-expect (get-elem 7 2 (list (list 1 2 3)(list 3 4 5))) false)

;;1c)

;;(goinguprows rowcount column mytable) produces a list
;;of all the elements at a column in a row, starting at the
;;first row and going up 1 row everytime
;;goinguprows: Nat Nat Table -> (listof Num)
(define (goinguprows rowcount column mytable)
  (cond
    [(empty? mytable) empty]
    [(not (columnexist? 0 column (first mytable))) empty]
    [else (cons (get-elem rowcount column mytable)
                 (goinguprows rowcount column (rest mytable)))]
    ))

;;(columnexist? columncount column myrow) checks
;;a row to see if the inputted column number exists in the
;;row
;;columnexist? Nat Nat (listof Num) -> Bool
(define (columnexist? columncount column myrow)
  (cond
    [(empty? myrow) false]
    [(= columncount column) true]
    [else (columnexist? (+ 1 columncount) column (rest myrow))]
    ))

;;(col column mytable) produces a list of all the numbers
;;in a given column of a given table (mytable)
;;col: Nat Table -> (listof Num)
;;Examples:
(check-expect (col 2 (list (list 1 2 3)(list 3 4 5)))
              (list 3 5))
(check-expect (col 4 (list (list 1 2 3)(list 3 4 5)))
              empty)

(define (col column mytable)
 (goinguprows 0 column mytable))

;;Tests:
(check-expect (col 0 (list (list 1 2 3)(list 3 4 5)))
              (list 1 3))



;;1d)
;;(rowadder row1 row2) creates a row by adding the components
;;of the rows from table1 (row1) and table2 (row2)
;;rowadder: (listof Num) (listof Num) -> (listof Num)
(define (rowadder row1 row2)
  (cond
    [(empty? row1) empty]
    [else (cons (+ (first row1)(first row2))
                (rowadder (rest row1)(rest row2)))]
    ))

;;(sum-tables table1 table2) produces a table with all
;;the elements being a sum of adding the elements of
;;table1 and table2 at each exact position
;;Table Table -> Table
;;Examples
(check-expect (sum-tables (list (list 1 2 3)) (list (list 3 4 5)))
                          (list (list 4 6 8)))
(check-expect (sum-tables (list (list 1 2 3)) (list (list 1 2 3)))
                          (list (list 2 4 6)))

(define (sum-tables table1 table2)
 (cond
   [(empty? table1) empty]
   [else (cons (rowadder (first table1)(first table2))
               (sum-tables (rest table1)(rest table2)))]
   ))

;;Tests:
(check-expect (sum-tables (list (list 1 2 3)(list 3 4 5))
                          (list (list 1 2 3)(list 3 4 5)))
                          (list (list 2 4 6) (list 6 8 10)))
