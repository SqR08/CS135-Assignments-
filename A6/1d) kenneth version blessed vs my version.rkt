;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |1d) kenneth version blessed vs my version|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (adder list1 list2)
  (cond
    [(empty? list1) empty]
    [else (cons (+ (first list1)(first list2))
                (adder (rest list1)(rest list2)))]
    ))

(define (sum-tables table1 table2)
 (cond
   [(empty? table1) empty]
   [else (cons (adder (first table1)(first table2))
               (sum-tables (rest table1)(rest table2)))]
   ))


;;Making rows
(define (rowbuilder row column table1 table2)
  (cond
    [(empty? table1) empty]
    [else (cons
           (columnbuilder row column (first table1) (first table2))
           (rowbuilder row column (rest table1) (rest table2)))]

    ))

;;Making the columns at a row
(define (columnbuilder row column table1row table2row)
  (cond
    [(empty? (first table1row)) empty]
    [else (cons (+ (get-elem row column table1row)
                   (get-elem row column table2row))
           (columnbuilder row column (rest table1row) (rest table2row)))]
   ))

;;(sum-tables table1 table2) produces a table with all
;;the elements being a sum of adding the elements of
;;table1 and table2 at each exact position
;;Table Table -> Table
(define (sum-tables table1 table2)
    (rowbuilder 0 0 table1 table2))
