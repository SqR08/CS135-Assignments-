;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |1d) kenneth version blessed|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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