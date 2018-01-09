;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |moretabular 2.0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

(define (mirror mytable)
  (map (lambda (x) (foldl cons empty x)) mytable))


;;element-apply-many consumes a list of functions and a table
;;and produces a list of tables as a result of applying the list of functions
;;on to the table
;;(listof functions) Table -> (listof Table)


(define (element-apply-many mytable lof)
  (cond
    
        [(empty? mytable) empty]
        [(empty? lof) mytable]
        [else (local [(define (tablemaker mytable fn)
                       (cond [(empty? mytable) empty]
                             
                             [else (cons (map fn (first mytable))
                                         (tablemaker (rest mytable) fn))]
                             ))]
                             
              (cons (tablemaker mytable (first lof))
                    (element-apply-many mytable (rest lof))))]))


;; (apply-function f arg) produces the result of f with the given argument arg.
;; apply-function: (X → Y) X → Y
(define (apply-function f arg)
(f arg))

(define (scale-smallest mytable offset)
  (local [
          (define (minoflist lst acc)
            (cond [(empty? lst) acc]
                  [(< (first lst) acc) (minoflist (rest lst) (first lst))]
                  [else (minoflist (rest lst) acc)]))
             
          (define (mintable/acc mytable minsofar)
                      (cond [(empty? mytable) minsofar]
                            [(< minsofar (minoflist (first mytable) (first (first mytable))))
                             (mintable/acc (rest mytable) minsofar)]
                            [(< (minoflist (first mytable) (first (first mytable))) minsofar)
                             (mintable/acc (rest mytable) (minoflist (first mytable) (first (first mytable))))]))                       

          (define (fn number)
            ( + (* number (mintable/acc mytable (first (first mytable)))) offset))]

    fn))

(check-expect (apply-function (scale-smallest '((5 3 4)(1 3 13)) 3.14) 3)
           6.14)





