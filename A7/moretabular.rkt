;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname moretabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 07, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

;;3a)
;;(mirror mytable) reverses the elements of each row of mytable
;;mirror: Table -> Table
;;Examples:
(check-expect (mirror empty) empty)
(check-expect (mirror (list (list 8 3 4 9)
                            (list 3 7 5 6)))
                      (list (list 9 4 3 8)
                            (list 6 5 7 3)))

(define (mirror mytable)
  (cond
    [(empty? mytable) empty] 
    [else
      (local 
        [(define (rowreverse myrow)
           (cond [(empty? myrow) empty]
                 [else
                  (append (rowreverse (rest myrow)) (list (first myrow)))]
                 ))]
     (cons (rowreverse (first mytable)) (mirror (rest mytable))))]

    ))
;;Tests:
(check-expect (mirror (list (list 9 9 9 9)
                            (list 9 9 9 9)))
                      (list (list 9 9 9 9)
                            (list 9 9 9 9)))


;;3b)
;;(element-apply-many f1 f2 f3 mytable) produces a list of tables
;;made from applying the functions f1, f2, f3 on to mytable
;;element-apply-many: (Num -> Num) (Num -> Int)
;;                    (Num -> Nat) Table -> (listof Table)
;;Examples:
(check-expect (element-apply-many (list abs floor sub1) empty) empty)
(check-expect (element-apply-many (list abs floor add1)
                                  (list (list 8 3 4 9)
                                        (list 3 7 5 6)))
                            (list (list (list 8 3 4 9)
                                        (list 3 7 5 6))
                                  (list (list 8 3 4 9)
                                        (list 3 7 5 6))
                                  (list (list 9 4 5 10)
                                        (list 4 8 6 7))))

(define (element-apply-many listoffunctions mytable) 
  (cond
    [(empty? mytable) empty]
    [(empty? listoffunctions) mytable]
    [else
      (local [;;(functionapplier function myrow) applies a function
              ;;onto every element of myrow
              ;;functionapplier: (anyof (Num -> Num) (Num -> Int) (Num -> Nat))
              ;;                 (listof Num) -> (listof Num)
             (define (functionapplier function myrow)
             (cond [(empty? myrow) empty]
                   [else 
                     (cons (function (first myrow))
                           (functionapplier function (rest myrow)))]))]

       (local [;;(rowcombiner function mytable) applies a function
              ;;onto every element of mytable
              ;;functionapplier: (anyof (Num -> Num) (Num -> Int) (Num -> Nat))
              ;;                 Table -> Table
              (define (rowcombiner function mytable)
              (cond [(empty? mytable) empty]
                    [else
                     (cons (functionapplier function (first mytable))
                           (rowcombiner function (rest mytable)))]))]

         (local [;;(functionselector listoffunctions mytable) takes each
                 ;;function in listoffunctions and applies it to mytable,
                 ;; creating a list of tables
                 ;;functionselector: (anyof (Num -> Num) (Num -> Int)
                 ;;                  (Num -> Nat)) Table
                 ;;                   -> (listof Table)
                 (define (functionselector listoffunctions mytable)
                 (cond [(empty? listoffunctions) empty]
                       [else
                         (cons (rowcombiner (first listoffunctions) mytable)
                               (functionselector (rest listoffunctions)
                                                 mytable))
                               ]))]
                 
          (functionselector listoffunctions mytable))))
      ]))
        
;;Tests:
(check-expect (element-apply-many (list abs ceiling sub1)
                                  (list (list 8 3 4 9)
                                        (list 3 7 5 6)))
                            (list (list (list 8 3 4 9)
                                        (list 3 7 5 6))
                                  (list (list 8 3 4 9)
                                        (list 3 7 5 6))
                                  (list (list 7 2 3 8)
                                        (list 2 6 4 5))))
(check-expect (element-apply-many empty
                                  (list (list 8 3 4 9)
                                        (list 3 7 5 6)))
                                  (list (list 8 3 4 9)
                                        (list 3 7 5 6)))

;;3c)

;; (apply-function f arg) produces the result of f with the given argument arg.
;; apply-function: (X → Y) X → Y
(define (apply-function f arg)
(f arg))

;;(scale-smallest (mytable offset) produces a second function
;;that consumes a number, multiplies that number by the smallest element
;;mytable, and adds the offset
;;(scale-smallest: Table Num -> (Num -> Num)
;;Examples:
(check-expect (apply-function (scale-smallest '((7 10 20)(0 3 13)) 3) 7)
              3)
(check-expect (apply-function (scale-smallest '((7 4.5 3.2)(-1 3 13)) 2.4) 9)
              -6.6)

(define (scale-smallest mytable offset)
 
    (local
      [;;(smallestinrow row smallestfar) produces the smallest element
       ;;in row
       ;;smallestinrow: (listof Num) -> Num
       (define (smallestinrow row smallestsofar)
         (cond [(empty? row) smallestsofar]
               [(< (first row) smallestsofar) (smallestinrow (rest row)
                                                             (first row))]
               [else
                (smallestinrow (rest row) smallestsofar)]))]
    (local
       [;;(smallestintable/acc mytable smallestoftablesofar) accumulates the
        ;;smallest of element of mytable
        ;;smallestelementintable/acc: Table -> Num
        (define (smallestintable/acc mytable smallestoftablesofar)
          (cond [(empty? mytable) smallestoftablesofar]
                [else
                 (min
                  (smallestinrow (first mytable) smallestoftablesofar)
                  (smallestintable/acc (rest mytable) smallestoftablesofar))]))
        ]            
    (local
      [;;(smallestoftable mytable) produces the smallest element of mytable
       ;;smallestoftable: Table -> Num
       (define (smallestoftable mytable)
         (smallestintable/acc mytable (first (first mytable))))]
    
      (local
        [;;(f number) consumes a number, multiplies that number by the smallest
         ;; element mytable, and adds the offset(define (f number)
         ;;f: Num -> Num
         (define (f number) 
           (+ (* (smallestoftable mytable) number) offset))]
    
     f
     ))))
  )

;;Tests:
(check-expect (apply-function (scale-smallest '((5 3 4)(1 3 13)) 3.14) 3)
              6.14)
   


