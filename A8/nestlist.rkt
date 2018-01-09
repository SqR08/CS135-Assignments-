;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nestlist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 08, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;3a)
;;(nfoldr combine1 combine2 base list) consumes a
;;list and produces a new list by combining the first
;;of the list and the recursive rest of the list
;;nfoldr: (X Y -> Y) (Y Y -> Y) Y Nested-Listof-X -> Y
;;Examples:
(check-expect (nfoldr (lambda (x y) (add1 y)) + 0 '(1 (2 3) () ((4)))) 4)
(check-expect (nfoldr cons append empty '(1 (2 3) () ((4)))) '(1 2 3 4))

(define (nfoldr combine1 combine2 base list)
  (cond [(empty? list) base]
        [(list? (first list))
         (combine2 (nfoldr combine1 combine2 base (first list))
                   (nfoldr combine1 combine2 base (rest list)))]
        [else
         (combine1 (first list) (nfoldr combine1 combine2 base (rest list)))]
        ))

;;Tests:
(check-expect (nfoldr (lambda (x y) (+ x y)) + 0 '((1 (2 3) () ((4))))) 10)


;;3b)
;;(nfilter pred? mynlist) consumes a predicate and a nested list
;;and produces a new list that only contains the items that
;;are true based on the predicate
;;nfilter: (X -> Bool) Nested-Listof-X -> Nested-Listof-X
;;Examples:
(check-expect (nfilter odd? '(1 (2 3) () ((4))))
              (list 1 (list 3) empty (list empty)))
(check-expect (nfilter even? '(1 (2 3) () ((4))))
              (list (list 2) empty (list (list 4))))

(define (nfilter pred? list)
  (nfoldr           
          (lambda (x y) (cond [(pred? x)(cons x y)]
                              [else y]))
          (lambda (x y) (cons x y))
                empty 
                list))

;;Tests:
(check-expect (nfilter (lambda (x) (> x 2)) '(1 (2 3) () ((4))))
                                   (list (list 3) empty (list (list 4))))
                                    

;;3c)
;;(nmap fn list) consumes list and produces a new list
;;with the function fn applied to every element of list
;;nmap: (X -> Y) Nested-Listof-X -> Nested-Listof-Y
;;Examples:
(check-expect (nmap sqr '(1 (2 3) () ((4))))
              (list 1 (list 4 9) empty (list (list 16))))
(check-expect (nmap (lambda (x) (* 2 x)) '(1 (2 3) () ((4))))
              (list 2 (list 4 6) empty (list (list 8))))

(define (nmap fn list)
  (nfoldr       
          (lambda (x y) (cons (fn x) y))
          (lambda (x y) (cons x y))
          empty
          list))
          

;;3d)
;;(nreverse list) consumes a nested list and
;;produces a new list with the original list reversed
;;nreverse: Nested-Listof-X -> Nested-Listof-X
;;Examples:
(check-expect (nreverse (list 1 2 3)) (list 3 2 1))
(check-expect (nreverse '(1 (2 3) () ((4)))) '(((4)) () (3 2) 1))


(define (nreverse nlist)
  (nfoldr
          (lambda (x y) (append y
                                (list x)))
          (lambda (x y) (append y
                                (list x)))
          empty
          nlist))


;;3e)
;;(nheight list) determines the height
;;of list
;;nheight: Nested-Listof-Any -> Nat
;;Examples:
(check-expect (nheight '()) 1)
(check-expect (nheight '((1 (2 3)) 4 (5 (6 7 8) 9))) 3)

(define (nheight list)
 (nfoldr
       (lambda (x y) y)
       (lambda (x y) (max (add1 x) y))                              
       1
       list))

;;Tests:
(check-expect (nheight '((1 a) (2 b) (3 c))) 2)

;;3f)
;;(prune nlist) removes all empty lists
;;and nested lists that only contain empty lists
;;prune: Nested-Listof-X -> Nested-Listof-X
;;Examples:
(check-expect (prune '(1 (2 3 ()) ( (()) (4) ()(())))) '(1 (2 3) ((4))))
(check-expect (prune '(()((())()))) '())

(define (prune nlist)
  (nfoldr
         (lambda (frst rst) (cons frst rst))
         (lambda (frst rst) (cond [(empty? frst) rst]
                                  [else (cons frst rst)]))
                                  
         empty
         nlist))
         
         
 














