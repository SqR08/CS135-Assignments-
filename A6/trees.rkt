;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 06, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct node (key val left right))
;;A node is a (make-node Num Str BT BT)

;;A binary tree (BT) is one of:
;;*empty
;;*Node 

(define exampleBT
(make-node 1 "a"
(make-node 7 "b" empty empty)
(make-node 3 "c" (make-node 7 "d" empty empty) empty)))

;;3a)
;;(height mytree) produces the height of a given
;;binary tree (mytree)
;;height: BT -> Nat
;;Examples:
(check-expect (height empty) 0)
(check-expect (height exampleBT) 3)

(define (height mytree)
  (cond
    [(empty? mytree) 0]
    [else (max (+ 1 (height (node-left mytree)))
               (+ 1 (height (node-right mytree))))]
    ))

;;Tests:
(check-expect (height (make-node 1 "a" empty empty)) 1)
(check-expect (height (make-node 1 "a" (make-node 2 "a" empty empty) empty)) 2)

;;3b)
;;(find-in-tree mytree listofsym) finds the key at the node
;;which is found after following the left and right movements
;;denoted in the list of symbols (listofsym)
;;find-in-tree: BT (listof Sym) -> Num
;;Examples:
(check-expect (find-in-tree (make-node 1 "a"
                            (make-node 2 "a" empty empty) empty) empty)
                            1)
(check-expect (find-in-tree (make-node 1 "a"
                            (make-node 2 "a" empty empty) empty)'(L))
                            2)

(define (find-in-tree mytree listofsym)
  (cond
    [(empty? mytree) false]
    [(empty? listofsym) (node-key mytree)]
    [(equal? (first listofsym) 'L)
     (find-in-tree (node-left mytree) (rest listofsym))]
    [(equal? (first listofsym) 'R)
     (find-in-tree (node-right mytree)(rest listofsym))]
    ))

;;Tests:
(check-expect (find-in-tree exampleBT '(L L)) false)
(check-expect (find-in-tree exampleBT '(R L)) 7)
(check-expect (find-in-tree empty '(L L)) false)

;;3c)
;;(prune mytree myum) finds all the roots of the tree (mytree)
;;with its key being equal to mynum, and removes all its subtrees
;;from the tree
;;prune: BT Num -> BT
;;Examples:
(check-expect (prune empty 0) empty)
(check-expect (prune (make-node 1 "a"
                            (make-node 2 "a" empty empty) empty) 2)
              (make-node 1 "a" empty empty))

(define (prune mytree mynum)
  (cond
    [(empty? mytree) empty]
    [(= (node-key mytree) mynum)
         empty]
    [else (make-node
          (node-key mytree)
          (node-val mytree)
          (prune (node-left mytree) mynum)
          (prune (node-right mytree) mynum))]
    ))

;;Tests:
(check-expect (prune exampleBT 7)
              (make-node 1 "a" empty (make-node 3 "c" empty empty)))
(check-expect (prune (make-node 1 "a"
                            (make-node 2 "a" empty empty) empty) 1)
              empty)
(check-expect (prune (make-node 1 "a"
                            (make-node 2 "a" empty empty) empty) 3)
              (make-node 1 "a"
                            (make-node 2 "a" empty empty) empty))
                            