;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require "a07lib.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 07, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;2a)

(define a-tnode (make-tnode #\a false (list (make-tnode #\t true empty))))

(define c-tnode
  (make-tnode #\c false
              (list
               (make-tnode #\o false (list (make-tnode #\o true empty)
                                           (make-tnode #\w true empty)))
               (make-tnode #\s false
                                     (list (make-tnode #\1 false
                                             (list
                                               (make-tnode #\1 false
                                                     (list (make-tnode #\5 true empty)
                                                           (make-tnode #\6 true empty)))           
                                               (make-tnode #\3 false
                                                     (list (make-tnode #\5 true empty)
                                                           (make-tnode #\6 true empty)))))))
               )))


(define a-c-trie (make-trie (list a-tnode c-tnode)))                                

;;2b)

;;trie-template: Trie -> Any
;;(define (trie-template mytrie)
;; (... list-tnode-template (trie-children mytrie)...)


;;list-tnode-template: (listof TNodes) -> Any
;;(define (list-tnode-template mylist)
;;  (cond [(empty? mylist)...]
;;        [else (tnode-template (first mylist))...
;;              (list-tnode-template (rest mylist))...]
;;        ))

;;tnode-template: TNode -> Any
;;(define (tnode-template mytnode)
;;  (... (tnode-key mytnode)...
;;       (tnode-ends-word? mytnode)...
;;       (list-tnode-template (tnode-children mytnode))...))

;;2c)
;;(in-trie? myword mytrie) determines whether myword is in
;;mytrie
;;in-trie?: Str Trie -> Bool
;;Examples:
(check-expect (in-trie? "cs135" a-c-trie) true)
(check-expect (in-trie? "" a-c-trie) false)

(define (in-trie? myword mytrie)
  (in-trie?/main (string->list myword) mytrie))

;;(in-trie?main myword mytrie) checks whether myword is in
;;mytrie
;;in-trie?: (listof Char) -> Bool
(define (in-trie?/main myword mytrie)
  (cond
    [(empty? (trie-children mytrie)) false]
    [(empty? myword) false]
    [(and (empty? (rest myword)) (char=?
                                  (first myword)
                                  (tnode-key (first (trie-children mytrie))))
             (tnode-ends-word? (first (trie-children mytrie)))) true]
     [(char=? (first myword) (tnode-key (first (trie-children mytrie))))
       (in-trie?/main (rest myword)
                      (make-trie
                       (tnode-children (first (trie-children mytrie)))))]
     [else (in-trie?/main myword (make-trie (rest (trie-children mytrie))))]

    ))

;;Tests:
(check-expect (in-trie? "at" a-c-trie) true)
(check-expect (in-trie? "at" (make-trie empty)) false)
(check-expect (in-trie? "a" a-c-trie) false)
(check-expect (in-trie? "cower" a-c-trie) false)
(check-expect (in-trie? "hot" h-u-trie) true)

;;2d)
;;(list-words mytrie) produces a list of all the words in
;;mytrie
;:list-words: Trie -> (listof Str)
;;Examples:
(check-expect (list-words blank-trie) empty)
(check-expect (list-words h-u-trie) (list "ha" "hat" "he" "hot" "use"))

(define (list-words mytrie)
 (list-words2 (trie-children mytrie) empty))

;;(list-words2 listofnodes worldlistacc) produces a list of words
;;(worldlistacc) that is in a list of nodes
;:list-words2: (listof TNodes) (listof Str) -> (listof Str)
(define (list-words2 listofnodes wordlistacc)
  (cond
    [(empty? listofnodes) empty]
    [else (append (char/acc (first listofnodes) wordlistacc)
                  (list-words2 (rest listofnodes) wordlistacc))]
    ))

;;(char/acc mynode characc) produces a list of words 
;;in mynode
;:characc: (listof TNodes) (listof Str) -> (listof Str)
(define (char/acc mynode characc)
  (cond
        [(tnode-ends-word? mynode)
          (append (list (list->string
                         (append characc (list (tnode-key mynode))))) 
                  (list-words2 (tnode-children mynode)
                               (append characc (list (tnode-key mynode)))))]
        
        [else
         (list-words2 (tnode-children mynode)
                      (append characc (list (tnode-key mynode))))]
         
        ))

;;Tests:
(check-expect (list-words c-d-trie) (list
 "cat" "catch" "cater" "catnip" "cattle" "dig" "dog" "dogfish" "donald" "donut"
 "doze"))

;;2e)
;;(insert-word myword mytrie) produces a trie consisting of
;;mytrie with myword inserted
;;insert-word: Str Trie -> Trie
(check-expect (list-words(insert-word "hated" h-u-trie))
              (list "ha" "hat" "hated" "he" "hot" "use"))
(check-expect (list-words (insert-word "ho" h-u-trie))
               (list "ha" "hat" "he" "ho" "hot" "use"))


(define(insert-word myword mytrie)
  (make-trie (trie-producer (string->list myword)(trie-children mytrie)empty)))


;;(build-trie myword listofnodes accum) produces list of nodes creating
;;the children of a tree
;;build-trie: (listof Str) (listof TNode) (listof TNode) -> (listof TNode)
(define(build-trie myword listofnodes accum)
  (cond
    [(empty?(rest myword))
     (append accum (list (make-tnode(tnode-key(first listofnodes))
                                 true
                                 (tnode-children (first listofnodes))))
             (rest listofnodes))]
    [else (append accum (list(make-tnode(tnode-key(first listofnodes))
                                      (tnode-ends-word?(first listofnodes))
                                      (trie-producer (rest myword)
                                                 (tnode-children
                                                  (first listofnodes))
                                                 empty)))
                  (rest listofnodes))]))

;;(node-builder myword) consumes myword and produces a tnode
;;node-builder Str -> Tnode
(define(node-builder myword)
  (make-tnode(first myword)
             (cond
               [(empty? (rest myword))true]
               [else false])
             (cond
               [(empty? (rest myword))empty]
               [else(list(node-builder (rest myword)))])))

;;(trie-producer myword listofnodes accum) places the myword into the existing
;;trie
;;trie-producer: (listof Str) (listof TNode) (listof TNode) -> (listof TNode)
(define(trie-producer myword listofnodes accum)
  (cond
    [(empty? listofnodes)(append accum (list(node-builder myword)))]
    [(char<?(tnode-key(first listofnodes))(first myword))
     (trie-producer myword (rest listofnodes)
                 (append accum (list(first listofnodes))))]
    [(char>?(tnode-key(first listofnodes))(first myword))
     (append accum (list(node-builder myword)) listofnodes)]
    [(char=? (tnode-key(first listofnodes))(first myword))
     (build-trie myword listofnodes accum)]))

;Tests:
(check-expect (list-words (insert-word "him" h-u-trie))
              (list "ha" "hat" "he" "him" "hot" "use"))

;;2f)
;;(insert-some-words listofwords mytrie) produces a trie consisting of
;;mytrie with all of the words in listofwords inserted
;;insert-some-words (listof Str) Trie -> Trie
;;Examples:
(check-expect (list-words (insert-some-words (list "hog" "hoot") h-u-trie))
              (list "ha" "hat" "he" "hog" "hoot" "hot" "use"))
(check-expect (list-words (insert-some-words (list "hob" "mans") h-u-trie))
              (list "ha" "hat" "he" "hob" "hot" "mans" "use"))

(define(insert-some-words mystr/list mytrie)
  (cond
    [(empty? mystr/list)mytrie]
    [else (insert-some-words (rest mystr/list)
                             (insert-word (first mystr/list)mytrie))]))

;;Tests:
(check-expect (list-words (insert-some-words empty h-u-trie))
              (list "ha" "hat" "he" "hot" "use"))


;;2g)
;;(list-completions mystring mytrie) produces a list of strings
;;in mytrie beginning with the prefix mystring
;;list-completions: Str Trie -> (listof Str)
;;Examples:
(check-expect (list-completions "don" c-d-trie)  (list "donald" "donut")) 
(check-expect (list-completions "danielwang" c-d-trie)  empty)

(define (list-completions mystring mytrie)
  (list-completions2 (string->list mystring) (list-words mytrie)))

;;(list-completions2 takes a prefix (mylistofchar) and
;;a list of words (mylistofwords) and produces a list of words
;;list-completions2: (listof Char) (listof Str) -> (listof Str)
(define (list-completions2 mylistofchar mylistofwords)
  (cond
    [(empty? mylistofchar) mylistofwords]
    [(empty? mylistofwords) empty]
    [(prefix? mylistofchar (string->list (first mylistofwords)))
      (append (list (first mylistofwords))
              (list-completions2 mylistofchar (rest mylistofwords)))]
    [else (list-completions2 mylistofchar (rest mylistofwords))]
    ))

;;(prefix? mylistofchar listofcharsinword) checks if a
;;list of char (mylistofchar) is a prefix of a word
;;prefix?: (listof Char)(listof Char) -> Bool
(define (prefix? mylistofchar listofcharsinword)
  (cond
    [(empty? mylistofchar) true]
    [(empty? listofcharsinword) false]
    [(char=? (first mylistofchar) (first listofcharsinword))
     (prefix? (rest mylistofchar)(rest listofcharsinword))]
    [else false]
    ))
     
;;Tests:
(check-expect (list-completions "donalde" c-d-trie) empty)
(check-expect (list-completions "" h-u-trie)(list "ha" "hat" "he" "hot" "use"))


;;2g)
;;(list-completions5 mystring mytrie) produces a sorted list of strings consisting
;;of all the words in the Trie that begin with that prefix
(define (list-completions5 mystring mytrie)
  (list-completions6 (string->list mystring) mytrie))

(define (list-completions6 myloc mytrie)
  
  (local [;;startswithpred? sees if myword starts with mystring
          ;;takes a string and sees it if starts with a pred
          (define (startswithpred? myloc myword)
            
            (cond [(empty? myword) false]
                  [(empty? myloc) true]
                  [(equal? (first myloc) (first myword))
                   (startswithpred? (rest myloc) (rest myword))]
                  [else false]) )]
        
  (map list->string (filter (lambda (x) (startswithpred? myloc x)) (map string->list (list-words mytrie))))))

;;Tests:
(list-completions5 "don" c-d-trie)
        
  

         

                                