;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |trie 2.0|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a07lib.rkt")

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
;;(in-trie? myword mytrie)

(define (in-trie? myword mytrie)
  (in-trie?/main (string->list myword) mytrie))

;;(listof Char) Trie -> Bool
(define (in-trie?/main myword mytrie)
  (cond [(empty? (trie-children mytrie)) false]
        [(empty? myword) false]
        [(and (empty? (rest myword)) (char=? (first myword) (tnode-key (first (trie-children mytrie))))) true]
        [(char=? (first myword) (tnode-key (first (trie-children mytrie))))
         (in-trie?/main (rest myword) (make-trie (tnode-children (first (trie-children mytrie)))))]
        [else (in-trie?/main myword (make-trie (rest (trie-children mytrie))))]))

(check-expect (in-trie? "at" a-c-trie) true)
(check-expect (in-trie? "at" (make-trie empty)) false)
(check-expect (in-trie? "cower" a-c-trie) false)
(check-expect (in-trie? "hot" h-u-trie) true)

;;2d)
;;consumes a trie and produces a list of all the words in the trie

;;(define (list-words mytrie)
;;  (list-words2 (trie-children mytrie) empty))

;;(list-words2 listofnodes worldlistacc) produces a list of words
;;(worldlistacc) that is in a list of nodes
;:list-words2: (listof TNodes) (listof Str) -> (listof Str)
;;(define (list-words2 listofnodes wordlistacc)
;;  (cond [(empty? listofnodes) wordlistacc]
;;        [else (worldlistacc (rest listofnodes)
;;                            (append (list-words3 (first listofnodes))
;;                                     worldlistacc))]))

;;(char/acc mynode characc) produces a list of words 
;;in mynode
;:characc: Tnode (listof Str) -> (listof Str)
;;(define (list-words3 node worldlistacc)        
;;  (cond [(empty? (tnode-children node)) wordlistacc]
;;        [(false? (tnode-ends-word? node)) (append (char/acc node 

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

;;2g)
;;(list-completions2 mystring mytrie) produces a sorted list of strings consisting
;;of all the words in the Trie that begin with that prefix
(define (list-completions2 mystring mytrie)
  (local [;;startswithpred? sees if myword starts with mystring
          ;;takes a string and sees it if starts with a pred
          (define (startswithpred? mystring myword)
            (local [(define THEPRED (string->list mystring))
                    (define THEWORD (string->list myword))]
              
            (cond [(empty? THEWORD) false]
                  [(empty? THEPRED) true]
                  [(equal? (first THEPRED) (first THEWORD))
                   (startswithpred? (rest THEPRED) (rest THEWORD))]
                  [else false])   ))]

                   
  (filter (lambda (x) (startswithpred? mystring x)) (map string->list (list-words mytrie))))

;;Tests:
(list-completions2 "don" c-d-trie)



  
  