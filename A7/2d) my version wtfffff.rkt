;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2d) my version wtfffff|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;2d)
;;(list-words mytrie) produces a list of all the words in
;;mytrie
;:(list-words: Trie -> (listof Str)
;;Examples:
(check-expect (list-words blank-trie) empty)
;;(check-expect (list-words h-u-trie) (list "ha" "hat" "he" "hot" "use"))

(define (list-words mytrie)
 (list-words2 (trie-children mytrie)))
  
(define (list-words2 listofnodes)
  (cond
    [(empty? listofnodes) empty]
    [else (append (list-words2 (rest listofnodes)) (char/acc (first listofnodes) empty) empty)]
    ))

(define (char/acc mynode characc)
  (cond [(empty? (tnode-children mynode))
         (cond
           [(tnode-ends-word? mynode) (append (list (tnode-key mynode)) characc)]
           [else empty])]
        
        [(tnode-ends-word? mynode)
          (append (list (list->string characc))
          (append (list-words2 (rest (tnode-children mynode))) 
                  (char/acc (first (tnode-children mynode))
                            (append (list (tnode-key mynode)) characc))))]
        
        [(not (tnode-ends-word? mynode))
         (append (list-words2 (rest (tnode-children mynode))) 
                  (char/acc (first (tnode-children mynode))
                            (append (list (tnode-key mynode)) characc)))]
        ))
         

(list-words h-u-trie)
