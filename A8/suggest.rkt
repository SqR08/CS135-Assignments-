;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname suggest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; OPTIONAL -- spellcheck.rkt provides the following function:
;;
;; (spellcheck? s) determines if s is spelled correctly
;;   (according to a medium-sized wordlist)
;; spellcheck: Str -> Bool
;;
;; You may use this function for your own experiments
;; (and to show off your program to your friends & family)

;; Do NOT leave this require in your file when you submit your code.
;; (require "spellcheck.rkt")
;; [this file will be available after the A07 deadline]
;; NOTE: You do not need to open spellcheck.rkt in DrRacket to use it
;;       (opening the file in DrRacket may slow down your computer).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 08, Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: you should complete the documentation & tests (design recipe)
;; for all functions (except remove-at and remove-letters)
;; But remember, because most of your functions will not have a cond
;; or much logic, exhaustive tests may not be required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; A Word is a Str
;; requires: only lowercase letters appear in the word
;;           (no spaces, punctuation, etc.)

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))

;; 4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(remove-dups slst) consumea a list of strings (slst) and
;;and removes any duplicate words
;;remove-dups: (listof Word) -> (listof Word)
;;requires: slst is sorted in non-decreasing order
;;Examples:
(check-expect (remove-dups '("apple" "apple" "apples" "banana" "cherry"
                                     "cherry"))
              (list "apple" "apples" "banana" "cherry"))

(define (remove-dups slst)
  (foldr
        (lambda (x y) (cond [(empty? y) (list x)]
                            [(string=? x (first y)) (cons x (rest y))]
                            [else (cons x y)]))
        empty
        slst))
                          

;; 4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(ifoldr combine base list) consumes a list and
;;produces a new list made by combining the first of the list
;;and the recursive rest of the list while keeping track of the
;;index we are working at
;;ifoldr: (Nat X Y -> Y) Y (listof X) -> Y
;;Examples:
(check-expect (ifoldr (lambda (i x y) (cons (list i x) y)) empty '(a b c))
              (list (list 0 'a) (list 1 'b) (list 2 'c)))

(define (ifoldr combine base lst)
  (local
  [(define (ifoldr2 combine i base lst)
     (cond  [(empty? lst) base]
            [else
             (combine i (first lst)
                        (ifoldr2 combine (add1 i) base (rest lst)))]))]
   (ifoldr2 combine 0 base lst)))


;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-at i lst) removes element with index i from lst
;; remove-at: Nat (listof Any) -> (listof Any)
;; Examples:
(check-expect (remove-at 0 '(a b c d)) '(b c d))
(check-expect (remove-at 3 '(a b c d)) '(a b c))
(check-expect (remove-at 0 '()) '())

(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))


;; (remove-letters s) produces a list of Words,
;;    each with one letter removed from s
;; remove-letters: Word -> (listof Word)
;; Examples:
(check-expect (remove-letters "abc") '("bc" "ac" "ab"))
(check-expect (remove-letters "") '())

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc) (lambda (i) (list->string (remove-at i loc))))))

;; 4c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; insert letters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(insert-letters s) inserts every possible letter before each letter in
;;the word s
;;insert-letters: Word -> (listof Word)
;;Example:
(check-expect (insert-letters "a")
              (list "aa" "ba" "ca" "da" "ea" "fa" "ga"
                    "ha" "ia" "ja" "ka" "la" "ma" "na"
                    "oa" "pa" "qa" "ra" "sa" "ta" "ua"
                    "va" "wa" "xa" "ya" "za"))

(define (insert-letters s)
  (local [(define loc (string->list s))]
    (flatten (build-list (length loc) (lambda (i) (insert-before i loc))))))

;;takes a lists of lists and produces one list flattened
;;flatten: Nest-List-X -> (listof X)
(define (flatten lst)
  (foldr append empty lst))

;;(insert-before i loc) inserts every letter in the alphabet before a given
;;index i in a list of characters
;;insert-before: Nat (listof Char) -> (listof Str)
;;Example:
(check-expect (insert-before 0 (list #\a))
              (list "aa" "ba" "ca" "da" "ea" "fa" "ga" "ha" "ia" "ja" "ka" "la"
                    "ma" "na" "oa" "pa" "qa" "ra" "sa" "ta" "ua" "va" "wa" "xa"
                    "ya" "za"))

(define (insert-before i loc)
   (ifoldr (lambda (k x y)
             (cons (list->string (char-insert x i loc)) y))
               empty
               letters))

;;(char-insert letter i loc) inserts 1 letter into the index before the given
;;index in a list of characters
;;char-insert: Char Nat (listof Char) -> (listof Char)
;;Example:
(check-expect (char-insert #\a 0 (list #\a #\b #\c #\d))
              (list #\a #\a #\b #\c #\d))

(define (char-insert letter i loc)
  (ifoldr (lambda (k x y) (cond [(= k i) (append (list letter)
                                                 (cons x y))]
                                [else (cons x y)]))                 
                                
          empty
          loc ))  

;; trailing letters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(trailing-letter s) produces a list of words with every possible
;;letter inserted at the end of the word s
;;trailing-letters: Word -> (listof Word)
;;Example:
(check-expect (trailing-letters "a")
              (list "aa" "ab" "ac" "ad" "ae" "af" "ag" "ah" "ai" "aj" "ak"
                    "al" "am" "an" "ao" "ap" "aq" "ar" "as" "at" "au" "av"
                    "aw" "ax" "ay" "az"))


(define (trailing-letters s)
  (local [(define loc (string->list s))]
    (foldr (lambda (x y) (cons (list->string (append loc (list x))) y))
           empty
           letters)))


;; replace-letters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(replace=letters s) produces a list of words
;;with every letter of s replaced with every other possible
;;letter
;;replace-letters: Word -> (listof Word)
;;Example: 
(check-expect (replace-letters "a")
              (list "a" "b" "c" "d" "e" "f" "g" "h" "i"
                    "j" "k" "l" "m" "n" "o" "p" "q" "r"
                    "s" "t" "u" "v" "w" "x" "y" "z"))
 
(define (replace-letters s)
  (flatten (build-list (length (string->list s))
              (lambda (i) (indexinsert i (string->list s))))))


;;(indexinsert index loc) inserts every letter of the
;;alphabet into the given index of a list of characters
;;indexinsert: Nat (listof Char) -> (listof Str)
;;Example:
(check-expect (indexinsert 0 (list #\a))
              (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
                    "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
                    "w" "x" "y" "z"))

(define (indexinsert index loc)
  (ifoldr (lambda (i x y) (cons (list->string (char-replace x index loc)) y))
          empty
          letters))


;;(char-replace letter index loc) takes a letter, an index,
;;and puts the letter into the given index of a list of char
;;char-replace: Char Nat (listof char) -> (listof Char)
;;Example:
(check-expect (char-replace #\a 1 (list #\a #\b #\c #\d))
              (list #\a #\a #\c #\d))
              
(define (char-replace letter index loc)
  (ifoldr (lambda (i x y) (cond [(= i index) (cons letter y)]      
                                [else (cons x y)]))
          empty
          loc ))


;; swap-letters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(swap-letters s) produces a new list of words with each adjacent
;;pair of letters in the word s swapped
;;swap-letters: Word -> (listof Word)
;;Example:
(check-expect (swap-letters "word") (list "owrd" "wrod" "wodr"))

(define (swap-letters s)
  (local
    [(define loc (string->list s))]
    (build-list (sub1 (length loc)) (lambda (i) (indexswap i loc)))
    ))


;;(indexswap i loc) swaps the letter at an index i with the adjacent letter
;;in a list of characters
;;indexswap: Nat (listof Char) -> Str
;;Example:
(check-expect (indexswap 0 (list #\a #\b #\c #\d)) "bacd")

(define (indexswap i loc)
  (list->string
   (ifoldr (lambda (k x y)
            (cond
              [(= k i) (append (list (char-at-i (add1 k) loc))
                               (list (char-at-i k loc)) y)]
              [(= k (add1 i)) y] 
              [else (cons x y)])) 
              empty
              loc)))

;;(char-at-i i lst) produces the character at an index i
;;of a list of characters
;;char-at-i: Nat (listof Char) -> Char
;;Example:
(check-expect (char-at-i 0 (list #\a #\b #\c #\d)) #\a)

(define (char-at-i i lst)
  (ifoldr (lambda (k x y) (cond
                                   [(= k i) x]
                                   [else y]))
                empty
                lst))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You are not required to modify the definition of suggest,
;; but you may if you wish

;; suggest: Word (Word -> Bool) -> (listof Word)

(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))
