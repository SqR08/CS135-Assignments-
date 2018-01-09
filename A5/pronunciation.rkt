;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pronunciation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steven Huang (20724105)
;; CS135 Fall 2017
;; Assignment 05, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the code in this file.
;; Do not remove this line.
(require "pronunciationlib.rkt")

;; The data definitions as given in the question.

;; A Vowel is a (list Sym (anyof 0 1 2))

;; A Phoneme is an (anyof Sym Vowel)

;; A Pronunciation is a (listof Phoneme)
;; requires: the list contains exactly one vowel with a stress of 1

;; A Dictionary is a (listof (list Str Pronunciation))
;; requires: The strings in each sub-list appear in alphabetical
;;           order in the Dictionary.

;; --------------------------
;; Place your functions here!
;; --------------------------


   
;;2a)
;;(vowelcount pronunciation) finds the number of elements in the pronunciation
;;that are lists
;;vowelcount: List -> Nat
;;Examples:
(check-expect (vowelcount '((AE 1) K T R (AH 0) S)) 2) 

(define (vowelcount pronunciation)
 (cond
   [(empty? pronunciation) 0]
   [(cons? (first pronunciation)) (+ 1 (vowelcount (rest pronunciation)))]
   [else (vowelcount (rest pronunciation))]
   
))

;;(num-syllables myword mydictionary) produces the
;;number of syllables of a given word(myword) and
;;according to the given dictionary(mydictionary)
;;num-syllables: String Dictionary -> Nat
;;Examples:
(check-expect (num-syllables "bigballerbrand" toy-dictionary) 0)
(check-expect (num-syllables "actress" toy-dictionary) 2)

(define (num-syllables myword mydictionary)
  (cond
    [(empty? mydictionary) 0]
    [(equal? myword (first (first mydictionary)))
             (vowelcount (second (first mydictionary)))]
    [else (num-syllables myword (rest mydictionary))]
    ))

;;Tests:
(check-expect (num-syllables "adopt" toy-dictionary) 2)
(check-expect (num-syllables "word" empty) 0)

;;2b)
(define small-dictionary
  (list
   (list "actress" '((AE 1) K T R (AH 0) S))
   (list "adopt" '((AH 0) D (AA 1) P T))
   (list "awful" '((AA 1) F (AH 0) L))
   (list "billion" '(B (IH 1) L Y (AH 0) N))))

;;(find-pattern pronunciation) takes in a pronunciation
;;and produces the stress pattern as a list of num
;;find-pattern: Pronunciation -> listof (any of 0 1 2)
;;Examples:
(check-expect (find-pattern '((AE 1) K T R (AH 0) S)) (list 1 0))
(define (find-pattern pronunciation)
  (cond
    [(empty? pronunciation) empty]
    [(cons? (first pronunciation)) (cons (second (first pronunciation))
                (find-pattern (rest pronunciation)))]
    [else (find-pattern (rest pronunciation))]
    ))
        

;;(find-stress-pattern stress-pattern mydictionary) produces a list
;;of words in a dictionary with the given stress pattern
;;find-stress-pattern: (listof (any of 0 1 2)) Dictionary -> listof Str
;;Examples:
(check-expect (find-stress-pattern '(0 2) empty) empty)
(check-expect (find-stress-pattern '(0 1) small-dictionary) '("adopt"))

(define (find-stress-pattern stress-pattern mydictionary)
  (cond
    [(or (empty? stress-pattern) (empty? mydictionary)) empty]
    [(equal? stress-pattern (find-pattern (second (first mydictionary))))
      (cons (first (first mydictionary))
            (find-stress-pattern stress-pattern (rest mydictionary)))]
    [else (find-stress-pattern stress-pattern (rest mydictionary))]
    ))

;;Tests:
(check-expect (find-stress-pattern '(1 0) small-dictionary)
      '("actress" "awful" "billion"))
(check-expect (find-stress-pattern '(0 1) toy-dictionary)
      '("adopt" "concrete" "deprive" "describe" "petite"))     


;;2c)

;;(suffix-finder pronunciation) finds the suffix
;;of the word’s pronunciation from the vowel
;;with primary stress until the end of the word.
;;suffix-finder: Pronunciation -> Pronunciation 
;;Examples:
(check-expect (suffix-finder '((AH 1) G L (IY 0))) (list (list 'AH 1) 'G 'L
                                                         (list 'IY 0)))

(define (suffix-finder pronunciation)
 (cond
   [(empty? pronunciation) empty]
   [(cons? (first pronunciation))
    (cond
      [(= (second (first pronunciation)) 1)
      (cons (first pronunciation)(rest pronunciation))]
      [else (suffix-finder (rest pronunciation))])]
   [else (suffix-finder (rest pronunciation))]
   ))

;;(pronunciation-finder myword mydictionary) produces the pronunciation
;;of a word from a ditionary
;;pronunciation: Str Dictionary -> Pronunciation
;;Examples:
(check-expect (pronunciation-finder "ugly" toy-dictionary)
              (list (list 'AH 1) 'G 'L (list 'IY 0)))

(define (pronunciation-finder myword mydictionary)
 (cond
   [(empty? mydictionary) empty]
   [(equal? myword (first (first mydictionary)))
    (second (first mydictionary))]
   [else (pronunciation-finder myword (rest mydictionary))]
   ))

;;Function that outputs all the words in a dictionary
;;that rhyme with a the inputted word
;;Examples
(check-expect (wordoutput "ugly" (list (list 'AH 1) 'G 'L (list 'IY 0))
                          toy-dictionary) (list "smugly"))

(define (wordoutput myword suffix mydictionary)
  (cond
    [(empty? mydictionary) empty]
    [(equal? suffix (suffix-finder (second (first mydictionary))))
      (cond
        [(not (equal? myword (first (first mydictionary))))
         (cons (first (first mydictionary)) (wordoutput myword suffix
                                                        (rest mydictionary)))]
        [else (wordoutput myword suffix (rest mydictionary))])]
    [else (wordoutput myword suffix (rest mydictionary))]
    ))

;;(find-rhymes myword mydictionary consumes a word and a dictionary
;;and produces a list of words that rhyme with the given word
;;find-rhymes Str Dictionary -> listof Str
;;Examples:
(check-expect (find-rhymes "adopt" empty) empty)
(check-expect (find-rhymes "adopt" toy-dictionary) '("swapped"))

(define (find-rhymes myword mydictionary)
(wordoutput myword (suffix-finder (pronunciation-finder myword mydictionary))
            mydictionary))

;;Tests:
(check-expect (find-rhymes "ugly" toy-dictionary) '("smugly"))
(check-expect (find-rhymes "five" toy-dictionary) '("deprive"))
         

