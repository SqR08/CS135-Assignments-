;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname battle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 3 Problem 4
;; ***************************************************
;;

(define-struct card (strength colour))
;; A Card is a (make-card Nat Sym)
;; requires: 1 <= strength <= 9
;; color is one of: ’red ’yellow ’green ’blue ’purple ’brown
(define-struct hand (c1 c2 c3))
;; A Hand is a (make-hand Card Card Card)


;;Defining constants for ranks based on attributes 
(define rank1 1)
(define rank2 2)
(define rank3 3)
(define rank4 4)
(define rank5 5)

;;(rank myhand) consumes a hand (myhand) and produces a
;;rank from 1-5 based on its attribute
;;rank: Hand -> Num
;;Examples
(check-expect (rank (make-hand (make-card 5 'red)(make-card 6 'red)
                               (make-card 7 'red))) rank1)
(check-expect (rank (make-hand (make-card 5 'blue)(make-card 6 'red)
                               (make-card 7 'red))) rank4)
(define (rank myhand)
  (cond
    ;;Colour-run
    [(and
      (or
        (and (= (card-strength (hand-c2 myhand))
                (+ (card-strength (hand-c1 myhand)) 1))
             (= (card-strength (hand-c3 myhand)) (+ (card-strength
                (hand-c1 myhand)) 2)))
        (and (= (card-strength (hand-c2 myhand))
                (- (card-strength (hand-c1 myhand)) 1))
             (= (card-strength (hand-c3 myhand))
                (- (card-strength (hand-c1 myhand)) 2)))
        (and (= (card-strength (hand-c2 myhand))
                (+ (card-strength (hand-c1 myhand)) 2))
             (= (card-strength (hand-c3 myhand))
                (+ (card-strength (hand-c1 myhand)) 1)))
        (and (= (card-strength (hand-c2 myhand))
                (- (card-strength (hand-c1 myhand)) 2))
             (= (card-strength (hand-c3 myhand))
                (- (card-strength (hand-c1 myhand)) 1)))
        (and (= (card-strength (hand-c2 myhand))
                (- (card-strength (hand-c1 myhand)) 1))
             (= (card-strength (hand-c3 myhand))
                (+ (card-strength (hand-c1 myhand)) 1)))
        (and (= (card-strength (hand-c2 myhand))
                (+ (card-strength (hand-c1 myhand)) 1))
             (= (card-strength (hand-c3 myhand))
                (- (card-strength (hand-c1 myhand)) 1)))
        )
        (and (symbol=? (card-colour (hand-c1 myhand))
                       (card-colour(hand-c2 myhand)))
             (symbol=?(card-colour (hand-c2 myhand))
                      (card-colour (hand-c3 myhand))))) rank1]
    ;;three-of-a-kind
      [(= (card-strength (hand-c1 myhand)) (card-strength (hand-c2 myhand))
          (card-strength(hand-c3 myhand))) rank2] 
    ;;colour
      [(and (symbol=? (card-colour (hand-c1 myhand))
                      (card-colour(hand-c2 myhand)))
            (symbol=?(card-colour (hand-c2 myhand))
                     (card-colour (hand-c3 myhand)))) rank3]
    ;;run
    [(or
     (and (= (card-strength (hand-c2 myhand))
             (+ (card-strength (hand-c1 myhand)) 1))
          (= (card-strength (hand-c3 myhand))
             (+ (card-strength (hand-c1 myhand)) 2)))
        (and (= (card-strength (hand-c2 myhand))
                (- (card-strength (hand-c1 myhand)) 1))
             (= (card-strength (hand-c3 myhand))
                (- (card-strength (hand-c1 myhand)) 2)))
        (and (= (card-strength (hand-c2 myhand))
                (+ (card-strength (hand-c1 myhand)) 2))
             (= (card-strength (hand-c3 myhand))
                (+ (card-strength (hand-c1 myhand)) 1)))
        (and (= (card-strength (hand-c2 myhand))
                (- (card-strength (hand-c1 myhand)) 2))
             (= (card-strength (hand-c3 myhand))
                (- (card-strength (hand-c1 myhand)) 1)))
        (and (= (card-strength (hand-c2 myhand))
                (- (card-strength (hand-c1 myhand)) 1))
             (= (card-strength (hand-c3 myhand))
                (+ (card-strength (hand-c1 myhand)) 1)))
        (and (= (card-strength (hand-c2 myhand))
                (+ (card-strength (hand-c1 myhand)) 1))
             (= (card-strength (hand-c3 myhand))
                (- (card-strength (hand-c1 myhand)) 1)))) rank4]
    ;;no attribute = lowest rank
    [else rank5]))

;;(sum myhand) consumes a hand (myhand) and gives the sum of
;; the cards in a hand
;;sum: Hand -> Num
(define (sum myhand)
  (+ (card-strength (hand-c2 myhand)) (card-strength (hand-c1 myhand))
     (card-strength (hand-c3 myhand))))

;;(battle hand1 hand2) consumes two hands (hand1, hand2)
;;and produces the winning player based on the rules of
;;Battle line
;;battle: Hand Hand -> String
;;Examples
(check-expect (battle (make-hand (make-card 5 'red)(make-card 6 'red)
                                 (make-card 7 'red))
                      (make-hand (make-card 5 'blue)
                                 (make-card 6 'red)
                                 (make-card 7 'red))) 'player1)
(check-expect (battle (make-hand (make-card 1 'blue)(make-card 2 'red)
                                 (make-card 7 'green))
                      (make-hand (make-card 2 'red)
                                 (make-card 3 'green)
                                 (make-card 4 'blue))) 'player2)

(define (battle hand1 hand2)
  (cond
    [(and (= (rank hand1) 5) (= (rank hand2) 5))
       (cond
         [(>= (sum hand1) (sum hand2)) 'player1]
         [else 'player2] )]
    [(< (rank hand1)(rank hand2)) 'player1]
    [(> (rank hand1)(rank hand2)) 'player2]
    [(= (rank hand1) (rank hand2))
       (cond
         [(>= (sum hand1) (sum hand2)) 'player1] 
         [else 'player2] )]
    ))

;;Tests
(check-expect (battle (make-hand (make-card 1 'blue)(make-card 2 'red)
                                 (make-card 7 'green))
                      (make-hand (make-card 1 'red)
                                 (make-card 3 'green)
                                 (make-card 4 'blue))) 'player1)
(check-expect (battle (make-hand (make-card 1 'blue)(make-card 2 'red)
                                 (make-card 4 'green))
                      (make-hand (make-card 1 'red)
                                 (make-card 2 'green)
                                 (make-card 7 'blue))) 'player2)
(check-expect (battle (make-hand (make-card 3 'red)(make-card 2 'red)
                                  (make-card 1 'red))
                      (make-hand (make-card 2 'red)
                                 (make-card 3 'red)
                                 (make-card 4 'red))) 'player2)
(check-expect (battle (make-hand (make-card 7 'red)(make-card 9 'red)
                                  (make-card 8 'red))
                      (make-hand (make-card 2 'red)
                                 (make-card 3 'red)
                                 (make-card 4 'red))) 'player1)

