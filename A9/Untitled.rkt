;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))


(define (coordinates myrectangle)
  (local [(define width (third myrectangle))
         (define height (fourth myrectangle))

         ;;given an y coordinate finds all the possible x coordinates 
          (define (possiblexcoordinates/acc x y lstsofar) 
          (cond [(= (+ width x) 0) lstsofar]
                [else (possiblexcoordinates/acc (- x 1) y
                                (cons (list (+ x width) y) lstsofar))]))]

  (possiblexcoordinates/acc (first myrectangle) (second myrectangle) empty) 

    ))

(coordinates (list 1 2 3 4))