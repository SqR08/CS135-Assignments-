;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 2 Problem 4
;; ***************************************************
;;

;; 4(a)
;; (can-donate-to/cond? donor recipient) determines whether or not a donor's 
;; bloodtype(donor) is acceptable for a recipients blood type(recipient
;;can-donate-to/cond? donor recipient: Sym Sym -> Sym
;; requires donor, recipient either 'O-, 'O+, 'A-, 'A+,
;; 'B-, 'B+, 'AB-, 'AB+
;;Examples:
(check-expect (can-donate-to/cond? 'O- 'O-) #t)
(check-expect (can-donate-to/cond? 'O- 'A+) #t)
              
(define (can-donate-to/cond? donor recipient)
  
   (cond
       [(symbol=? donor 'O-) #t]
       [(symbol=? recipient 'AB+) #t]
       [(symbol=? donor 'O+)
         (cond
           [(symbol=? recipient 'O+) #t]
           [(symbol=? recipient 'A+) #t]
           [(symbol=? recipient 'B+) #t][else #f])]
       
      [(symbol=? donor 'A-)
           (cond
              [(symbol=? recipient 'A-) #t]
              [(symbol=? recipient 'A+) #t]
              [(symbol=? recipient 'AB-) #t][else #f])]
      
      [(symbol=? donor 'A+)
           (cond
             [(symbol=? recipient 'A+) #t][else #f])]
      
      [(symbol=? donor 'B-)
            (cond
              [(symbol=? recipient 'B-) #t]
              [(symbol=? recipient 'B+) #t]
              [(symbol=? recipient 'AB-) #t][else #f])]

      [(symbol=? donor 'B+)
            (cond
              [(symbol=? recipient 'B+) #t][else #f])]

     [(symbol=? donor 'AB-)
            (cond
              [(symbol=? recipient 'AB-) #t][else #f])]

     [else #f]
     
     ))

;;Tests
(check-expect (can-donate-to/cond? 'O- 'O-) #t)
(check-expect (can-donate-to/cond? 'O- 'O+) #t)
(check-expect (can-donate-to/cond? 'O- 'A-) #t)
(check-expect (can-donate-to/cond? 'O- 'A+) #t)
(check-expect (can-donate-to/cond? 'O- 'B-) #t)
(check-expect (can-donate-to/cond? 'O- 'B+) #t)
(check-expect (can-donate-to/cond? 'O- 'AB-) #t)
(check-expect (can-donate-to/cond? 'O- 'AB+) #t)
(check-expect (can-donate-to/cond? 'O+ 'O+) #t)
(check-expect (can-donate-to/cond? 'O+ 'A+) #t)
(check-expect (can-donate-to/cond? 'O+ 'B+) #t)
(check-expect (can-donate-to/cond? 'O+ 'AB+) #t)
(check-expect (can-donate-to/cond? 'A- 'A-) #t)
(check-expect (can-donate-to/cond? 'A- 'A+) #t)
(check-expect (can-donate-to/cond? 'A- 'AB-) #t)
(check-expect (can-donate-to/cond? 'A- 'AB+) #t)
(check-expect (can-donate-to/cond? 'A+ 'A+) #t)
(check-expect (can-donate-to/cond? 'A+ 'AB+) #t)
(check-expect (can-donate-to/cond? 'B- 'B-) #t)
(check-expect (can-donate-to/cond? 'B- 'B+) #t)
(check-expect (can-donate-to/cond? 'B- 'AB-) #t)
(check-expect (can-donate-to/cond? 'B- 'AB+) #t)
(check-expect (can-donate-to/cond? 'B+ 'B+) #t)
(check-expect (can-donate-to/cond? 'B+ 'AB+) #t)
(check-expect (can-donate-to/cond? 'AB- 'AB-) #t)
(check-expect (can-donate-to/cond? 'AB- 'AB+) #t)
(check-expect (can-donate-to/cond? 'AB+ 'AB+) #t)


;;4(b)
;; (can-donate-to?bool donor recipient) determines whether or not a donor's 
;; bloodtype is acceptable for a recipients blood type

;;can-donate-to/bool? donor recipient: Sym Sym -> Sym

;;Examples:
(check-expect (can-donate-to/bool? 'O- 'O-) #t)
(check-expect (can-donate-to/bool? 'O- 'A+) #t)


(define (can-donate-to/bool? donor recipient)
  (or  
  (symbol=? donor 'O-)
  (symbol=? recipient 'AB+)
  (and (symbol=? donor 'O+) (symbol=? recipient 'O+))
  (and (symbol=? donor 'O+) (symbol=? recipient 'A+))
  (and (symbol=? donor 'O+) (symbol=? recipient 'B+))

  (and (symbol=? donor 'A-) (symbol=? recipient 'A-))
  (and (symbol=? donor 'A-) (symbol=? recipient 'A+))
  (and (symbol=? donor 'A-) (symbol=? recipient 'AB-))

  (and (symbol=? donor 'A+) (symbol=? recipient 'A+))
  
  (and (symbol=? donor 'B-) (symbol=? recipient 'AB-))
  (and (symbol=? donor 'B-) (symbol=? recipient 'B+))
  (and (symbol=? donor 'B-) (symbol=? recipient 'B-))
  (and (symbol=? donor 'B-) (symbol=? recipient 'AB-))

  (and (symbol=? donor 'B+) (symbol=? recipient 'B+))

  (and (symbol=? donor 'AB-) (symbol=? recipient 'AB-))

  )
)

;;Tests
(check-expect (can-donate-to/bool? 'O- 'O-) #t)
(check-expect (can-donate-to/bool? 'O- 'O+) #t)
(check-expect (can-donate-to/bool? 'O- 'A-) #t)
(check-expect (can-donate-to/bool? 'O- 'A+) #t)
(check-expect (can-donate-to/bool? 'O- 'B-) #t)
(check-expect (can-donate-to/bool? 'O- 'B+) #t)
(check-expect (can-donate-to/bool? 'O- 'AB-) #t)
(check-expect (can-donate-to/bool? 'O- 'AB+) #t)
(check-expect (can-donate-to/bool? 'O+ 'O+) #t)
(check-expect (can-donate-to/bool? 'O+ 'A+) #t)
(check-expect (can-donate-to/bool? 'O+ 'B+) #t)
(check-expect (can-donate-to/bool? 'O+ 'AB+) #t)
(check-expect (can-donate-to/bool? 'A- 'A-) #t)
(check-expect (can-donate-to/bool? 'A- 'A+) #t)
(check-expect (can-donate-to/bool? 'A- 'AB-) #t)
(check-expect (can-donate-to/bool? 'A- 'AB+) #t)
(check-expect (can-donate-to/bool? 'A+ 'A+) #t)
(check-expect (can-donate-to/bool? 'A+ 'AB+) #t)
(check-expect (can-donate-to/bool? 'B- 'B-) #t)
(check-expect (can-donate-to/bool? 'B- 'B+) #t)
(check-expect (can-donate-to/bool? 'B- 'AB-) #t)
(check-expect (can-donate-to/bool? 'B- 'AB+) #t)
(check-expect (can-donate-to/bool? 'B+ 'B+) #t)
(check-expect (can-donate-to/bool? 'B+ 'AB+) #t)
(check-expect (can-donate-to/bool? 'AB- 'AB-) #t)
(check-expect (can-donate-to/bool? 'AB- 'AB+) #t)
(check-expect (can-donate-to/bool? 'AB+ 'AB+) #t)
