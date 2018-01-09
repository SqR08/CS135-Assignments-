;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname creditcheck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Steven Huang (20724105)
;; CS 135 Fall 2017
;; Assignment 3 Problem 3
;; ***************************************************
;;

(define-struct date (year month day))
;; A Date is a (make-date Nat Nat Nat)
;; requires: year/month/day corresponds to a valid date
;; (in the Gregorian calendar)
(define-struct transaction (tdate amount category))
;; A Transaction is a (make-transaction Date Num Sym)
(define-struct account (name expires limit threshold exception))
;; An Account is a (make-account Str Date Num Num Sym)
;; requires: 0 < threshold < limit

;;(3a)
;;(date<=? date1 date2) consumes two dates (date1, date2)
;;and produces true if the first date occurs before the
;;second date
;;date<=?: Date Date -> Bool
;;Examples
(check-expect (date<=? (make-date 2017 08 27) (make-date 2017 08 28)) true)
(check-expect (date<=? (make-date 2017 08 27) (make-date 2017 08 26)) false)

(define (date<=? date1 date2)
 (cond
   [(< (date-year date1) (date-year date2)) true]
   [(> (date-year date1) (date-year date2)) false]
   [(< (date-month date1) (date-month date2)) true]
   [(> (date-month date1) (date-month date2)) false]
   [(< (date-day date1) (date-day date2)) true]
   [(> (date-day date1) (date-day date2)) false]
   [else true]
   ))

;;Tests
(check-expect (date<=? (make-date 2017 07 27) (make-date 2017 08 27)) true)
(check-expect (date<=? (make-date 2017 08 27) (make-date 2017 06 27)) false)
(check-expect (date<=? (make-date 2016 08 27) (make-date 2017 08 27)) true)
(check-expect (date<=? (make-date 2017 08 27) (make-date 2016 08 27)) false)
(check-expect (date<=? (make-date 2017 08 27) (make-date 2017 08 27)) true)

;;(3b)
;;(approve? mytransaction myaccount) consumes a transaction (mytransaction)
;;and an account (myaccount) and produces true if the transaction amount
;;does not exceed account limit and transaction date is not after the date
;;the card expires
;;approve? mytransaction myaccount: Transaction Account -> Bool
;;Examples:
(check-expect (approve? (make-transaction (make-date 2017 08 27) 80 'cats)
                        (make-account 'bob (make-date 2017 09 27) 100 60 'dogs))
                         true) 
(check-expect (approve? (make-transaction (make-date 2017 10 27) 80 'cats)
                        (make-account 'bob (make-date 2017 09 27) 100 60 'dogs))
                        false)

(define (approve? mytransaction myaccount)
  (cond
    [(and (< (transaction-amount mytransaction) (account-limit myaccount))
           (date<=? (transaction-tdate mytransaction)
                     (account-expires myaccount))
            ) true]
    [else false]
    ))

;;Tests
;;Examples:
(check-expect (approve? (make-transaction (make-date 2017 08 27) 110 'cats)
                        (make-account 'bob (make-date 2017 09 27) 100 60 'dogs))
                         false) 
(check-expect (approve? (make-transaction (make-date 2018 10 27) 120 'cats)
                        (make-account 'bob (make-date 2017 09 27) 100 60 'dogs))
                        false)

;; (3c)
;; (alert? mytransaction myaccount) consumes a transaction (mytransaction)
;; and an account (myaccount) and produces true if the transaction is approved
;; but the transaction amount exceeds the threshold and the transaction category
;; is not the the exception category in the account
;; alert? Transaction Account -> Bool
;;Examples
(check-expect (alert? (make-transaction (make-date 2017 08 27) 90 'cats)
                      (make-account 'bob (make-date 2017 09 27) 100 60 'dogs))
                      true)
(check-expect (alert? (make-transaction (make-date 2017 08 27) 110 'cats)
                      (make-account 'bob (make-date 2017 09 27) 100 60 'dogs))
                      false)
 
(define (alert? mytransaction myaccount)
  (cond
    [(and (< (transaction-amount mytransaction) (account-limit myaccount))
          (> (transaction-amount mytransaction) (account-threshold myaccount))
          (not (symbol=? (transaction-category mytransaction)
                           (account-exception myaccount)))
            ) true]
    [else false]
    ))

;;Tests
(check-expect (alert? (make-transaction (make-date 2017 08 27) 90 'dogs)
                      (make-account 'bob (make-date 2017 09 27) 100 60 'dogs))
                      false)
(check-expect (alert? (make-transaction (make-date 2017 08 27) 120 'cats)
                      (make-account 'bob (make-date 2017 09 27) 100 80 'dogs))
                      false)
 