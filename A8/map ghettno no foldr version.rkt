;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |map ghettno no foldr version|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (nmap fn list)
  (cond [(empty? list) empty]
        [(list? (first list))
         (cons (nmap fn (first list))
               (nmap fn (rest list)))]
        [else (cons (fn (first list))
                    (nmap fn (rest list)))]
        ))