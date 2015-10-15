#lang racket
(define (string-repeat str n)
  (define (string-repeat-inside counter result)
    (if (= counter (- n 1)) (string-append result str)
        (string-repeat-inside (+ counter 1) (string-append result str))))
  (string-repeat-inside 0 "")
 )

(define (fence n)
  (let ((dashes (string-repeat "-" (round (+ 1 (log n))))))
    (string-append (string-append (string-append (string-append (string-append (string-append "{" dashes) ">") (number->string n)) "<") dashes) "}")
    )  
  )
(fence 100)