#lang racket
(require "../week2/fence.rkt")
(require "../week2/binary.rkt")
(require "lab.rkt")


(define (nth-beast-number n)
  (string-repeat "666" n)
  )
; (~a (string-ref "a" 0))

(define (occurences string char)
  (define (occurences-iter index total)
    (cond
      [(>= index (string-length string)) total]
      [(equal? char (~a (string-ref string index))) (occurences-iter (+ 1 index) (+ 1 total))]
      [(occurences-iter (+ 1 index) total)]
      )
    )
  (occurences-iter 0 0))
;(occurences "desislavaaaaa" "a")

(define (is-hack? n)
  (and (is-palindrome? (to-binary-string n)) (= 1 (remainder (occurences (to-binary-string n) "1") 2)))
  )
(define (next-hack n)
  (define (find-next current)
    (if (is-hack? current) current
        (find-next (+ 1 current))))
  (find-next (+ 1 n))
 )
;(next-hack 8031)

(define (p-score number)
  (define (p-score-iter new-number total)
    (if (is-palindrome? (number->string new-number)) (+ total 1)
        (p-score-iter (+ new-number (string->number (string-reverse (number->string new-number)))) (+ total 1))
       )
   )
  (p-score-iter number 0)
)
(p-score 198)
(p-score 48)