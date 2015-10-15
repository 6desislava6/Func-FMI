#lang racket

(define (series a b n)
; a + b is the next number
    (define (series-inside current a b)
      (cond
      [(= n 1) a]
      [(= n 2) b]
      [(= current n) (+ a b)]
      [else (series-inside (+ 1 current) b (+ a b))]  
      )
    )
  (series-inside 3 a b)
 )
 
(define (lucas n)
  (series 2 1 n)
)
(define (fibonacci n)
  (series 1 1 n)
 )
(fibonacci 5)

(define (summed-member n)
  (+ (lucas n) (fibonacci n))
  )

(define (nth-sum function-series n)
  (define (function-inside counter result)
    (if (= counter n) (+ result (function-series n))
        (function-inside (+ 1 counter) (+ result (function-series counter)))
        )
    )
  (function-inside 1 0)
)
(define (nth-lucas-sum n)
  (nth-sum lucas n)
  )
(define (nth-fibonacci-sum n)
  (nth-sum fibonacci n)
  )
(nth-fibonacci-sum 10)
(nth-lucas-sum 10)

(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n))
  )
