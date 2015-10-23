#lang racket
(provide is-palindrome?)
(define (string-reverse str)
  (define (string-inside counter reversed)
    (if (= 0 counter) (string-append reversed (~a (string-ref str 0)))
        (string-inside (- counter 1) (string-append reversed (~a (string-ref str counter))))
    )
   )
  (string-inside (- (string-length str) 1) "")
)

(define (is-palindrome? str)
  (string=? str (string-reverse str))
)
(define (fast-exponent x n)
  (define (fast-inside result power number)
    (cond
      [(= power 0) result]
      [(= (remainder power 2) 0) (expt (fast-inside result (quotient power 2) number) 2)]
      [else (* (fast-inside result (- power 1) number) number)]))
  (fast-inside 1 n x)
    
  )


(define (sum-divisors n)
  (define (sum-iter result current)
    (cond 
    [(>= current n) result]
    [else (if (= (remainder n current) 0) (sum-iter (+ result current) (+ 1 current))
            (sum-iter result (+ 1 current)))]
    )
 )
 (sum-iter 0 1)
)
(define (perfect? n)
  (= n (sum-divisors n))
 )


(define (occurences a x)
  (define (occurences-iter current counter)
    (if (= current 0) counter
        (occurences-iter (quotient current 10) (if (= (remainder current 10) a) (+ 1 counter)
                                                     counter))))
  (occurences-iter x 0)
)


(define (increasing? x)
  (define (increasing-iter bool-result current last-num)
    (cond
      [(= current x) (increasing-iter #t (quotient current 10) (remainder current 10))]
      [(= current 0) bool-result]
      [(equal? bool-result #t) (increasing-iter (< (remainder current 10) last-num) (quotient current 10) (remainder current 10))]
      [else #f])
  )
  (increasing-iter #t x 0)
  )
(increasing? 12)

(define (contains-digits? x y)
;current is initially y!
  (define (contains-iter current searched-number)
    (cond
      [(= current 0) (> (occurences searched-number x) 0)]
      [(> (occurences searched-number x) 0) (contains-iter (quotient current 10) (remainder current 10))]
      [else #f]
      )
    )
  (contains-iter (quotient y 10) (remainder y 10))
  )

