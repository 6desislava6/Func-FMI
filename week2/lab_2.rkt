#lang racket
(define (fact n)
  (define (fact-iter i result)
    (if (> i n) result
        (fact-iter (+ i 1) (* result i)))
  )
  (fact-iter 1 1)
)

(define (sum-digits n)
  (define (sum-digits-iter n result)
    (if (< n 10) (+ n result)
      (sum-digits-iter (quotient n 10) (+ result (remainder n 10))))
  )
  (if (< n 0) (sum-digits-iter (- n) 0)
      (sum-digits-iter n 0))
)

(define (fib n)
  (define (fib-iter i a b)
    (if (= i n) (+ a b)
        (fib-iter (+ i 1) b (+ a b))))
  (fib-iter 3 1 1)
  )

(define (sum-in-range a b)
  (define(sum-iter result current b)
    (cond
      [(= current b) (+ b result)]
      [else (sum-iter (+ current result) (+ current 1) b)]))
  (sum-iter 0 a b)
)


(define (sum-divisors n)
  (define (sum-iter result current)
    (cond 
    [(> current n) result]
    [(= current n) (+ result current)]
    [else (if (= (remainder n current) 0) (sum-iter (+ result current) (+ 1 current))
            (sum-iter result (+ 1 current)))]
    )
 )
 (sum-iter 0 1)
)

(define (prime? n)
  (define (prime_inside n max current)
    (cond
      [(= n 1) #f]
      [(= n 2) #t]
      [(>= current max) (not (= (remainder n current) 0))]
      [else (and (not (= (remainder n current) 0)) (prime_inside n max (+ current 1)))]
      )
    )
    (prime_inside n (round (+ (sqrt n))) 2)
)

(define (primes-in-range a b)
    (define(primes-iter result current b)
      (cond
        [(= current b) (if (prime? b) (+ b result)
                           result)]
        [(prime? current) (primes-iter (+ current result) (+ current 1) b)]
        [else (primes-iter result (+ current 1) b)]))
  (primes-iter 0 a b)
  )
(primes-in-range 2 11)

(define (sum-prime-divisors n)
  (define (sum-iter result current)
    (cond 
    [(> current n) result]
    [(= current n) (if (prime? current) (+ result current)
                       result)]
    [else (if (and (= (remainder n current) 0) (prime? current)) (sum-iter (+ result current) (+ 1 current))
            (sum-iter result (+ 1 current)))]
    )
    )
  (sum-iter 0 1)
  )
(define (oddly? n)
  (define (total-divisors n total)
    ))