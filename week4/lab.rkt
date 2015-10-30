#lang racket
(define (calculate f a b)
  (f a b))

(define (twice f x)
  (f (f x))
)

(define (inc x)
  (+ 1 x))


; it's like a tail recursion
(define (call-n-times n f x)
  (define (call-iter counter result)
    (if (= counter n) (f result)
        (call-iter (+ 1 counter) (f result))
        )
    )
  (if (= n 0) x
      (call-iter 0 0))
)



(define (sum-range a b)
  (define (sum-iter counter result)
    (cond
      [(> a b) 0]
      [(= counter b) (+ result (* counter counter))]
      [else (sum-iter (+ 1 counter) (+ result (* counter counter)))]
      )
  )
  (sum-iter a 0)
 )
(sum-range 1 3)

(define (sum-range-func f a b)
  (define (sum-iter counter result)
    (cond
      [(> a b) 0]
      [(= counter b) (+ result (f counter))]
      [else (sum-iter (+ 1 counter) (+ result (f counter)))]
      )
  )
  (sum-iter a 0)
 )

(define (first-sum a b)
  (sum-range-func (lambda (x) (expt x x)) a b)
  )

(define (second-sum a b)
  (sum-range-func (lambda (x) (- (* x x) 2)) a b)
  )

(define (third-sum a b)
  (sum-range-func (lambda (x) (/ x 10)) a b)
  )
(first-sum 1 3)

(twice (lambda (x) (string-append x "!")) "Racket")

(define (get-operation str)
  (cond
    [(string=? str "+") +]
    [(string=? str "-") -]
    [(string=? str "*") *]
    [(string=? str "/") /]
    [else (lambda args "not okay")]
    )
 )


(define (f x)
  (lambda (y)
    (lambda (z)
      (+ (+ x y) z))))

(define (compose f g)
  (lambda (x)
    (f (g x))
  ))

(define (negate p)
  (lambda (x) (not (p x))))

(define (even? x)
  (= (remainder x 2) 0))