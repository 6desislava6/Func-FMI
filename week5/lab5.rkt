#lang racket
(define (sum2 numbers)
  (define (iter numbers r)
    (cond
      [(empty? numbers) r]
      [else (iter (rest numbers) (+ r (first numbers)))])
  )
  (iter numbers 0)
)


(define (length2 numbers)
  (define (iter numbers r)
    (cond
      [(empty? numbers) r]
      [else (iter (rest numbers) (+ r 1))])
  )
  (iter numbers 0)
)

(define (member? element given-list)
  (cond
    [(empty? given-list) #f]
    [(equal? element (first given-list)) #t ]
    [else (member? element (rest given-list))])
;[else (or (equal? element (first given-list)) (member? element (rest given-list))] 
  )

(define (list-ref2 given-list index)
  (define (iter given-list current)
    (cond
      [(empty? given-list) (raise-range-error 'list "list" "ending" index index 0 current 0 )]
      [(= index current) (first given-list)]
      [else (iter (rest given-list) (+ 1 current))]
      )
    )
  (iter given-list 0)
)


(define (range2 a b)
  (cond
    [(> a b) (list)]
    [(= a b) (list a)]
    [else (cons a (range2 (add1 a) b))]
    )
 )

(define (rangef f a b)
  (cond
    [(> a b) (list)]
    [else (cons (f a) (rangef f (add1 a) b))]
    )
 )

(define (number->list number)
  (define (iter number result-list)
    (cond
      [(= number 0) result-list]
      [else (iter (quotient number 10) (cons (remainder number 10) result-list))]
      ))
  (iter number (list))
 )

 (define (map2 f xs)
   (cond
     [(empty? xs) (list)]
     [else (cons (f (first xs)) (map2 f (rest xs)))]
   )
 )