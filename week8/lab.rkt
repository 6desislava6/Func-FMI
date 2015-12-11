#lang racket
(define (compose fs)
  (define (apply fs x)
    (if (empty? fs)
        x
        ((first fs) (apply (rest fs) x))))
  (lambda (x) (apply fs x)))

;((compose (list even? (lambda (x) (* x x)) (lambda (x) ( + 1 x)))) 2)
 ;(foldr compose id fs)



(define (take n items)
  (cond
    [(empty? items) '()]
    [(= n 0) (list)]
    [else (cons (first items) (take (- n 1) (rest items)))]))

(define (drop n items)
  (cond
    [(empty? items) '()]
    [(= n 0) items]
    [else (drop (- n 1) (rest items))]
  ))

(define (slice start len items)
  (take len (drop start items))
  )

(define (sublists-at index items)
  (let* ([mylength (length items)]
        [helper (range 1 (+ 1 mylength) 1)])
    
    (drop index (map (lambda (item) (slice index (- item index) items)) helper))
 ))

(define (uniq items)
  1
  )

(define (all-increasing items)
  (cond
    [(empty? items) #t]
    [(= (length items) 1) #t]
    [(= (length items) 2) (<= (first items) (first (rest items))) ]
    [else (and (<= (first items) (first (rest items))) (all-increasing (rest items))) ]
  ))


(define (permute-pairs items start end)
 (cond
   [(= end (length items)) (permute-pairs items (+ start 1) (+ start 2))]
   [(= start (length items)) '()]
   [else (cons (cons (list-ref items start) (list-ref items end)) (permute-pairs items start (+ end 1)))]
   )
)
  ;car cdr
(define (longest-increasing-subsequence items)
  (let
      ([permutations (map (lambda (pair) (slice (car pair) (- (cdr pair) (car pair)) items)) (permute-pairs (range 0 (length items)) 0 0))])
    (filter (lambda (x) (car x)) (map (lambda (items) (cons (all-increasing items) (length items))) permutations))
    )
  
 )    
(longest-increasing-subsequence '(1 2 3 0 1 2 3 4 0 1 2 3))


;(all-increasing '(1 2 3 4 5 6))

(define (uniq2 items)
  (define (iter items result)
    (cond
      [(empty? items) result]
      [(member (first items) result) (iter (rest items) result)]
      [else (iter (rest items) (append result (first items)))]
      )
    )
  (iter items (list))
  )