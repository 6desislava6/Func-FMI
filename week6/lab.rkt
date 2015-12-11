#lang racket
(define (filter p xs)
  (cond
      [(empty? xs) xs]
      [(p (first xs)) (cons (first xs) (filter p (rest xs)))]
      [else (filter p (rest xs))]))

; like reduce
; from right to left
(define (foldr op z xs)
  (cond
    [(empty? xs) z]
    [else (op (first xs) (foldr op z (rest xs)))]))

;left->right
(define (foldl op z xs)
    (cond
      [(empty? xs) z]
      [(foldl op (op z (first xs)) (rest xs))]))

(define (join connect xs)
  (foldl (lambda (first second) (string-append (~a first) connect (~a second))) (first xs) (rest xs)))

(define (reject p xs)
  (define (iter result rest-xs)
    (cond
      [(empty? rest-xs) result]
      [(p (first rest-xs)) (iter result (rest rest-xs))]
      [else (cons (first rest-xs) (iter result (rest rest-xs)))]
      ))
  (iter '() xs))
; може чрез филтър

(define (all? p xs)
  (foldr (lambda (x y) (and (p x) y)) #t xs))

(define (any? p xs)
  (foldr (lambda (x y) (or (p x) y)) #f xs))

(define (count element xs)
  (foldr (lambda (x y) (if (equal? x element) (+ 1 y)
                           y)) 0 xs))
(define (zip list1 list2)
  (define (iter result rest-list1 rest-list2)
    (cond
      [(or (empty? rest-list1) (empty? rest-list2)) result]
      [else (cons (list (first rest-list1) (first rest-list2)) (iter result (rest rest-list1) (rest rest-list2)))]))
  (iter (list) list1 list2))

(define (zip2 xs ys)
  (map cons xs ys)) ; is just like zipo
(define (trim string)
  (define (drop-first list-string)
    (cond
      [(empty? list-string) '()]
      [(equal? #\space (first list-string)) (drop-first (rest list-string))]
      [else list-string]))
 
  (list->string (reverse (drop-first (reverse (drop-first (string->list string)))))))


(define (group xs)
  )
  