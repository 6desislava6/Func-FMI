http://docs.racket-lang.org/quick/
1.
(define (product_digits n)
    (cond
    [(= n 0) 1]
    [else (*(product_digits(quotient n 10)) (remainder n 10))]
    )
)
(product_digits 123)

2.
(point-x - circle-x)^2 + (point-y - circle-y)^2 <= radius^2
(define (circle? circle-x circle-y radius point-x point-y)
    (<= (+(* (- point-x  circle-x) (- point-x circle-x)) (* (- point-y circle-y) (- point-y circle-y))) (* radius radius))
)
3.
(define (area a b c)
    (* (/ 1 4) (sqrt (* (* (* (+ (+ a b) c) (- (+ a b) c)) (+ (- a b) c)) (+ (- b a) c))))
)

4.
(define (prime_inside n max current)
    (cond
    [(>= current max) (not (= (remainder n current) 0))]
    [else (and (not (= (remainder n current) 0)) (prime_inside n max (+ current 1)))]
    )
)

(define (prime? n)
    (prime_inside n (+ (sqrt n) 1) 2)
)
(prime? 1609 )
(prime? 6)


5.
(define (cube_sums_inside num a)
    (cond
    [(<= a 0) #f]
    [(and (integer? (expt (- num (expt a 3)) (/ 1 3))) (> (expt (- num (expt a 3)) (/ 1 3)) 0)) #t]
    [else (cube_sums_inside num (- a 1))]
    )
)
(define (cube-sums? n)
    (cube_sums_inside n (round (expt n (/ 1 3))))
)

(define (count to current total)
    (cond
    [(< to current) total]
    [(cube-sums? current) (count to (+ 1 current) (+ 1 total))]
    [else (count to (+ 1 current) total)]
    )

)

(define (count-cube-sums from to)
    (count to from 0)
)
(count-cube-sums 1 9)
(cube-sums? 8)
