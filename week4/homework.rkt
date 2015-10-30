#lang racket
(define (f p g h)
  (lambda (x) ( if (and (p (g x)) (p (h x))) #t
                   #f))
  )

(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (simplify-fraction pair)
  (let* 
  ([divisor (gcd (fst pair) (snd pair))])
   (cons (/ (fst pair) divisor) (/ (snd pair) divisor))
   )  
 )
; I don't see the point of lambdas here
; but since this is the topic of the homework :D
(define (make-operation)
  (lambda (frac1 frac2 f) (simplify-fraction (cons (f (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))
  )
 )
(define (add-frac frac1 frac2)
  ((make-operation) frac1 frac2 +)
  )

(define (substract-frac frac1 frac2)
   ((make-operation) frac1 frac2 -))

(define (mult-frac frac1 frac2)
  (simplify-fraction (cons (* (fst frac1) (fst frac1)) (* (snd frac1) (snd frac2))))
  )