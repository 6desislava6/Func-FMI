#lang racket

; Намира сумата на всички числа в numbers
; -> (sum (list))
; 0
; -> (sum (list 1 2 3))
; 6
(define (sum numbers)
  (define (summer result rest-numbers)
    (if (empty? rest-numbers) result
        (summer (+ result (first rest-numbers)) (rest rest-numbers))))
  (summer 0 numbers))

; Проверява дали x се среща в items
; -> (member? 1 (list 1 2 3))
; #t
; -> (member? "asdf" (list "asd"))
; #f
; Разгледайте http://docs.racket-lang.org/reference/booleans.html
(define (member? element given-list)
  (cond
    [(empty? given-list) #f]
    [(equal? element (first given-list)) #t ]
    [else (member? element (rest given-list))])
;[else (or (equal? element (first given-list)) (member? element (rest given-list))] 
  )

; -> (length2 (range2 1 10))
; 9
; В Racket има такава функция, наречена length
(define (length2 numbers)
  (define (iter numbers r)
    (cond
      [(empty? numbers) r]
      [else (iter (rest numbers) (+ r 1))]))
  (iter numbers 0)
)

; Връща n-тия елемент от items при 0лево базиран индекс
; -> (list-ref2 (list 1 2 3) 0)
; 1
; В Racket има такава функция, наречена list-ref
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

; -> (range2 1 10)
; '(1 2 3 4 5 6 7 8 9)
; В Racket съществува такава функция, наречена range
(define (range2 a b)
  (cond
    [(>= a b) (list)]
    [else (cons a (range2 (add1 a) b))]
    )
 )
; Строи списък от числата между 0 и n, включително, като прилага f върху всяко число
; i-тия елемент на този списък е (f i)
; -> (build-list2 3 id)
; '(0 1 2)
; -> (build-list2 3 (lambda (x) (* x x)))
; '(0 1 4)
; В Racket има такава функция, наречена build-list
 (define (map2 f xs)
   (cond
     [(empty? xs) (list)]
     [else (cons (f (first xs)) (map2 f (rest xs)))]
   )
 )

(define (build-list2 n f)
  (map2 f (range2 0 n))
 )
; Конкатенира два списъка в нов списък
; -> (append2 (list 1 2 3) (list 4 5 6))
; '(1 2 3 4 5 6)
; В Racket има такава фунцкия, наречена append
(define (append2 l1 l2)
  (define (iter l1 l2)
     (if (empty? l1) l2
         (cons (first l1) (iter (rest l1) l2))
         )
    )
  (iter l1 l2)
  )

; Обръща списъка наобратно
; -> (reverse2 (list 1 2 3))
; '(3 2 1)
; В Racket има такава функция, наречена reverse
(define (reverse2 items)
  (define (iter result list-rest)
    (if (empty? list-rest) result
        (iter (cons (first list-rest) result) (rest list-rest)))
    )
  (iter (list) items)
  )

; Взима първите n елемента от даден списък
; Ако (> n (length items)), тогава връща items
; -> (take2 3 (list 1 2 3 4 5))
; '(1 2 3)
(define (take2 n items)
  (define (iter result index items-rest)
    (cond
      [(empty? items-rest) (list)]
      [(= index n) result]
      [else (cons (first items-rest) (iter result (+ 1 index) (rest items-rest)))]
      ))
  (iter (list) 0 items))

; Маха първите n елемента от даден списък
; Ако (> n (length items)) връща '()
; -> (drop2 3 (list 1 2 3 4 5))
; '(4 5)
(define (drop2 n items)
  (define (iter index rest-items)
    (cond
      [(empty? rest-items) '()]
      [(= index n) rest-items]
      [else (iter (+ 1 index) (rest rest-items))]
    ))
  (iter 0 items))

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина
; -> (take-while zero? (list 0 0 0 1 2 3))
; '(0 0 0)
; -> (take-while even? (list 2 4 5 7 8 3 2))
; '(2 4)
; -> (take-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '()
(define (take-while p items)
  (define (iter result rest-items)
    (cond
      [(empty? rest-items) result]
      [(not (p (first rest-items))) result]
      [else (cons (first rest-items) (iter result (rest rest-items)))]
      ))
  (iter (list) items))

; Функция от по-висок ред. Маха поредните елементи от items докато предикатa p дава лъжа за тях
; -> (drop-while zero? (list 0 0 0 1 2 3))
; '(1 2 3)
; -> (drop-while even? (list 2 4 5 7 8 3 2))
; '(5 7 8 3 2)
; -> (drop-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '(1 1 1 1 1)
(define (drop-while p items)
  (define (iter rest-items)
     (cond
      [(empty? rest-items) rest-items]
      [(not (p (first rest-items))) rest-items]
      [else (drop-while p (rest rest-items))])
    )
  (iter items))

; Функцията взима число и връща списък от цифрите му
; -> (number->list 123)
; '(1 2 3)
(define (number->list number)
  (define (iter number result-list)
    (cond
      [(= number 0) result-list]
      [else (iter (quotient number 10) (cons (remainder number 10) result-list))]
      ))
  (iter number (list))
 )

; Функцията взима списък от цифри и връща числото
; -> (list->number (list 1 2 3))
; 123
(define (list->number ns)
  (define (iter power result rest-ns)
    (if (empty? rest-ns) result
        (iter (- power 1) (+ result (* (expt 10 power) (first rest-ns))) (rest rest-ns))))
  (iter (- (length ns) 1) 0 ns))
