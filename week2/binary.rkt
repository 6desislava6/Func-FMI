#lang racket
;Converts each v to a string in display mode—that is
;like (format "~a" v)—then concatentates;
;the results with separator between consecutive items, and then pads
;or truncates the string to be at least min-width characters and at most max-width characters.

;> (~a "abcde" #:max-width 4)
;"abcd"
; formats the string
;> (~a "abcde" #:max-width 4 #:limit-marker "*")
;"abc*"

;> (~a "orange" #:min-width 20 #:align 'center
;                #:left-pad-string "- " #:right-pad-string " -")
;"- - - -orange- - - -"


;-> (string->number "111")
;111

(provide
  string-reverse)
(provide to-binary-string)
(define (string-reverse str)
  (define (string-inside counter reversed)
    (if (= 0 counter) (string-append reversed (~a (string-ref str 0)))
        (string-inside (- counter 1) (string-append reversed (~a (string-ref str counter))))
    )
   )
  (string-inside (- (string-length str) 1) "")
)


(define (to-binary-string n)
  (define (division-two result current)
    (if (= current 0) result
        (division-two (string-append result (~a (remainder current 2))) (quotient current 2)))
    )
  (string-reverse (division-two "" n))
)

(define (from-binary-string binary-str)
  (define (powers-inside length counter result)
    (if (= counter length) result
        (powers-inside length (+ counter 1) (+ result (* (string->number (~a (string-ref binary-str (- (- length counter) 1)))) (expt 2 counter))))))
  (powers-inside (string-length binary-str) 0 0)
 )

