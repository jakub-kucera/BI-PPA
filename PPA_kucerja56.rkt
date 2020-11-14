#lang slideshow


(define (print-row list)
  (cond ((null? list) (display "\n"))
        ((= (car list) 1) (display "#")(print-row (cdr list)))
        ((= (car list) 0) (display "_")(print-row (cdr list)))
  ))

;matches a new cell state to old pattern
(define (get-new-cell old-pattern)
  (cond ((null? old-pattern) null)
        ((equal? old-pattern '(1 1 1)) 0)
        ((equal? old-pattern '(1 0 0)) 0)
        ((equal? old-pattern '(0 0 0)) 0)
        ( true 1)))

(define (get-2nd-cell list)
  (cond ((null? list) null)
        ((null? (cdr list)) null)
        (true (car (cdr list)))))
  ;(car (cdr list)))

(define (get-3rd-cell list)
  (cond ((null? list) null)
        ((null? (cdr list)) null)
        ((null? (cdr (cdr list))) null)
        (true (car (cdr (cdr list))))))

(define (generate-row-rec list)
  (cond ((null? (get-3rd-cell list)) 0 display "2220"));nothing
  (cond (true (cons (car list) (generate-row-rec (cdr list))))))

(define (generate-row list)
  (cons 0 (generate-row-rec list)))

;first 0
(define (generate-110 list rows)
  ;if length(list) < 3 return null
  (cond ((> rows 0)
         (print-row list)
         (generate-110 list (- rows 1)))))
  ;(if (> rows 0)
 ;     ((print-row list)
 ;     (generate-110 list (- rows 1)))
;  false))
  

;(print-row '(1 0 1 0 1 1 1 1 1 1))
;(get-new-cell '(0 1 0))
;(get-2nd-cell '(1 2 3))
;(get-3rd-cell '(1 2 3 4 5))

;(generate-110 '(1 0 1 0 1 1 1 1 1 1) 0)

(generate-row '(1 0 1 0 1 1 1 1 1 1))