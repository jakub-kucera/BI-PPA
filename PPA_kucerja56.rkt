#lang slideshow
;prints formatted line
(define (print-row list)
  (cond ((null? list) (display "\n"))
        ((= (car list) 1) (display "#")(print-row (cdr list)))
        ((= (car list) 0) (display "-")(print-row (cdr list)))))

;matches a new cell state to old pattern
(define (get-new-cell old-pattern)
  (cond ((null? old-pattern) null)
        ((null? (get-3rd-cell old-pattern)) 0)
        ((equal? (take old-pattern 3) '(1 1 1)) 0)
        ((equal? (take old-pattern 3) '(1 0 0)) 0)
        ((equal? (take old-pattern 3) '(0 0 0)) 0)
        ( true 1)))

;checks whether this cell can be transformed
(define (get-3rd-cell list)
  (cond ((null? list) null)
        ((false? (list? list)) null)
        ((null? (cdr list)) null)
        ((false? (list? (cdr list))) null)
        ((null? (cdr (cdr list))) null)
        (true (car (cdr (cdr list))))))

;recursive function for transforming a list using the rule 110
(define (generate-row-rec list)
  ;(print list)
  ;(display "\n")
  (cond ((false?(null? (get-3rd-cell list))) (cons (get-new-cell list) (generate-row-rec (cdr list))))
        (true (cons 0 '()))))

;generates single rule 110 transformation
(define (generate-row list)
  (println list)
  (cons 0 (generate-row-rec list)))

;main function that generates a given number of sequential rule 110 transformations
(define (generate-110 list rows)
  ;if length(list) < 3 return null
  (cond ((> rows 0)
         (let((new-row (generate-row list)))
         (print-row new-row)
         (generate-110 new-row (- rows 1))))))
  

;(print-row '(1 0 1 0 1 1 1 1 1 1))
;(get-new-cell '(0 1 0))
;(get-2nd-cell '(1 2 3))
;(get-3rd-cell '(1 2 3 4 5))

(generate-110 '(1 0 1 0 1 1 1 1 1 1) 10)

;(println (generate-row '(0 0 0 1 0 0 0 0 0 0)))
;(println "======================")
;(println (generate-row(generate-row '(0 0 0 1 0 0 0 0 0 0))))
;(println "======================")
;(print-row (generate-row '(0 0 0 1 0 0 0 0 0 0)))
;(print (get-new-cell '(0 1 0 1 0 1 0 1 0)))
;(print (get-new-cell '(0 0)))