#lang slideshow
;prints formatted line
(define (print-row list)
  (cond ((null? list) (display "\n"))
        ((= (car list) 1) (display "#")(print-row (cdr list)))
        ((= (car list) 0) (display "-")(print-row (cdr list)))))

;matches a new cell state to old pattern
(define (get-new-cell old-list)
  (let((pattern (take old-list 3)))
  (cond ((null? old-list) null)
        ((equal? pattern '(1 1 1)) 0)
        ((equal? pattern '(1 0 0)) 0)
        ((equal? pattern '(0 0 0)) 0)
        ( true 1))))

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
  (cond ((false?(null? (get-3rd-cell list))) (cons (get-new-cell list) (generate-row-rec (cdr list))))
        (true (cons 0 '()))))

;generates single rule 110 transformation
(define (generate-row list)
  (cons 0 (generate-row-rec list)))

;main function that generates a given number of sequential rule 110 transformations
(define (generate-110 list rows)
  (print-row list)
  (cond ((< (length list) 3) (display "Invalid input."))
        ((> rows 0)
         (let((new-row (generate-row list)))
         (generate-110 new-row (- rows 1))))))


;#-#-#####-
;-####---#-
;-#--#--##-
;-#-##-###-
;-######-#-
;-#----###-
;-#---##-#-
;-#--#####-
;-#-##---#-
;-#--#-#---
;(generate-110 '(1 0 1 0 1 1 1 1 1 0) 10)

;(generate-110 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 0 1 1 1 1 1 0 0 0 ) 100)
;(generate-110 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1) 100)
;(generate-110 '(1 0 1 0 1 1 1 1 1 1) 10)
;(generate-110 '(0 1) 10)
;(generate-110 '() 10)
