#lang slideshow


(define (print-row list)
  (cond ((null? list) (display "\n"))
        ((= (car list) 1) (display "#")(print-row (cdr list)))
        ((= (car list) 0) (display "_")(print-row (cdr list)))
  ))

;matches a new cell state to old pattern
(define (get-new-cell old-pattern)
  (cond ((null? old-pattern) null)
        ;;((null? (get-2nd-cell old-pattern)) 0)
        ((null? (get-3rd-cell old-pattern)) 0)
        ((equal? (take old-pattern 3) '(1 1 1)) 0)
        ((equal? (take old-pattern 3) '(1 0 0)) 0)
        ((equal? (take old-pattern 3) '(0 0 0)) 0)
        ( true 1)))

;deprecated => delete
;return 2nd cell in a list, or null.
;(define (get-2nd-cell list)
;  (cond ((null? list) null)
;        ((false? (list? list)) null)
;        ((null? (cdr list)) null)
;        (true (car (cdr list)))))
;  (car (cdr list)))

;return 3rd cell in a list, or null.
(define (get-3rd-cell list)
  (cond ((null? list) null)
        ((false? (list? list)) null)
        ((null? (cdr list)) null)
        ((false? (list? (cdr list))) null)
        ((null? (cdr (cdr list))) null)
        (true (car (cdr (cdr list))))))

(define (generate-row-rec list);maybe useless, use map?
  (print list)
  (display "\n")
  ;(get-new-cell list)
;  (generate-row-rec (cdr))
  (cond ((false?(null? (get-3rd-cell list))) (append (get-new-cell list) (generate-row-rec (cdr list))))
  (true (list 0 0))))
 ; (cond ((null? (get-3rd-cell list)) 0);last zero  ;use false instead
  ;      (true (cons (get-new-cell list) (generate-row-rec (cdr list))))))


(define (generate-row list)
  (println list)
  (cons 0 (generate-row-rec list)))
  ;(cons 0 (map(lambda (w) (get-new-cell w)) list)));wrong length

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

(println (generate-row '(0 0 0 1 0 0 0 0 0 0)))
(println "======================")
(println (generate-row(generate-row '(0 0 0 1 0 0 0 0 0 0))))
;(print (get-new-cell '(0 1 0 1 0 1 0 1 0)))
;(print (get-new-cell '(0 0)))