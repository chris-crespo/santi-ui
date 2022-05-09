#lang racket 

(require rackunit)
(require "main.rkt")

(define (zip l1 l2)
  (map cons l1 l2))

(define-syntax check-equal-values?
  (syntax-rules ()
    ((_ generator expected-values ...)
     (call-with-values
       (lambda () generator)
       (lambda given-values
         (for-each (lambda (pair) 
                     (check-equal? (car pair) (cdr pair)))
                   (zip given-values (list expected-values ...))))))))

(make-test-suite 
  "divide"
  (test-begin
    (test-case 
      "Single element"
      (check-equal-values? 
        (divide (list 7)) (list 7) (list)))  
    (test-case 
      "First element larger than half total"
      (check-equal-values? 
        (divide (list 9 3 2 1)) (list 8) (list 3 2 1)))))
