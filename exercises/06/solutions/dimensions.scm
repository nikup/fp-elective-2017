#lang racket
(require rackunit rackunit/text-ui)

(define (lenght list)
  (define (helper count l)
    (if (null? l)
        count
        (helper
         (+ count 1)
         (cdr l))))
  (helper 0 list))

(define (dimensions list)
 (if (null? list)
     (cons 0 0)
     (cons (lenght list) (lenght (car list)))))

(define dimensions-tests
  (test-suite
   "Test for matrix dimensions"

   (check-equal? (dimensions '((1 2 3) (4 5 6))) '(2 . 3))
   (check-equal? (dimensions '((1 2) (4 5))) '(2 . 2))
   (check-equal? (dimensions '()) '(0 . 0))
   (check-equal? (dimensions '((1 2 3) (4 5 6) (4 5 6))) '(3 . 3))

   ))

(run-tests dimensions-tests)