#lang racket
(require rackunit rackunit/text-ui)

(define (group-by f l)
  (define (helper new old)
    (if (null? old)
        new
        (helper (cons (cons (car old) (filter (lambda (el) (= (f el) (f (car old)))) old)) new)
                (filter (lambda (el) (not (= (f el) (f (car old))))) old))))
  (reverse (helper '() l)))

(define group-by-tests
  (test-suite
   "Test for matrix group-by"

   (check-equal? (group-by (lambda (x) (remainder x 3)) '(0 1 2 3 4 5 6 7 8))
                 '((0 0 3 6) (1 1 4 7) (2 2 5 8)))

   ))

(run-tests group-by-tests)