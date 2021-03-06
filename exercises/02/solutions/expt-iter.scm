#lang racket
(require rackunit rackunit/text-ui)

(define (expt-iter x n)
  (define (expt-iter-helper product counter)
    (if (> counter n)
        product
        (expt-iter-helper (* x product) (+ counter 1))))

  (expt-iter-helper 1 1))

(define expt-iter-tests
  (test-suite
   "Tests for expt-iter"

   (check = (expt-iter 2 0) 1)
   (check = (expt-iter 2 1) 2)
   (check = (expt-iter 2 2) 4)
   (check = (expt-iter 3 2) 9)
   (check = (expt-iter 5 3) 125)
   (check = (expt-iter 2 10) 1024)
   (check = (expt-iter -2 10) 1024)
   (check = (expt-iter -2 11) -2048)))

(run-tests expt-iter-tests)
