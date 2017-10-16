#lang racket
(require rackunit rackunit/text-ui)

(define (fast-expt-iter x n)
  (define (helper product x n)
    (if (= n 0)
        product
        (if (even? n)
            (helper product (* x x) (/ n 2))
            (helper (* product x) x (- n 1)))))
  (helper 1 x n))

(define fast-expt-iter-tests
  (test-suite
   "Tests for fast-expt-iter"

   (check = (fast-expt-iter 2 0) 1)
   (check = (fast-expt-iter 2 1) 2)
   (check = (fast-expt-iter 2 2) 4)
   (check = (fast-expt-iter 3 2) 9)
   (check = (fast-expt-iter 5 3) 125)
   (check = (fast-expt-iter 2 10) 1024)
   (check = (fast-expt-iter -2 10) 1024)
   (check = (fast-expt-iter -2 11) -2048)))

(run-tests fast-expt-iter-tests)
