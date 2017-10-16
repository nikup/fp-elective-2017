#lang racket
(require rackunit rackunit/text-ui)

(define (fast-expt a b)
  (cond ((= b 0) 1)
      ((= b 1) a)
      ((even? b) (* (fast-expt a (/ b 2)) (fast-expt a (/ b 2))))
      (else (* a (fast-expt a (- b 1))))))

(define fast-expt-tests
  (test-suite
   "Tests for fast-expt"

   (check = (fast-expt 2 0) 1)
   (check = (fast-expt 2 1) 2)
   (check = (fast-expt 2 2) 4)
   (check = (fast-expt 3 2) 9)
   (check = (fast-expt 5 3) 125)
   (check = (fast-expt 2 10) 1024)
   (check = (fast-expt -2 10) 1024)
   (check = (fast-expt -2 11) -2048)))

(run-tests fast-expt-tests)
