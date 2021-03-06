#lang racket
(require rackunit rackunit/text-ui)

(define (sum-digits n)
  (define (helper n result)
    (if (> 10 n)
        (+ result n)
        (helper (quotient n 10) (+ result (remainder n 10)))))

  (helper n 0))

(define sum-digits-tests
  (test-suite
   "Tests for sum-digits"

   (check = (sum-digits 3) 3)
   (check = (sum-digits 12) 3)
   (check = (sum-digits 42) 6)
   (check = (sum-digits 666) 18)
   (check = (sum-digits 1337) 14)
   (check = (sum-digits 65510) 17)
   (check = (sum-digits 8833443388) 52)
   (check = (sum-digits 100000000000) 1)))

(run-tests sum-digits-tests)
