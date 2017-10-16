#lang racket
(require rackunit rackunit/text-ui)

(define (count-divisors n)
  (define (helper i count)
    (if (> i n)
        count
        (helper (+ i 1) (+ count (if (= 0 (remainder n i)) 1 0)))))

  (helper 1 0))

(define (prime? n)
  (= (count-divisors n) 2))

(define prime?-tests
  (test-suite
   "Primality tests"

   (check-true (prime? 3))
   (check-true (prime? 19))
   (check-true (prime? 599))
   (check-true (prime? 661))
   (check-true (prime? 2221))
   (check-true (prime? 7879))

   (check-false (prime? 12))
   (check-false (prime? 15))
   (check-false (prime? 42))
   (check-false (prime? 666))
   (check-false (prime? 1337))
   (check-false (prime? 65515))
   (check-false (prime? 1234567))))

(run-tests prime?-tests)
