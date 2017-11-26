#lang racket
(require rackunit rackunit/text-ui)

(define (maximum list)
  (define (helper max l)
    (if (null? l)
        max
        (helper
         (if (> max (car l)) max (car l))
         (cdr l))))
  (helper 0 list))

(define maximum-tests
  (test-suite
    "Tests for maximum"

    (check = (maximum '(2)) 2)
    (check = (maximum '(5 3 5 5)) 5)
    (check = (maximum '(8 4 92 82 8 13)) 92)
    (check = (maximum '(8 4 82 12 31 133)) 133)))

(run-tests maximum-tests)
