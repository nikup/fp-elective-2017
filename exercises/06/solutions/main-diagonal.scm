#lang racket
(require rackunit rackunit/text-ui)

(define (nth-element list n)
  (define (helper l count)
    (if (= count n)
        (car l)
        (helper (cdr l) (+ count 1))))

  (helper list 1))

(define (main-diagonal list)
  (define (helper new old count)
    (if (null? old)
        new
        (helper (cons (nth-element (car old) count) new) (cdr old) (+ count 1))))

  (reverse (helper '() list 1)))

(define main-diagonal-tests
  (test-suite
   "Test for matrix main-diagonal"

   (check-equal? (main-diagonal '((1 2 3) (4 5 6) (7 8 9))) '(1 5 9))
   (check-equal? (main-diagonal '((2 3) (5 6))) '(2 6))
   (check-equal? (main-diagonal '((1 2) (4 5))) '(1 5))
   (check-equal? (main-diagonal '((4))) '(4))
   (check-equal? (main-diagonal '((1 2 3 3) (4 5 6 6) (7 8 9 9) (7 8 9 9))) '(1 5 9 9))

   ))

(run-tests main-diagonal-tests)