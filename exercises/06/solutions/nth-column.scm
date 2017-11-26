#lang racket
(require rackunit rackunit/text-ui)

(define (nth-element list n)
  (define (helper l count)
    (if (= count n)
        (car l)
        (helper (cdr l) (+ count 1))))

  (helper list 1))

(define (nth-column2 list n)
  (define (helper new old)
    (if (null? old)
        new
        (helper (cons (nth-element (car old) n) new) (cdr old))))

  (reverse (helper '() list)))

(define (nth-column list n)
  (map (lambda (l) (nth-element l n)) list))

(define nth-column-tests
  (test-suite
   "Test for matrix nth-column"

   (check-equal? (nth-column '((1 2 3) (4 5 6) (7 8 9)) 2) '(2 5 8))
   (check-equal? (nth-column '((1 2 3) (4 5 6)) 1) '(1 4))
   (check-equal? (nth-column '((1 2) (4 5)) 2) '(2 5))
   (check-equal? (nth-column '((4)) 1) '(4))
   (check-equal? (nth-column '((1 2 3) (4 5 6) (7 8 9)) 3) '(3 6 9))

   ))

(run-tests nth-column-tests)