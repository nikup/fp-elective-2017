#lang racket
(require rackunit rackunit/text-ui)

(define (add list el n)
  (if (= n 0)
      list
      (add (cons el list) el (- n 1))))

(define (run-length-decode list)
  (define (helper new old)
    (if (null? old)
        new
        (helper (add new (caar old) (cdar old)) (cdr old))))

      (reverse (helper '() list)))

(define run-length-decode-tests
  (test-suite
   "Test for matrix run-length-decode"

   (check-equal? (run-length-decode '((8 . 1) (7 . 2) (2 . 4) (3 . 2) (4 . 1)))
                 '(8 7 7 2 2 2 2 3 3 4))
   (check-equal? (run-length-decode '((1 . 2) (3 . 4) (5 . 2)))
                 '(1 1 3 3 3 3 5 5))

   ))

(run-tests run-length-decode-tests)