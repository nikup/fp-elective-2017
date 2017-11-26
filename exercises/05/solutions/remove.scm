#lang racket
(require rackunit rackunit/text-ui)

(define (reverse list)
  (define (helper new old)
    (if (null? old)
        new
        (helper (cons (car old) new) (cdr old))))
  (helper '() list))

(define (remove list number)
  (define (helper new old)
    (if (= (car old) number)
        (cons new (cdr old))
        (helper (cons (car old) new) (cdr old))))

  (helper '() list))

(define remove-tests
  (test-suite
    "Tests for remove"

    (check-equal? (remove '(42) 42) '())
    (check-equal? (remove '(5 3 5 5) 5) '(3 5 5))
    (check-equal? (remove '(8 4 92 82 8 13) 82) '(8 4 92 8 13))
    (check-equal? (remove '(8 4 82 12 31 133) 133) '(8 4 82 12 31))))

(run-tests remove-tests)
