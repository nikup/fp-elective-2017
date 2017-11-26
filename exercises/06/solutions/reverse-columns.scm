#lang racket
(require rackunit rackunit/text-ui)

(define (reverse list)
  (define (helper new old)
    (if (null? old)
        new
        (helper (cons (car old) new) (cdr old))))
  (helper '() list))

(define (reverse-columns list)
  (define (helper new old)
    (if (null? old)
        new
        (helper (cons (reverse (car old)) new) (cdr old))))

  (reverse (helper '() list)))

(define reverse-columns-tests
  (test-suite
   "Test for matrix reverse-columns"

   (check-equal? (reverse-columns '((1 2 3) (4 5 6))) '((3 2 1) (6 5 4)))
   (check-equal? (reverse-columns '((1 2) (4 5))) '((2 1) (5 4)))
   (check-equal? (reverse-columns '()) '())
   (check-equal? (reverse-columns '((4))) '((4)))
   (check-equal? (reverse-columns '((1 2 3) (4 5 6) (7 8 9))) '((3 2 1) (6 5 4) (9 8 7)))

   ))

(run-tests reverse-columns-tests)