#lang racket
(require rackunit rackunit/text-ui)


(define (transpose list)
  
)

(define transpose-tests
  (test-suite
   "Test for matrix transpose"

   (check-equal? (transpose '((1 2 3) (4 5 6))) '((1 4) (2 5) (3 6)))
   (check-equal? (transpose '((1 2 3) (4 5 6) (7 8 9))) '((1 4 7) (2 5 8) (3 6 9)))
   (check-equal? (transpose '((2 3) (5 6))) '((2 5) (3 6)))
   (check-equal? (transpose '((1 2) (4 5))) '((1 4) (2 5)))
   (check-equal? (transpose '((4))) '(4))
   (check-equal? (transpose '((1 2 3 3) (4 5 6 6) (7 8 9 9) (7 8 9 9)))
                 '((1 4 7 7) (2 5 8 8) (3 6 9 9) (3 6 9 9)))
   ))

(run-tests transpose-tests)