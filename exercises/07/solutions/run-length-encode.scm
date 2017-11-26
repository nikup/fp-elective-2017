#lang racket
(require rackunit rackunit/text-ui)

(define (run-length-encode list)
  (define (helper new old count prev)
    (if (null? old)
        (cons (cons prev count) new)
        (if (= (car old) prev)
            (helper new (cdr old) (+ count 1) prev)
            (helper (cons (cons prev count) new) (cdr old) 1 (car old)))))

  (if (null? list)
      '()
      (reverse (helper '() (cdr list) 1 (car list)))))

(define run-length-encode-tests
  (test-suite
   "Test for matrix run-length-encode"

   (check-equal? (run-length-encode '(8 7 7 2 2 2 2 3 3 4))
                 '((8 . 1) (7 . 2) (2 . 4) (3 . 2) (4 . 1)))

   ))

(run-tests run-length-encode-tests)