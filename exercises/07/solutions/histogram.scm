#lang racket
(require rackunit rackunit/text-ui)

(define (partition f l)
  (define (helper old yes no)
    (if (null? old)
        (list (reverse yes) (reverse no))
        (helper (cdr old)
                (if (f (car old))
                    (cons (car old) yes)
                    yes)
                (if (not (f (car old)))
                    (cons (car old) no)
                    no))))

  (helper l '() '()))

(define (partition-histogram list)
  (define (helper part new)
    (if (null? (car part))
        new
        (helper (partition (lambda (el) (= el (car (cadr part)))) (cadr part))
                (cons (cons (car (car part)) (length (car part))) new))))

  (reverse (helper (partition (lambda (el) (= el (car list))) list) '())))

(define (histogram list)
  (define (helper new old)
    (if (null? old)
        new
        (helper (cons (cons (car old) (length (filter (lambda (el) (= el (car old))) old))) new)
                (filter (lambda (el) (not (= el (car old)))) old))))
  (reverse (helper '() list)))

(define histogram-tests
  (test-suite
   "Test for matrix histogram"

   (check-equal? (partition-histogram '(8 7 1 7 8 2 2 8 2 7 8 1))
                 '((8 . 4) (7 . 3) (1 . 2) (2 . 3)))

   ))

(run-tests histogram-tests)