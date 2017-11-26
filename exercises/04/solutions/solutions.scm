#lang racket
(require rackunit rackunit/text-ui)

(define (lenght list)
  (define (helper count l)
    (if (null? l)
        count
        (helper
         (+ count 1)
         (cdr l))))
  (helper 0 list))

(define lenght-tests
  (test-suite
    "Tests for lenght"

    (check = (lenght '()) 0)
    (check = (lenght '(2)) 1)
    (check = (lenght '(5 3 5 5)) 4)
    (check = (lenght '(8 4 92 82 8 13)) 6)
    (check = (lenght '(8 4 82 12 31 133)) 6)))

(run-tests lenght-tests)


(define (last list)
  (define (helper l)
    (if (null? (cdr l))
        (car l)
        (helper (cdr l))))
  (helper list))

(define last-tests
  (test-suite
    "Tests for last element"

    (check = (last '(2)) 2)
    (check = (last '(5 3 5 5)) 5)
    (check = (last '(8 4 92 82 8 13)) 13)
    (check = (last '(8 4 82 12 31 133)) 133)))

(run-tests last-tests)

(define (reverse list)
  (define (helper new old)
    (if (null? old)
        new
        (helper (cons (car old) new) (cdr old))))
  (helper '() list))

(define reverse-tests
  (test-suite
    "Tests for reverse list"

    (check-equal? (reverse '(2)) '(2))
    (check-equal? (reverse '(5 3 5 5)) '(5 5 3 5))
    (check-equal? (reverse '(8 4 92 82 8 13)) '(13 8 82 92 4 8))
    (check-equal? (reverse '(8 4 82 12 31 133)) '(133 31 12 82 4 8))))

(run-tests reverse-tests)

(define (append l1 l2)
  (define (helper l1 l2)
    (if (null? l1)
        l2
        (helper (cdr l1) (cons (car l1) l2))))
  (helper (reverse l1) l2))

(define append-tests
  (test-suite
    "Tests for appending lists"

    (check-equal? (append '(2) '()) '(2))
    (check-equal? (append '(5 3 5 5) '(8 4 92 82 8 13)) '(5 3 5 5 8 4 92 82 8 13))
    (check-equal? (append '(8 4) '(92 82 8 13)) '(8 4 92 82 8 13))
    (check-equal? (append '() '(8 4 82 12 31 133)) '(8 4 82 12 31 133))))

(run-tests append-tests)

(define (map l f)
  (define (helper new old)
    (if (null? old)
        new
        (helper
         (cons (f (car old)) new)
         (cdr old))))
  (helper '() (reverse l)))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define map-tests
  (test-suite
   "Tests for map"

    (check-equal? (map '(2) identity) '(2))
    (check-equal? (map '(8 4 92 82 8 13) inc) '(9 5 93 83 9 14))
    (check-equal? (map '(92 82 8 13) inc) '(93 83 9 14))
    (check-equal? (map '(8 4 82 12 31 133) square) '(64 16 6724 144 961 17689))))

(run-tests map-tests)

(define (filter l f)
  (define (helper new old)
    (if (null? old)
        new
        (helper
         (if (f (car old))
             (cons (car old) new)
             new)
         (cdr old))))
  (helper '() (reverse l)))

(define filter-tests
  (test-suite
   "Tests for filter"

    (check-equal? (filter '(2) even?) '(2))
    (check-equal? (filter '(8 4 92 82 8 13) even?) '(8 4 92 82 8))
    (check-equal? (filter '(92 82 8 13) even?) '(92 82 8))
    (check-equal? (filter '(8 4 82 12 31 133) even?) '(8 4 82 12))))

(run-tests filter-tests)