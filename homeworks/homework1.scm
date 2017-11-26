#lang racket

(define (argcomp f arg comp)
  (define (helper max rest)
    (if (null? rest)
        max
        (helper
         (if (comp (f (car rest)) (f max))
             (car rest)
             max)
         (cdr rest))))

  (if (null? arg)
      "special value"
      (helper (car arg) (cdr arg))))

(define (argmax f arg)
  (argcomp f arg >))

(define (argmin f arg)
  (argcomp f arg <))

(define (split n)
  (define (helper number list)
    (if (= 0 number)
        list
        (helper (quotient number 10) (cons (modulo number 10) list))))

  (helper n '()))

(define (concat l)
  (define (helper n rest)
    (if (null? rest)
        n
        (helper (+ (* n 10) (car rest)) (cdr rest))))

  (helper 0 l))

(define (remove l n)
  (define (helper new old)
    (if (= n (car old))
        (append (reverse new) (cdr old))
        (helper (cons (car old) new) (cdr old))))

  (helper '() l))

(define (max n)
  (argmax identity (split n)))

(define (removemax n)
  (concat (remove (split n) (max n))))

(define (reduce n)
  (if (< n 10)
      n
      (reduce (* (max n) (removemax n)))))

(define (approx-zero? x)
    (< (abs x) 0.0001))

(define (find-root f a b eps)
  (define (helper ai bi i)
    (let ([mid (exact->inexact (/ (+ ai bi) 2))])
      (let ([fm (f mid)])
        (if (or (approx-zero? fm) (< (abs (- ai bi)) eps))
            (cons mid i)
            (if (equal? (< fm 0) (< (f ai) 0))
                (helper mid bi (+ i 1))
                (helper ai mid (+ i 1)))))))

  (helper a b 0))

(define (find-root-secants f a b eps)
  (define (helper x y i)
    (if (< (abs (- x y)) eps)
        (cons (exact->inexact y) i)
        (helper y (- y (/ (* (- y x) (f y)) (- (f y) (f x)))) (+ i 1))))

  (helper a b 0))

(define (compare-methods f a b eps)
  (list
   (cons "iterative" (cdr (find-root f a b eps)))
   (cons "secants" (cdr (find-root-secants f a b eps)))))