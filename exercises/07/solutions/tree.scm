#lang racket

;конструира двоично дърво по корен, ляво и дясно поддърво.
(define (make-tree root left right)
  (list root left right))

;връща корена на двоичното дърво tree.
(define (root-tree tree)
  (car tree))

;връща лявото поддърво на двоичното дърво tree.
(define (left-tree tree)
  (car (cdr tree)))

;връща дясното поддърво на двоичното дърво tree.
(define (right-tree tree)
  (car (cdr (cdr tree))))

;проверява дали двоичното дърво tree е празно.
(define (empty-tree? tree)
  (and (null? (root-tree tree))
       (null? (left-tree tree))
       (null? (right-tree tree))))

;проверява дали tree е листо.
(define (leaf-tree? tree)
  (and (not (null? (root-tree tree)))
       (null? (left-tree tree))
       (null? (right-tree tree))))

;проверява дали tree е двоично дърво.
;what?
(define (tree? tree)
  tree)

(define (height tree)
    (if (null? tree)
        0
        (+ 1 (max (height (left-tree tree)) (height (right-tree tree))))))

(height '(1 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(define (sum-tree tree)
  (if (null? tree)
      0
      (+ (root-tree tree) (sum-tree (left-tree tree)) (sum-tree (right-tree tree)))))

(sum-tree '(1 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(define (max-tree tree)
  (if (null? tree)
      0
      (max (root-tree tree) (max-tree (left-tree tree)) (max-tree (right-tree tree)))))

(max-tree '(1 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(define (invert tree)
  (if (null? tree)
      '()
      (list (root-tree tree) (invert (right-tree tree)) (invert (left-tree tree)))))

(invert '(1 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(define (binary-heap? tree)
  (if (null? tree)
      #t
      (and (or (null? (left-tree tree)) (< (root-tree tree) (root-tree (left-tree tree))))
           (or (null? (right-tree tree)) (< (root-tree tree) (root-tree (right-tree tree))))
           (binary-heap? (left-tree tree))
           (binary-heap? (right-tree tree)))))

(display "binary-heap?")
(binary-heap? '(1 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(display "binary-heap?")
(binary-heap? '(7 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(define (balanced? tree)
  (if (null? tree)
      #t
      (and (< (abs (- (height (left-tree tree)) (height (right-tree tree)))) 2)
           (balanced? (left-tree tree))
           (balanced? (right-tree tree)))))

(display "balanced?")
(balanced? '(1 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(display "balanced?")
(balanced? '(1 (2 (6 () ()) ())
            (3 () (9 () ()))))


(define (binary-search-tree? tree)
  (if (null? tree)
      #t
      (and (or (null? (left-tree tree)) (> (root-tree tree) (root-tree (left-tree tree))))
           (or (null? (right-tree tree)) (< (root-tree tree) (root-tree (right-tree tree))))
           (binary-search-tree? (left-tree tree))
           (binary-search-tree? (right-tree tree)))))

(display "binary-search-tree?")
(binary-search-tree? '(1 (2 () ())
            (3 (4 (6 () ()) ())
               (5 () ()))))

(display "binary-search-tree?")
(binary-search-tree? '(2 (1 () ())
            (5 (4 (3 () ()) ())
               (7 () ()))))

(define (binary-search-tree-insert tree v)
  (if (null? tree)
      (list v '() '())
      (if (> (root-tree tree) v)
          (list (root-tree tree) (binary-search-tree-insert (left-tree tree) v) (right-tree tree))
          (list (root-tree tree) (left-tree tree) (binary-search-tree-insert (right-tree tree) v))))
  )

(binary-search-tree-insert '(2 (1 () ())
            (5 (4 (3 () ()) ())
               (7 () ()))) 6)

(define (to-binary-search-tree l)
  (define (helper tree list)
    (if (null? list)
        tree
        (helper (binary-search-tree-insert tree (car list)) (cdr list))))
  (helper '() l))

(define (in-order tree)
    (if (null? tree)
        '()
        (append (in-order (left-tree tree)) (list (root-tree tree)) (in-order (right-tree tree)))))

(define (tree-sort l)
  (in-order (to-binary-search-tree l)))

(tree-sort '(1 6 3 4 7 5 2))

(define (mid l)
  (define (helper new old)
    (if (< (abs (- (length new) (length old))) 2)
        (list (reverse new) (car old) (cdr old))
        (helper (cons (car old) new) (cdr old))))
  (helper '() l))

(define (to-balanced-tree l)
  (if (null? l)
      '()
      (let ([sl (mid l)])
        (list (car (cdr sl)) (to-balanced-tree (car sl)) (to-balanced-tree (car (cdr (cdr sl))))))))

(to-balanced-tree '(1 2 3 4 5 6 7 8 9 19))