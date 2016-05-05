#lang racket
(require eopl/eopl)
(require racket/trace)

;; CS496
;; Homework 2
;; Alana L Seabra A Santos

; Question 4
(define-datatype dTree dTree?
  (leaf-t
   (datum number?))
  (node-t
   (symbol symbol?)
   (left dTree?)
   (right dTree?)))

; Question 5
(define tLeft (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (leaf-t 8)))
(define tRight (node-t 'w (node-t 'x (leaf-t 2) (leaf-t 5)) (node-t 'y (leaf-t 7) (leaf-t 5))))

(display "Question 5\n")
tLeft
tRight 

; Question 6

;; dTree-height :: dTree? -> number?
(define dTree-height
  (lambda (t)
    (cases dTree t
      (leaf-t (datum) 0)
      (node-t (symbol left right) (+ 1 (max (dTree-height left) (dTree-height right)))))))

;; dTree-size :: dTree? -> number?
(define dTree-size
  (lambda (t)
    (cases dTree t
      (leaf-t (datum) 1)
      (node-t (symbol left right) (+ 1 (dTree-size left) (dTree-size right))))))


;; dTree-paths :: dTree? -> listof listof number?
(define dTree-paths
  (lambda (t)
    (cases dTree t
      (leaf-t (datum) '())
      (node-t (symbol left right)
         (append (find-paths left '0) (find-paths right '1))))))

;; find-paths :: dTree? -> listof number? -> listof listof number?
(define find-paths  ;takes a node and the previous path to get to it
  (lambda (t prev)
    (cases dTree t
      (leaf-t (datum) (list prev))
      (node-t (symbol left right)
              (append (find-paths left (list prev '0)) (find-paths right (list prev '1)))))))

;; isLeaf? :: dTree? -> boolean?
(define isLeaf?
  (lambda (t)
    (cases dTree t
      (leaf-t (datum) #t)
      (node-t (symbol left right) #f))))

;; dTree-perfect? :: dTree? -> boolean?
(define dTree-perfect?
  (lambda (t)
    (cases dTree t
      (leaf-t (datum) #t)
      (node-t (symbol left right)
          (let ([a (isLeaf? left)] [b (isLeaf? right)])
            (cond
              [(and a b) #t]
              [(or a b) #f]
              [(and (not a) (not b)) (and (dTree-perfect? left) (dTree-perfect? right))])
          )))))

;; dTree-map (symbol? -> symbol?), (number? -> number?), dTree? -> dTree?
(define dTree-map
  (lambda (f g t)
    (cases dTree t
      (leaf-t (datum) (leaf-t (g datum)))
      (node-t (symbol left right)
              (node-t (f symbol) (dTree-map f g left) (dTree-map f g right))))))

;; aux functions
(define symbol-upcase
  (compose string->symbol (compose string-upcase symbol->string)))
(define (succ x) (+ x 1))

; testing all functions from part 2
(display "\nQuestion 6\n")
(display "(a) (dTree-height tLeft): ")
(dTree-height tLeft)
(display "(b) (dTree-size tLeft): ")
(dTree-size tLeft)
(display "(c) (dTree-paths tLeft): ")
(dTree-paths tLeft)
(display "(d.1) (dTree-perfect? tLeft): ")
(dTree-perfect? tLeft)
(display "(d.2) (dTree-perfect? tRight): ")
(dTree-perfect? tRight)
(display "(e) (dTree-map symbol-upcase succ tLeft): ")
(dTree-map symbol-upcase succ tLeft)


; Question 7

(define b0 '( (a b) . (((0 0) . 1) ((0 1) . 1) ((1 0) . 1) ((1 1) . 0))))
(define b1 '( (x y z) . (((0 0 0) . 0) ((0 0 1) . 1) ((0 1 0) . 1) ((0 1 1) . 0) ((1 0 0) . 1) ((1 0 1) . 0) ((1 1 0) . 0) ((1 1 1) . 1) )))

;; list->tree :: listof symbol? -> dTree?
(define list->tree
  (lambda (sym)
    (if (null? (cdr sym))
        (node-t (car sym) (leaf-t 8) (leaf-t 8))
        (node-t (car sym) (list->tree (cdr sym)) (list->tree (cdr sym))))))

;; replaceLeafAt :: dTree?, a -> dTree?
(define replaceLeafAt
  (lambda (t bf)
    (if (null? bf)
        t  ;if is over
        (replaceLeafAt (replace t (car (car bf)) (cdr (car bf))) (cdr bf)) ; I give the tree, the path and the result
        )))

;; replace :: dTree?, listof number?, number? -> dTree?
(define replace
  (lambda (t path res)
    (if (and (pair? path) (= (car path) 0))
        (cases dTree t
          (leaf-t (datum) (leaf-t res))
          (node-t (symbol left right) (node-t symbol (replace left (cdr path) res) right))) ; left (0)
        (cases dTree t
          (leaf-t (datum) (leaf-t res))
          (node-t (symbol left right) (node-t symbol left (replace right (cdr path) res)))) ; right (1)
       )))
                     
;; bf->dTree :: a -> dTree
(define bf->dTree
  (lambda (bf)
    (replaceLeafAt (list->tree (car bf)) (cdr bf))))

(display "\nQuestion 7\n")
(display "(bf->dTree b1: \n")
(bf->dTree b1)