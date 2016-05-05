#lang racket

#| CS496: Homework 1
   Due date: Jan 29th
   Alana Laryssa Seabra A Santos |#


; question 1: that given any value returns 7.
;; seven :: a -> number?
(define seven
  (lambda (input) 7))

; question 2: that given an integer returns 1 if it is positive,
; -1 if it is negative and 0 if it is zero.
;; sign :: number? -> number?
(define (sign x)
  (if (= x 0) 0
      (if (> x 0) 1
          -1)))

; question 3: the absolute value function.
;; absolute :: number? -> number?
(define (absolute x)
  (if (= (sign x) -1) (- 0 x) x))

; question 4: the standard boolean connectives (you must resort to if-then-else).
;; andp :: boolean?, boolean? -> boolean?
(define (andp a b)
  (if (equal? a b)
      (if (equal? a #t) #t #f)
      #f))

;; orp :: boolean?, boolean? -> boolean?
(define (orp a b)
  (if (equal? a #t) #t
      (if (equal? b #t) #t #f)))

;; notp :: boolean? -> boolean?
(define (notp a)
  (if (equal? a #t) #f #t))

;; xorp :: boolean?, boolean? -> boolean?
(define (xorp a b)
  (if (andp a b) #f
      (if (orp a b) #t #f)))

; question 5: that given two numbers determines if the
; first is divisible by the second (use remainder).
;; dividesBy :: number?, number? --> boolean?
(define (dividesBy x y)
  (= (remainder x y) 0))

; question 6: a predicate that, given a list, returns a boolean indicating whether it has
;exactly one element. Provide two solutions, one using the match construct for pattern
;matching and another using the predicates cons? and null?.
;; singleton1? :: listof a -> boolean?
(define (singleton1? l)
  (if (cons? l) (null? (cdr l)) #f))

;; singleton2? :: listof a -> boolean?
(define (singleton2? l)
  (match l
    [(list a) #t]
    [_ #f]))

; question 7: a function that, given a pair, returns the same pair except that its
; first and second components are interchanged. Use match.
;; swap :: pair? a,b -> pair? b,a
(define swap
  (lambda (p)
    (match p
      [(cons a b) (cons b a)])))

; question 8: a function that, given two arguments, applies the first argument
; to the second one. Eg. if succ is the successor function then
;; app :: (a -> b), a -> b
(define (app func x) (func x))

; question 9: a function that, given two arguments, applies the first one
; to the second argument and then again to the result
;; twice :: (a -> a), a -> a
(define (twice func x) (func (func x)))

; question 10: compose: a function that, given three arguments, applies the second
; to the third and then the first to to its result.
;; compose :: (b -> c), (a -> b), a -> c
(define (compose f1 f2 x) (f1 (f2 x)))



;; utils
(define (f1 x)
  (match x
    [-1 "negative"]
    [0 "neutral"]
    [1 "positive"]))

;; examples:
; (compose f1 sign -45)
; (compose f1 sign 0)
; (compose f1 sign 37)