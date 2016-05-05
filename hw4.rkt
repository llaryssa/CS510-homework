#lang racket

(require compatibility/mlist)

(define stack
  (let ((stk '()))
    (lambda (message)
      (case message
        ((empty?) (lambda ()
                    (null? stk)))
        ((push!) (lambda (x)
                   (set! stk (cons x stk))))
        ((pop!) (lambda ()
                  (if (null? stk)
                      (error "Cannot pop!: the stack is empty")
                      (set! stk (cdr stk)))))
        ((top) (lambda ()
                  (if (null? stk)
                      (error "Cannot top: the stack is empty")
                      (car stk))))
        (else (error "stack: Invalid message" message))))))


((stack 'push!) 1)
((stack 'push!) 2)
((stack 'push!) 3)
((stack 'push!) 4)
((stack 'top))
((stack 'pop!))
((stack 'pop!))
((stack 'pop!))
((stack 'top))


(define (ex1 v1 v2)
  (let ((f
         (let ((curr '())) (lambda (x)
                             (begin
                               (set! curr (cons x curr))
                               curr)))))
    (begin
      (f v1)
      (f v2))))

(ex1 1 2)




(define c (mcons 1 2))
(define d (mcons 0 c))
(define e (mcons 0 c))


(define x 2)
(define (modify y)
  (begin
  (set! y 5)
  y))



(define a '(1 2))
(define b a)
