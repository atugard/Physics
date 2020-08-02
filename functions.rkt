#lang racket

(require "oldfunctions.rkt")
(require "tuples.rkt")
(require "types.rkt")

(provide + - * compose expt exp sin cos tan square sqrt)

(define (reducer l op)
  (let  ((numbers (filter number? l))
         (non-numbers (filter (lambda (x) (not-number? x)) l)))
    (if (null? numbers)
        l
        (cons (apply op numbers) non-numbers))))


(define (generate-arithmetic-functions op)
  (lambda fns
    (lambda x
      (apply op (map (lambda (fn) (apply fn x)) fns)))))
(define func+
  (generate-arithmetic-functions old+))
(define func-
  (generate-arithmetic-functions old-))
(define func*
  (generate-arithmetic-functions old*))

(define (+ . args)
  (cond [(null? args) 0]
        [(null? (cdr args)) (car args)]
        [(type-list? args procedure?)
         (apply func+ args)]
        [(contains-type? args not-number?)
         (tag-list (reducer args old+) '+)]
        [else (apply old+ args)]))
(define (- . args)
  (cond [(null? args) (error "Require at least one argument. You gave: 0")]
        [(type-list? args procedure?)
         (apply func- args)]
        [(contains-type? args not-number?)
         (if (null? (cdr args))
             (tag-list args '-)
             (tag-list (reducer args old-) '-))]
        [else (apply old- args)]))
(define (* . args)
  (if (null? args)
      1
      (cond [(type-list? args procedure?)
              (apply func* args)]
             [(contains-type? args not-number?)
              (tag-list (reducer args old*) '*)]
             [else (apply old+ args)])))

(define (compose f g)
  (display 'f))
(define (expt a b)
  (if (or (not (number? a)) (not (number? b)))
      (list 'expt a b)
      (oldexpt a b)))
(define (exp a b)
  (if (or (not (number? a)) (not (number? b)))
      (list 'exp a b)
      (oldexp a b)))
(define (sin a)
  (if (not (number? a))
      (list 'sin a)
      (oldsin a)))
(define (cos a)
  (if (not (number? a))
      (list 'cos a)
      (oldcos a)))
(define (tan a)
  (if (not (number? a))
      (list 'tan a)
      (oldtan a)))
(define (square a)
  (if (not (number? a))
      (list 'square a)
      (oldsquare a)))
(define (sqrt a)
  (if (not (number? a))
      (list 'sqrt a)
      (oldsqrt a)))
