#lang racket

(require "functions.rkt")
(require "tuples.rkt")
(require "types.rkt")

(provide + - * expt exp sin cos tan square sqrt)


(define (reducer l op)
  (let  ((numbers (filter number? l))
         (non-numbers (filter (lambda (x) (not (number? x))) l)))
    (cons (apply op numbers) non-numbers)))

(define (+ . args)
  (cond [(type-list? args procedure?)
         (apply func+ args)]
        [(contains-type? args symbol?)
         (tag-list (reducer args old+) '+)]
        [else (apply old+ args)]))
(define (- . args)
  (cond [(type-list? args procedure?)
         (apply func- args)]
        [(contains-type? args symbol?)
         (tag-list (reducer args old-) '-)]
        [else (apply old+ args)]))
(define (* . args)
  (cond [(type-list? args procedure?)
         (apply func* args)]
        [(contains-type? args symbol?)
         (tag-list (reducer args old*) '*)]
        [else (apply old+ args)]))

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
