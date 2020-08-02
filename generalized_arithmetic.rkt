#lang racket

(require "functions.rkt")
(require "tuples.rkt")
(require "types.rkt")

(provide + - *)


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
