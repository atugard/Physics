#lang racket

(provide type-list? contains-type?
         tag-list get-tag tagged-list?
         up down up? down? tuple?
         not-number? not-null? negative? positive? negative-symbol? plural-type-checker numbers? symbols? procedures? pairs?  tuples? not-numbers? negative-symbols?)

(define (type-list? x type?)
  (cond [(null? x) true]
        [(not (pair? x)) false]
        [else (and (type? (car x)) (type-list? (cdr x) type?))]))
(define (contains-type? x type?)
  (cond [(not (pair? x)) false] 
        [(type? (car x)) true]
        [else (contains-type? (cdr x) type?)]))
(define (tag-list l t)
  (when (symbol? t)
    (cons t l)))
(define (get-tag l)
  (if (symbol? (car l))
      (car l)
      null))
(define (tagged-list? l)
  (if (pair? l)
      (symbol? (car l))
      false))

(define (up . args)
  (tag-list args 'up))
(define (down . args)
  (tag-list args 'down))
(define (up? t)
  (if (tagged-list? t)
      (eq? (get-tag t) 'up)
      false))
(define (down? t)
  (if (tagged-list? t)
      (eq? (get-tag t) 'down)
      false))
(define (tuple? t)
  (or (up? t) (down? t)))
(define (not-number? n)
  (not (number? n)))
(define (not-null? a)
  (not (null? a)))
(define (negative? n)
  (if (number? n)
      (< n 0)
      false))
(define (positive? n)
  (if (number? n)
      (> 0)
      false))
(define (negative-symbol? s)
  (if (and (pair? s) (not-null? (cdr s)))
      (and (eq? (car s) '-) (symbol? (cadr s)) (null? (cddr s)))
      false))


(define (plural-type-checker type?)
  (define (rec . args)
    (if (null? args)
        true
        (and (type? (car args)) (apply rec (cdr args)))))
  rec)
(define numbers?
  (plural-type-checker number?))
(define symbols?
  (plural-type-checker symbol?))
(define procedures?
  (plural-type-checker procedure?))
(define pairs?
  (plural-type-checker pair?))
(define tuples? 
  (plural-type-checker tuple?))
(define not-numbers?
  (plural-type-checker not-number?))
(define negative-symbols?
  (plural-type-checker negative-symbol?))


