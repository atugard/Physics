#lang racket

(provide type-list? contains-type? tag-list get-tag tagged-list?)


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
