#lang racket 

(provide dec inc square)

(define (dec n)
  (- n 1))
(define (inc n)
  (+ n 1))
(define (square n)
  (* n n))
