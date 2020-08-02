#lang racket 

(provide -- ++ square)

(define (-- n)
  (- n 1))
(define (++ n)
  (+ n 1))
(define (square n)
  (* n n))
