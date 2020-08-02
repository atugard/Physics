#lang racket 

(define (install-math-package m)
  (define (-- n)
    (- n 1))
  (define (++ n)
    (+ n 1))
  (define (square n)
    (* n n))
  (define (dispatch m)
    (cond [(eq? m '--) --]
          [(eq? m '++) ++]
          [(eq? m 'square) square]
          [else
           (error "Unrecognized operation: " m)]))
  (dispatch m))

(define --
  (install-math-package '--))

(define ++
  (install-math-package '++))

(define square
  (install-math-package 'square))
