#lang racket
(require "math.rkt")
(provide old+ old- old* oldexpt oldexp oldsin oldcos oldtan oldsquare oldsqrt func+ func- func* compose)

(define old+ +)
(define old- -)
(define old* *)
(define oldexpt expt)
(define oldexp exp)
(define oldsin sin)
(define oldcos cos)
(define oldtan tan)
(define oldsquare square)
(define oldsqrt sqrt)

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
(define (compose f g)
  (display 'f))
;; (if (and (procedure? f) (procedure? g))
;;     (lambda x
;;       (cond [(number-list? x)
;;              (apply f (apply g x))]
;;             [else
;;              (define (it l result)
;;                (list f (list g
;;             
;;     (error "Can only compose two procedures. You gave: " f g)))


