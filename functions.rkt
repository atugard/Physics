#lang racket

(provide old+ old- old* func+ func- func* compose)

(define old+ +)
(define old- -)
(define old* *)


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


