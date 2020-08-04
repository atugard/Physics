#lang racket

(require "types.rkt")

(provide up down up? down? tuple? ref component +tup -tup *tup a*tup)

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

(define (ref tup i)
  (let ((tup (cdr tup)))
    (define (traverse tup i)
      (cond [(null? tup)
             null]
            [(eq? i 0)
             (car tup)]
            [else
             (traverse (cdr tup) (- i 1))]))
    (traverse tup i)))
(define ((component . args) tup)
  (if (null? args)
      tup
      ((apply component (cdr args)) (ref tup (car args)))))
(define (+tup val1 val2)
  (if (and (tagged-list? val1) (tagged-list? val2)
           (and (eq? (length val1) (length val2))))
      (let ((tag1 (get-tag val1))
            (tag2 (get-tag val2))
            (rest1 (cdr val1))
            (rest2 (cdr val2)))
        (define (add l1 l2)
          (if (null? l1)
              null
              (cons (list '+ (car l1) (car l2)) (add (cdr l1) (cdr l2)))))
        (cond [(and (eq? tag1 'up) (eq? tag2 'up))
               (apply up (add rest1 rest2))]
              [(and (eq? tag1 'down) (eq? tag2 'down))
               (apply down (add rest1 rest2))]
              [else
               (error "This operation requires either up, up or down, down, you gave: " tag1 tag2)]))
      (error "This operation requires that both arguments be tagged tuples of the same length.")))
(define (-tup val1 val2)
  (if (and (tagged-list? val1) (tagged-list? val2)
           (and (eq? (length val1) (length val2))))
      (let ((tag1 (get-tag val1))
            (tag2 (get-tag val2))
            (rest1 (cdr val1))
            (rest2 (cdr val2)))
        (define (sub l1 l2)
          (if (null? l1)
              null
              (cons (list '- (car l1) (car l2)) (sub (cdr l1) (cdr l2)))))
        (cond [(and (eq? tag1 'up) (eq? tag2 'up))
               (apply up (sub rest1 rest2))]
              [(and (eq? tag1 'down) (eq? tag2 'down))
               (apply down (sub rest1 rest2))]
              [else
               (error "This operation requires either up, up or down, down, you gave: " tag1 tag2)]))
      (error "This operation requires that both tuples be tagged up or down, and be of the same length.")))
(define (*tup val1 val2)
  (if (and (tagged-list? val1) (tagged-list? val2)
           (and (eq? (length val1) (length val2))))
      (let ((tag1 (get-tag val1))
            (tag2 (get-tag val2)))
        (cond [(or (and (eq? tag1 'down) (eq? tag2 'up))
                   (and (eq? tag1 'up) (eq? tag2 'down)))
               (define (dot-prod l1 l2)
                 (define (componentwise* l1 l2)
                   (if (null? l1)
                       null
                       (cons (list '* (car l1) (car l2)) (componentwise* (cdr l1) (cdr l2)))))
                 (cons '+ (componentwise* l1 l2)))
               (dot-prod (cdr val1) (cdr val2))]
              [else (error "This operation requires either up, down or down, up, you gave: " tag1 tag2)]))
      (error "This operation requires two tuples of the same length.")))
(define (a*tup val1 val2)
  (if (and (number? val1) (tagged-list? val2))
      (let ((tag2 (get-tag val2)))
        (define (scalar-mul l)
          (if (null? l)
              null
              (cons (list '* val1 (car l)) (scalar-mul (cdr l)))))
        (cond [(eq? 'up tag2)
               (apply up (scalar-mul (cdr val2)))]
              [(eq? 'down tag2)
               (apply down (scalar-mul (cdr val2)))]
              [else
               (error "Unrecognized tag: " tag2)]))
      (error "We require two arguments: the first a number, the second a tuple either tagged up or down.")))


;;(define (contractive tup1 tup2)
;;  (cond [(not (and (tagged-list? tup1) (tagged-list? tup2))) false]
;;        [(and (eq? (get-tag tup1) 'up) (eq? (get-tag tup1) 'down)
;;              (eq? (length tup1) (length tup2))
;;              (or (car tup1)
