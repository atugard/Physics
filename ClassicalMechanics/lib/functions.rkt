#lang racket

(require "oldfunctions.rkt")
(require "types.rkt")

(provide + - * compose expt exp sin cos tan square sqrt cube)

(define identities
  (list
   (cons '(+ (square cos) (square sin)) 1)
   (cons '(+ (square sin) (square cos)) 1)))

;;an idea for how to deal with this is... to keep track of the argument, and pass it to the identity, and see if its the same, in the case of a non-numeric result?
;;downside is having to evaluate all identities for each computed function... 
(define (simplifier l)
  (display 'notyetimplemented)
  )

;; we have a_1 + a_2 + ... + a_n = a_1 + (a_2 + ... + a_n), so we use recursion to define addition.
(define (+ . args)
  (cond [(null? args) 0]
        [(null? (cdr args)) (car args)]
        [(null? (cddr args)) 
         (let ((a1 (car args))
               (a2 (cadr args)))
           (cond [(or (symbols? a1 a2)
                      (negative-symbols? a1 a2)
                      (and (negative-symbol? a1) (symbol? a2))
                      (and (symbol? a1) (negative-symbol? a2))
                      (and (or (symbol? a1) (negative-symbol? a1)) (number? a2))
                      (and (number? a1) (or (symbol? a2) (negative-symbol? a2))))
                  (list '+ a1 a2)]
                 [(numbers? a1 a2)
                  (old+ a1 a2)]
                 [(procedures? a1 a2)
                      (lambda x
                        (let ((b1 (apply a1 x))
                              (b2 (apply a2 x)))
                          (if (numbers? b1 b2)
                              (old+ b1 b2)
                              (tag-list (list b1 b2) '+))))]
                 [(tuples? a1 a2)
                  ;;nyi
                  ]
                 [(and (symbol? a1) (procedure? a2))
                  (lambda x (list '+ a1 (apply a2 x)))]
                 [(and (procedure? a1) (symbol? a2))
                  (lambda x (list '+ a2 (apply a1 x)))]
                 [(and (negative-symbol? a1) (procedure? a2))
                  (lambda x (list '+ a1 (apply a2 x)))]
                 [(and (procedure? a1) (negative-symbol? a2))
                  (lambda x (list '+ a2 (apply a1 x)))]
                 [(and (number? a1) (procedure? a2))
                  (lambda x
                    (let ((b (apply a2 x)))
                          (if (number? b)
                              (old+ a1 b)
                              (tag-list (list a1 b) '+))))]
                 [(and (procedure? a1) (number? a2))
                  (lambda x
                    (let ((b (apply a1 x)))
                          (if (number? b)
                              (old+ a2 b)
                              (tag-list (list a2 b) '+))))]
                 [(and (procedure? a1) (tuple? a2)) ;;defined if dim(Im(a1)) = length(a2)
                  ;;nyi
                  ]
                 [(and (procedure? a2) (tuple? a1))
                  ;;nyi
                  ]
                 [(and (procedure? a1) (pair? a2) (eq? (get-tag a2) '+))
                  (lambda x (tag-list (list (apply a1 x) a2) '+))]
                 [(and (procedure? a2) (pair? a1) (eq? (get-tag a1) '+))
                  (lambda x (tag-list (list (apply a2 x) a1) '+))]
                 [(or (and (symbol? a1) (pair? a2) (eq? (get-tag a2) '+))
                      (and (symbol? a2) (pair? a1) (eq? (get-tag a1) '+))
                      (and (negative-symbol? a1) (pair? a2) (eq? (get-tag a2) '+))
                      (and (negative-symbol? a2) (pair? a1) (eq? (get-tag a1) '+))
                      (and (number? a1) (pair? a2) (eq? (get-tag a2) '+))
                      (and (number? a2) (pair? a1) (eq? (get-tag a1) '+)))
                  (tag-list (list a1 a2) '+)]))]
        [else
         (+ (car args) (apply + (cdr args)))]))

;;We have a_1 - a_2 - ... - a_n = a_1 + -a_2 + ... + -a_n
(define (- . args)
  (cond [(null? args) (error "Require at least one argument.")]
        [(null? (cdr args))
         (let ((a (car args)))
           (cond [(number? a) (old- a)]
                 [(symbol? a) (list '- a)]
                 [(negative-symbol? a) (cadr a)]
                 [(procedure? a)
                  (lambda x
                    (let ((b (apply a x)))
                      (if (number? b)
                          (old- b)
                          (list '- b))))]
                 [(tuple? a)
                  ;;nyi
                  ]
                 [(and (tagged-list? a) (or (eq? (get-tag a) '+) (eq? (get-tag a) '*)))
                  (list '- a)]))]
        [else
         (define (rec l)
           (if (null? l)
               null
               (cons (- (car l)) (rec (cdr l)))))
         (apply + (cons (car args) (rec (cdr args))))]))

;;a*b = a + a + a + ... + a <-- b times
;;a_1 * a_2 * ... * a_n = a_1 * (a_2 * ... * a_n) <-- can use recursion
(define (* . args)
  (cond [(null? args) 1]
        [(null? (cdr args)) (car args)]
        [(null? (cddr args))
         (let ((a1 (car args)) (a2 (cadr args)))
           (cond [(or (eq? a1 0) (eq? a2 0)) 0]
                 [(numbers? a1 a2) (old* a1 a2)]
                 [(or (and (positive? a1) (or (symbol? a2) (negative-symbol? a2)))
                      (and (positive? a2) (or (symbol? a1) (negative-symbol? a2)))
                      (and (negative-symbol? a1) (symbol? a2))
                      (and (negative-symbol? a2) (symbol? a1))
                      (symbols? a1 a2))
                  (list '* a1 a2)]
                 [(or (and (negative? a1) (negative-symbol? a2))
                      (and (negative? a2) (negative-symbol? a1))
                      (negative-symbols? a1 a2))
                  (list '* (- a1) (- a2))]
                 [(tuples? a1 a2)
                  ;;nyi
                  ]
                 [(and (tuple? a1) (number? a2))
                  ;;nyi
                  ]
                 [(and (tuple? a2) (number? a1))
                  ;;nyi
                  ]
                 [(and (tuple? a1) (symbol? a2))
                  ;;nyi
                  ]
                 [(and (tuple? a2) (symbol? a1))
                  ;;nyi
                  ]
                 [(and (tuple? a1) (negative-symbol? a1))
                  ;;nyi
                  ]
                 [(and (tuple? a2) (negative-symbol? a1))
                  ;;nyi
                  ]
                 [(procedures? a1 a2)
                  (lambda x
                    (let ((b1 (apply a1 x))
                          (b2 (apply a2 x)))
                      (if (numbers? b1 b2)
                          (old* b1 b2)
                          (tag-list (list b1 b2) '*))))]
                 [(and (procedure? a1) (symbol? a2))
                  (lambda x (list '* a2 (apply a1 x)))]
                 [(and (symbol? a1) (procedure? a2))
                  (lambda x (list '* a1 (apply a2 x)))]
                 [(and (negative-symbol? a1) (procedure? a2))
                  (lambda x (list '* a1 (apply a2 x)))]
                 [(and (procedure? a1) (negative-symbol? a2))
                  (lambda x (list '* a2 (apply a1 x)))]
                 [(and (procedure? a1) (number? a2))
                  (lambda x
                    (let ((b (apply a1 x)))
                      (if (number? b)
                          (old* b a2)
                          (tag-list (list b a2) '*))))]
                 [(and (procedure? a2) (number? a1))
                  (lambda x
                    (let ((b (apply a2 x)))
                      (if (number? b)
                          (old* a1 b)
                          (tag-list (list b a1) '*))))]
                 [(and (procedure? a1) (pair? a2) (eq? (get-tag a2) '*))
                  (lambda x (tag-list (list (apply a1 x) a2) '*))]
                 [(and (procedure? a2) (pair? a1) (eq? (get-tag a1) '*))
                  (lambda x (tag-list (list (apply a2 x) a1) '*))]
                 [(or (and (symbol? a1) (pair? a2) (eq? (get-tag a2) '*))
                      (and (symbol? a2) (pair? a1) (eq? (get-tag a1) '*))
                      (and (negative-symbol? a1) (pair? a2) (eq? (get-tag a2) '*))
                      (and (negative-symbol? a2) (pair? a1) (eq? (get-tag a1) '*))
                      (and (number? a1) (pair? a2) (eq? (get-tag a2) '*))
                      (and (number? a2) (pair? a1) (eq? (get-tag a1) '*)))
                  (tag-list (list a1 a2) '*)]))]
        [else
         (* (car args) (apply * (cdr args)))]))

                  
(define (compose f g)
  (lambda (x)
    (f (g x))))
  

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
(define (sqrt a)
  (if (not (number? a))
      (list 'sqrt a)
      (oldsqrt a)))
(define (square a)
  (* a a)) 
(define (cube a)
  (* a a a))


