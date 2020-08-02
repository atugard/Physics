(define old+ +)
(define old- -)
(define old* *)

;;Implementations of data structure needed for the book.

(define (type-list? x type?)
  (cond [(null? x) true]
        [(not (pair? x)) false]
        [else (and (type? (car x)) (type-list? (cdr x) type?))]))

(define (contains-type? x type?)
  (cond [(not (pair? x)) false] 
        [(type? (car x)) true]
        [else (contains-type? (cdr x) type?)]))

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

(define (install-function-package m)
  (define (generate-arithmetic-functions op)
    (lambda fns
      (lambda x
        (apply op (map (lambda (fn) (apply fn x)) fns)))))
  (define +
    (generate-arithmetic-functions old+))
  (define -
    (generate-arithmetic-functions old-))
  (define *
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
  (define (dispatch m)
    (cond [(eq? m '+) +]
          [(eq? m '-) -]
          [(eq? m '*) *]
          [(eq? m 'compose) compose]))
  (dispatch m))

(define +func
  (install-function-package '+))
(define -func
  (install-function-package '-))
(define *func
  (install-function-package '*))
(define compose
  (install-function-package 'compose))

(define (+ . args)
  (cond [(type-list? args procedure?)
         (apply +func args)]
        [(contains-type? args symbol?)
         (tag-list args '+)]
        [else (apply old+ args)]))
(define (- . args)
  (cond [(type-list? args procedure?)
         (apply -func args)]
        [(contains-type? args symbol?)
         (tag-list args '-)]
        [else (apply old+ args)]))
(define (* . args)
  (cond [(type-list? args procedure?)
         (apply *func args)]
        [(contains-type? args symbol?)
         (tag-list args '*)]
        [else (apply old+ args)]))

(define (install-tuples-package m)
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
  (define (dispatch m)
    (cond [(eq? m 'up) up]
          [(eq? m 'down) down]
          [(eq? m 'ref) ref]
          [(eq? m 'component) component]
          [(eq? m '+tup) +tup]
          [(eq? m '-tup) -tup]
          [(eq? m '*tup) *tup]
          [(eq? m 'a*tup) a*tup]
          [else
           (error "Operation not recognized!" m)]))
  (dispatch m))


;;(define (contractive tup1 tup2)
;;  (cond [(not (and (tagged-list? tup1) (tagged-list? tup2))) false]
;;        [(and (eq? (get-tag tup1) 'up) (eq? (get-tag tup1) 'down)
;;              (eq? (length tup1) (length tup2))
;;              (or (car tup1)

(define up
  (install-tuples-package 'up))
(define down
  (install-tuples-package 'down))
(define ref
  (install-tuples-package 'ref))
(define component
  (install-tuples-package 'component))
(define +tup
  (install-tuples-package '+tup))
(define -tup
  (install-tuples-package '-tup))
(define *tup
  (install-tuples-package '*tup))
(define a*tup
  (install-tuples-package 'a*tup))


(define v
  (down 'v_1 'v_2 'v_3))
(define w
  (up 'w^1 'w^2 'w^3))

;;1.4 Computing actions
;;For a system of one free particle we have the Lagrangian of L(t,x,v) = \frac{1}{2} mv \dot v

;;(define ((L-free-particle1 mass) local)
;;  (let ((v (velocity local)))
;;    (* 1/2 mass (dot-product v v))))

;;Suppose that we let q denote a coordiante path function that maps time to position components:
;; q(t) = (x(t), y(t), z(t))
;; Then

;;(define q
;;  (up (literal-function 'x)
;;      (literal-function 'y)
;;      (literal-function 'z)))

;;where literal-function makes a procedure that represents a function of one argument that has no known properties other than the given symbolic name
;;A tuple of coordinate or velocity components is made with the procedure up.
;;Component i of the tuple q is (ref q i).
;;Up is to remind us that components indexed by superscripts in usual math notation.

