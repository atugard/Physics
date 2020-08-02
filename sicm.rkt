
;;1.4 Computing actions
;;For a system of one free particle we have the Lagrangian of L(t,x,v) = \frac{1}{2} mv \dot v
(define (-- n)
  (- n 1))
(define (++ n)
  (+ n 1))

(define (tag l t)
  (when (symbol? t)
    (cons t l)))
(define (get-tag l)
  (if (symbol? (car l))
      (car l)
      null))
(define (tagged-list? l)
  (symbol? (car l)))
(define (up . args)
  (tag args 'up))
(define (down . args)
  (tag args 'down))
(define (ref tup i)
  (let ((tup (cdr tup)))
    (define (traverse tup i)
      (cond [(null? tup)
             null]
            [(eq? i 0)
             (car tup)]
            [else
             (traverse (cdr tup) (-- i))]))
    (traverse tup i)))

(define ((component . args) tup)
  (if (null? args)
      tup
      ((apply component (cdr args)) (ref tup (car args)))))


(define ((arithmetic-tup-factory op) tup1 tup2)
  (if (and (tagged-list? tup1) (tagged-list? tup2)
           (and (eq? (length tup1) (length tup2))))
      ((define (op-lists l1 l2 op)
         (if (or (null? l1))
             null
             (cons (list op (car l1) (car l2)) (op-lists (cdr l1) (cdr l2) op))))
       (define (mul-lists l1 l2)
         (cons '+ (op-lists l1 l2 '*)))
       (let ((tag1 (get-tag tup1))
             (tag2 (get-tag tup2))
             (rest1 (cdr tup1))
             (rest2 (cdr tup2)))
         (cond [(and (eq? tag1 'up) (eq? tag2 'up) (not (eq? op '*)))
                (apply up (op-lists rest1 rest2 op))]
               [(and (eq? tag1 'down) (eq? tag2 'down) (not (eq? op '*)))
                (apply down (op-lists rest1 rest2 op))]
               [(or (and (eq? tag1 'down) (eq? tag2 'up) (eq? op '*))
                    (and (eq? tag1 'up) (eq? tag2 'down) (eq? op '*)))
                (mul-lists rest1 rest2)]
               [else
                (error "Unrecognized operation on tuples of the given type!" op tag1 tag2)])))
      
      (error "Tuples must be tagged up or down, each, and be of equal length!")))

(define add-tup
  (arithmetic-tup-factory '+))
(define sub-tup
  (arithmetic-tup-factory '-))
(define mul-tup
  (arithmetic-tup-factory '*))


(define v
  (down 'v_1 'v_2 'v_3))
(define w
  (down 'w_1 'w_2 'w_3))


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

