(require "tuples.rkt")
(require "functions.rkt")

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

