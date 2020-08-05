;;1.4 Computing actions
;;For a system of one free particle we have the Lagrangian of L(t,x,v) = \frac{1}{2} mv \dot v

(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))


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

(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))

(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))

(define ((varied-free-particle-action mass q nu t1 t2) eps)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
                       (+ q (* eps eta))
                       t1
                       t2)))
;;example implementation of make path:
;;(define (make-path t0 q0 t1 q1 qs)
;;  (let ((n (length qs)))
;;    (let ((ts (linear-interpolants t0 t1 n)))
;;      (Lagrangian-interpolation-function
;;       (append (list q0) qs (list q1))
;;       (append (list t0) ts (list t1))))))
;;linear-interpolants produces a list of elements that linearly interpolate the first two arguments.
;;We use this procedure to get ts, the n evenly spaced intermediate times between t0 and t1 at which the path will be specified.
;;qs are the positions at these intermediate times.
;;The procedure Lagrange-interpolation-function takes a list of values and a list of times and produces a procedure that computes the Lagrange interpolation polynomial that passes through these points.

(define win2 (frame 0.0 :pi/2 0.0 1.2))


(define ((parametric-path-action Lagrangian t0 q0 t1 q1) intermediate-qs)
  (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
    ;;display path
    (graphics-clear win2)
    (plot-function win2 path t0 t1 (/ (- t1 t0) 100))
    ;;compute action
    (Lagrangian-action Lagrangian path t0 t1)))

(define (find-path Lagrangian t0 q0 t1 q1 n)
  (let ((initial-qs (linear-interpolants q0 q1 n)))
    (let ((minimizing-qs
           (multidimensional-minimize
            (parametric-path-action Lagrangian t0 q0 t1 q1)
            initial-qs)))
      (make-path t0 q0 t1 q1 minimizing-qs))))
;;The procedure multidimensional-minimize takes a procedure that computes the function to be minimized and an initial guess for the parameters.

;;To illustrate the use of this strategy, we find trajectories of harmonic oscillator, with Lagrangian:
;;L(t,q,v) = 1/2 m v^2 - 1/2 k q^2:

(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
        (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))

;;We can find an approximate path taken by the harmonic oscillator for m=1 and k=1 between q(0)=1 and q(pi/2) = 0 as follows:

(define q
  (find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 2))

;;We know that the trajectories of this harmonic oscillator for m=1 and k=1 are
;;q(t) = Acos(t+\phi), in our case q(t) = cos(t)
