;;program needed to run this is scmutils

;;exercise 1.2: degrees of freedom
;;For each of the mechanical systems below, give the number of degrees of freedom of the configuration space.

;;a. three juggling pins.

;;each juggling pin can be described by (x,d) where x=(x_1,x_2,x_3) is it's position, and d=(d_1,d_2,d_3) is it's orientation.
;;so there's 6 degrees of freedom per juggling pin, and 6*3 = 18 for the whole system of 3 juggling

;;b. a spherical pendulum, consisting of a point mass hanging from a rigid massless rod attached to a fixed support point. The pendulum bob may move in any direction subject to the constraint imposed by the rigid rod. The point mass is subject to the uniform force of gravity.

;;it's configuration space is a sphere of radius the length of the rigid rod centered at the fixed support point, so it's got 2 degrees of freedom.

;;c. A spherical double pendulum, consisting of one point-mass hanging from a rigid massless rod attached to a second point-mass hanging from a second massless rod attached to a fixed support point. The point mass is subject to the uniform force of gravity. 

;;I think two spheres are enough here... so that'd be 4 degrees of freedom.

;;d. A point mass sliding without friction on a rigid curved wire.

;;I want to say just 1 degree of freedom. The rigid curved wire is homeomorphic to a line segment?

;;===================1.3 Generalized coordinates===================

;;exercise 1.3

;;b.

;;for the spherical pendulum, our function takes the point on the sphere (a1,a2), which is specified say by 2 angles, and gives something like c + (rcos(a1)sin(a2), rsin(a1)sin(a2), rcos(a2)), where c is the position of the support, and r is the length of the massless rigid rod...

;;c in this case we have something like (a1, a2, b1, b2), and we use these to compute actual positions in a 6 dimensional euclidean space... hmm...
;;well the first pendulum bob will be at x=c2 = c1 + (r1cos(a_1)sin(a_2), r1sin(a_1)sin(a_2), r2cos(a_2)), and then the second bob will be at
;; y=c2 + (r2cos(b_1)sin(b_2), r2sin(b_1)sin(b_2), r2cos(b_2)), where c_1, c_2 are 3 dimensional vectors... so we have the 6 dimensional configuration space given by (x,y)...

;;===================1.4 Computing actions===================

(define ((L-free-particle mass) local)
  (let ((v (velocity local)))
    (* 1/2 mass (dot-product v v))))

;;equivalent to:
;; (define ((L-free-particle mass)
;; 	 (lambda local
;; 	   (let ((v (velocity local)))
;; 	     (* 1/2 mass (dot-product v v))))

(define (square a)
  (* a a))

(define q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

(define (Lagrangian-action L q t1 t2)
  (definite-integral (compose L (Gamma q)) t1 t2))

(define (test-path t)
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

;;Exercise 1.4: Lagrangian actions
;;Show that the action on a solution path is m/2 (xb-xa)/(tb-ta)

;;we check that it's true with the following procedure:
(define (exercise1.4 q m ta tb)
  (let ((proposed   (* (/ m 2.0) (/ (square (magnitude (- (q tb) (q ta)))) (- tb ta))))
	(actual (Lagrangian-action (L-free-particle m) q ta tb)))
    (if (< (abs (- proposed actual)) 0.1)
	#t
	#f)))
;;-------------
(define ((make-eta nu t1 t2) t)
  (* (- t t1) (- t t2) (nu t)))

(define ((varied-free-particle-action mass q nu t1 t2) epsilon)
  (let ((eta (make-eta nu t1 t2)))
    (Lagrangian-action (L-free-particle mass)
		       (+ q (* epsilon eta))
		       t1
		       t2)))

      




