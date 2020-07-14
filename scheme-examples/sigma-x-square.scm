;; a logo/visualization idea that I had

(require aod.core)
(aod/require aod.imgui.macros :as igm)
(aod/require aod.c.imgui :as ig)
(aod/require aod.c.foreign :as c)

;; (format *stderr* "pow 2 3 ~A\n" (pow 2 3))

(define *phase (c/new-float 0.5))
(comment
 (*phase)
 )

(define (setup)
  (sdl/set-window-size! 400 400))

(define color (ig/frgb->u32 0.5 0.2 0.2))

(define* (draw-arrow cx cy r (dir :right) (offs-x 0) (offs-y 0) (color (ig/frgb->u32 0.5 0.5 0.5)))
  (map (lambda (theta)
	 (apply ig/draw-line (append (sxs/circular-line :cx cx :cy cy :r r
							:offs-x offs-x
							:offs-y offs-y
							:theta theta)
				     (list color))))
       (sxs/-arrow-angles :dir dir)))

(define* (sxs/circular-line cx cy r theta
			    (offs-x 0)
			    (offs-y 0)
			    (clip #t)
			    (mod-theta (lambda (theta)
					 (- theta))))
  
  (let* ((r (if (or (not clip)
		    (= 0 offs-x offs-y))
		r
		;; else..
		(- r (sqrt (+
			    (pow offs-x 2)
			    (pow offs-y 2))))
		))
	 ;; (_ (format *stderr* "offs-x ~A r ~A\n" offs-x r))
	 (theta (mod-theta theta))
	 (x (* r (cos theta)))
	(y (* r (sin theta))))
    (list (+ cx offs-x ) (+ cy offs-y)
	  (+ cx offs-x x)
	  (+ cy offs-y y))))

(define (draw)
  (igm/maximized
   ("sigma-x-square")
   (ig/text "hi there you")
   (ig/slider-float "phase"
		    *phase
		    0
		    1)
   (ig/draw-circle 100
		   100
		   100
		   color)
   (draw-arrow :cx 100 :cy 100 :r 100 :dir :right)
   (draw-arrow :cx 100 :cy 100 :offs-x -70 :r 100 :dir :right)

   ;; this needs fixing: it can be displayed fully
   (draw-arrow :cx 100 :cy 100 :offs-x 70 :r 100 :dir :right)
   
   (draw-arrow :cx 100 :cy 100 :offs-x 70 :r 100 :dir :left)
   (draw-arrow :cx 100 :cy 100 :r 100 :dir :left)
   (ig/dummy 100 100)
   ))


;; needs the direction to which the arrow is looking at
(define* (sxs/-arrow-angles (dir :right))
  ;; or.. I could just hardcode it.. wtf
  (let* ((angle-a (* 0.75 pi))
	 (angle-b (+ angle-a (/ pi 2))))
    (cond ((eq? dir :right)
	   (list angle-a angle-b))
	  (else
	   (list (- angle-a pi)
		 (- angle-b pi))))))

;; returns x1 y1 x2 y2
;; modifying the theta to actually have 0 to 3 o'clock and go anti-clock wise




(comment
 (let ((ig/draw-line (lambda args
		       (map (lambda (x)
			      (format *stderr* "arg ~A\n" x))
			    args))))
   (draw-arrow :cx 100 :cy 100 :r 100 :theta 0))
 )

(comment
 (sxs/circular-line :cx 100 :cy 100 :r 100 :theta 0)
 ;; => (100 100 200 100)
 (sxs/circular-line :cx 100 :cy 100 :r 100 :theta (/ pi 2))
 ;; => (100 100 100.0 200.0)
 )

(define (aod.geom/rad->deg rad)
  (/ (* 180 rad) pi))

(comment

 (map aod.geom/rad->deg
      (sxs/-arrow-angles :dir :left))
 ;; => (-45.0 45.0)
 (map aod.geom/rad->deg
      (sxs/-arrow-angles :dir :right))
 ;; => (135.0 225.0)
 
 )
