;; a logo/visualization idea that I had

;; (define *test* #t)
(require aod.core)
(aod/require aod.imgui.macros :as igm)
(aod/require aod.c.imgui :as ig)
(aod/require aod.c.foreign :as c)
(aod/require aod.geom :as geom)
(aod/require aod.imgui.helpers :as igh)
(aod/require aod.sxs :as sxs)

(define *phase (c/new-float 0.5))
(comment
 (*phase)
 )

(define (setup)
  (sdl/set-window-size! 420 400))

(define color (ig/frgb->u32 0.5 0.2 0.2))

(define* (arrow-lines cx cy r (dir :right) (color (ig/frgb->u32 0.5 0.5 0.5)))
  (map (lambda (theta)
	 (sxs/circular-line :cx cx :cy cy :r r
			    :theta theta))
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
   (let ((circle '(200 130 100)))
     (ig/slider-float "phase"
		      *phase
		      0
		      1)
     (igh/draw-circle-with-color circle color)

     (let ((lines (sxs/lines circle :phase (*phase) :clip #t )))
       (igh/draw-lines-with-color lines color ))

     (ig/dummy 100 100))
   ))
