(ns aod.components.piano-wheel
    :require ((aod.c.imgui :as ig)
	      (aod.imgui.helpers :as igh)
	      (aod.layout :as l)))

(define black-keys '(1 3 6 8 10))
(define color-black (igh/frgb->u32 '(0 0 0)))
(define color-white (igh/frgb->u32 '(1 1 1)))

(define* (new (R 150) (cx R) (cy R) (padding-factor 0.1))
  (lambda* (x y (N 1) (n 0) (phase 0))
	   (let* ((d-theta (/ (* 2 pi)
			      N))
		  (offset (* d-theta padding-factor))
		  (a1 (- (* n d-theta)
			 (+ (/ pi 2)
			    (/ d-theta 2))))
		  (a2 (+ a1 d-theta))
		  (color (if (member n black-keys)
			     color-black
			     color-white)))
	     (igh/draw-arc (list cx cy R)
			   (+ a1 offset)
			   (- a2 offset)
			   :color color
			   :thickness 10))))

(define demo-element (new :R 50))
(define* (draw (element demo-element))
  (l/circular element :N 12)
  )
