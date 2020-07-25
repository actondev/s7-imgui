(ns aod.imgui.helpers)
(ns-require aod.c.imgui :as ig)

(define* (draw-circle circle (color -1) (segments 0) (filled #f) (thickness 1))
  (if filled
      (apply ig/draw-circle-filled (apply-values circle)
	     color
	     segments
	     ())
      (apply ig/draw-circle (apply-values circle)
	     color
	     segments
	     thickness
	     ())))

;; hmm this draws clockwise from 3 o'clock
(define* (draw-arc circle (a-min 0) (a-max 1) (color -1) (segments 32) (thickness 1))
  (apply ig/draw-arc (apply-values circle)
	 a-min
	 a-max
	 color
	 segments
	 thickness
	 ()))

;; int -1 => white
(define* (draw-lines lines (color -1) (thickness 1))
  (map (lambda (line)
	 (apply ig/draw-line
		(apply-values line)
		color
		thickness
		()))
       lines))

(comment
 (apply ig/draw-line (append line
			     (list color)
			     (list thickness)))
 ;; vs
 (apply ig/draw-line
		(apply-values line)
		color
		thickness
		())
 
 )

(define (frgb->u32 color)
  (apply ig/color32
	 (map (lambda (val)
		(* val 255))
	      color)))

(test "frgb->u32 : input 0.0 .. 1.0"
      (is (= -1 (apply ig/color32 '(255 255 255))))
      (is (= -1 (frgb->u32 '(1 1 1)))))
