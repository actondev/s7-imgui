(require aod.core)
(provide 'aod.imgui.helpers)
(aod/require aod.c.imgui :as ig)

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
