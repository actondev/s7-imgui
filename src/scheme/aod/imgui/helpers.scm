(require aod.core)
(provide 'aod.imgui.helpers)
(aod/require aod.c.imgui :as ig)

(define (draw-circle-with-color circle color)
  (apply ig/draw-circle (append circle
				(list color))))

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
