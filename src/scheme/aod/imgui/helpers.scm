(require aod.core)
(provide 'aod.imgui.helpers)
(aod/require aod.c.imgui :as ig)

(define (draw-circle-with-color circle color)
  (apply ig/draw-circle (append circle
				(list color))))

(define (draw-lines-with-color lines color)
  (map (lambda (line)
	 (apply ig/draw-line (append line
				     (list color)))
	 )
       lines))
