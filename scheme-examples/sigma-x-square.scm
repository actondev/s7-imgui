;; a logo/visualization idea that I had

;; (define *test* #t)
(require aod.core)
(ns app.sxs)
(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.imgui :as ig)
(ns-require aod.c.foreign :as c)
(ns-require aod.geom :as geom)
(ns-require aod.imgui.helpers :as igh)
(ns-require aod.sxs :as sxs)

(define *phase (c/new-float 0.5))
(comment
 (*phase)
 )

(define (setup)
  (sdl/set-window-size! 420 400))

(define color (ig/frgb->u32 0.5 0.2 0.2))


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
       (igh/draw-lines lines color ))

     (ig/dummy 100 100))
   ))
