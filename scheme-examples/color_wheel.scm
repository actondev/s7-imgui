(require aod.core)

(aod/require aod.colors :as colors)
(aod/require aod.c.imgui :as ig)
(aod/require aod.imgui.macros :as igm)
(aod/require aod.layout :as layout)

(define w 400)
(define h 400)
;; there seems to be a padding of 8 pixels each side?
;; plus.. 14? the menu bar?
(define w-padding 16)
(define h-padding 30)

(define (setup)
  (sdl/set-window-size! (+ w w-padding) (+ h h-padding)))

(define N 12)

;; phase 0 .. 1 (like 0 .. 2pi)
(define* (element x y (phase 0) (n 0) (N 1))
  (let* ((rgb (colors/rgb-phase phase))
	 ;; red is (car rgb) or (rgb 0).. it's the same
	 ;; green (cadr rgb) or (rgb 1) etc
	 (color (ig/frgb->u32 (rgb 0) (rgb 1) (rgb 2)))
	 (label (format #f "~A " n)))
    (ig/draw-text x y label color)
    (ig/draw-circle x y 20 color)))

(define (draw)
  (igm/maximized
   ("color wheel")
   (layout/circular
    element
    :N N :R 200 :r 20)))

(load "example_inc.scm")
(example/set-screenshot-and-exit "color_wheel.png")
