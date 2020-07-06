(require imgui-macros.scm)
(load "layout.scm")

(define w 400)
(define h 400)
(define w-padding 16)
(define h-padding 30)

(define (element-debug x y)
  (format *stderr* "x ~A y ~A\n" x y))
(define (setup)
  ;; there seems to be a padding of 8 pixels each side?
  ;; plus.. 14? the menu bar?
  (sdl/set-window-size (+ w w-padding) (+ h h-padding))
  ;; (colors/wheel element :N 4 :R 200)
  )

(comment
 (sdl/set-window-size 800 800)
 )

(define color (imgui.color/frgb->u32 1 0 0))

(define* (element x y (phase 1) (id 0))
  ;; (format #t "phase ~A\n" phase)
  (let ((color (imgui.color/frgb->u32 phase 0.5 0.5))
	(label (format #f "~A " id)))
    (imgui.draw/text x y label color)
    (imgui.draw/circle x y 20 color)))

(define (draw)
  (imgui/m-maximized
   ()
   (layout/circular element :N 12 :R 200 :r 20)
))
