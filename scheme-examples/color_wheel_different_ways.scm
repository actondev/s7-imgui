(require imgui-macros.scm)
(load "layout.scm")
(load "colors.scm")

(define w 400)
(define h 400)
;; there seems to be a padding of 8 pixels each side?
;; plus.. 14? the menu bar?
(define w-padding 16)
(define h-padding 30)

(define (element-debug x y)
  (format *stderr* "x ~A y ~A\n" x y))

(define (setup)
  (sdl/set-window-size! (+ w w-padding) (+ h h-padding))
  )

(comment
 (sdl/set-window-size! 800 800)
 )

(define color (imgui.color/frgb->u32 1 0 0))

(define N 12)

(define rgb-N (colors/rgb-steps N))
(define rgb-N-u32
  (map (lambda (rgb)
	 (imgui.color/frgb->u32 (rgb 0) (rgb 1) (rgb 2)))
       rgb-N))

;; phase 0 .. 1 (like 0 .. 2pi)
(define* (element x y (phase 0) (n 0))
  (let* ((rgb (colors/rgb-phase phase))
	 (red (car rgb))
	 (green (cadr rgb))
	 (blue (caddr rgb)))
    ;; (format #t "phase ~A\n" phase)
    (let ((color (imgui.color/frgb->u32 red green blue))
	  (label (format #f "~A " n)))
      (imgui.draw/text x y label color)
      (imgui.draw/circle x y 20 color))))

;; this is slightly more performant?
(define* (element-cache-1 x y (phase 1) (n 0))
  (let* ((rgb (rgb-N n)))
    ;; (format #t "phase ~A\n" phase)
    (let ((color (imgui.color/frgb->u32 (rgb 0) (rgb 1) (rgb 2)))
	  (label (format #f "~A " n)))
      (imgui.draw/text x y label color)
      (imgui.draw/circle x y 20 color))))

(define* (element-cache-2 x y (phase 1) (n 0))
  (let ((color (rgb-N-u32 n))
	(label (format #f "~A " n)))
    (imgui.draw/text x y label color)
    (imgui.draw/circle x y 20 color)))

(define (draw)
  ;; either passing element, element-cache-1 or element-cache-2 the same result will be yielded
  (imgui/m-maximized
   ()
   (layout/circular
    element
    ;; element-cache-1
    ;; element-cache-2
    :N N :R 200 :r 20)
))

(load "example_inc.scm")
(example/set-screenshot-and-exit "color_wheel.png")
