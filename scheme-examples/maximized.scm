(require imgui-macros.scm)

(define (setup)
  ;; there seems to be a padding of 8 pixels each side?
  (sdl/set-window-size 416 416)
  )

(define (draw)
  (imgui/begin-maximized)
  (when (imgui/begin-menu-bar)
    (when (imgui/begin-menu "File")
      (imgui/menu-item "Open")
      (imgui/end-menu))
    (imgui/end-menu-bar))
  ;; (imgui/text "maximized window")
  (let ((color (imgui.color/frgb->u32 1 0 0))
	(menu-height 50))
    (imgui.draw/circle 200
		       200
		       200
		       ;; (- 200 (* 2 menu-height))
		       color
		       64))
  (imgui/end)
  ())

(load "example_inc.scm")
(example/set-screenshot-and-exit "maximized.png")
