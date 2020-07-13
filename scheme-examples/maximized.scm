(require imgui-macros.scm)

(define (setup)
  ;; there seems to be a padding of 8 pixels each side?
  ;; plus.. 14? the menu bar?
  (sdl/set-window-size! 416 430)
  )

(define (draw)
  (imgui/m-maximized ("max")
   (when (imgui/begin-menu-bar)
     (when (imgui/begin-menu "File")
       (imgui/menu-item "Open")
       (imgui/end-menu))
     (imgui/end-menu-bar))
   ;; (imgui/text "maximized window")
   (let ((color (imgui.color/frgb->u32 1 0 0))
	 (menu-height 50))
     ;; cx cy r color segments
     (imgui.draw/circle 200
			200
			200
			color
			64)))
  (imgui/m-window
   ("popup window")
   (imgui/text "another window")
		))

(load "example_inc.scm")
(example/set-screenshot-and-exit "maximized.png")
