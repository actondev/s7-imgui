(require imgui-macros.scm)

(define (setup)
  ())

(define (draw-circles)
  (let ((color (imgui.color/frgb->u32 1 1 1)))
     (imgui/m-horizontal
      (imgui/m-group
       ()
       (imgui/text "circles:")
       (imgui/text "circles:")
       (imgui/text "circles:")
       (imgui/text "circles:"))
      (imgui/m-group
       ()
       (imgui/text "lo & behold")
       ;; (imgui/dummy 100 100)
       (imgui.draw/circle 50 50 50 color 32 1)
       (imgui/dummy 100 100)
       (imgui/text "after circle")
       )
      (imgui/m-group
       ()
       (imgui/text "lo & behold 2")
       (imgui.draw/circle 50 50 50 color 0 5)
       (imgui/dummy 100 100)
       (imgui/text "after circle")
       )
      (imgui/m-group
       ()
       (imgui/text "homocentric")
       (dotimes (i 5)
	 (let ((col (imgui.color/frgb->u32 1.0 0.0 (* 1.0 (/ i 4)))))
	   ;; going from red to red+blue
	   (imgui.draw/circle 50 50 (- 50 (* i 10)) col 0 2)))
       (imgui/dummy 100 100)
       (imgui/text "after circle")
       )
      )))

(define (draw-lines)
  (let ((color (imgui.color/frgb->u32 1 1 1)))
    (imgui/m-horizontal
      (imgui/m-group
       ()
       (imgui/text "lines:")
       (imgui/text "lines:")
       (imgui/text "lines:")
       (imgui/text "lines:"))
      (imgui/m-group
       ()
       (imgui/text "a line!")
       ;; (imgui/dummy 100 100)
       (imgui.draw/line 0 0 100 100 color)
       (imgui/dummy 100 100)
       (imgui/text "after line")
       )
      (imgui/m-group
       ()
       (imgui/text "lo & behold some lines")
       (dotimes (i 10)
	 (imgui.draw/line 0 0 100 (* i 10) color))
       (imgui/dummy 100 100)
       (imgui/text "after lines")
       ))))
(define (draw-shapes)
  (imgui/m-window
   ("shapes")
   (draw-circles)
   (draw-lines)
   )
  )

(define (draw)
  (draw-shapes)
  )

(define frame 0)
(define (post-draw)
  ;; on the first call there wasn't.. enough time? to show it on screen
  ;; the screenshot is black, but if i'm on debug (breakpoint) indeed i can see something and the screenshot is not blank
  ;; so.. 1 frame delay to have actually shown something in the screen
  ;; and then.. on first draw, there is just the menu for some reason, no windows
  ;; so, we take a screenshot on the third frame
  (set! frame (+ 1 frame))
  (when (= frame 3)
    (gl/save-screenshot "../scheme-examples/shapes.png")
    (exit)))
