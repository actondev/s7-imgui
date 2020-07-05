(require imgui-macros.scm)

(define (setup)
  ())

(define (draw-menu)
  (imgui/m-main-menu-bar
   ()
   (imgui/m-menu
    ("File")
    (imgui/m-menu-item
     ("Inc counter!")
     (begin
       ;; note that I can pass multiple statements in the macro
       ;; but for clarity it's nice to group them
       (format #t "inc counter clicked\n")
       (set! click-counter (+ 1 click-counter)))))
   (imgui/m-menu
    ("Settings")
    (imgui/m-menu-item ("Load settings")
		       (begin
			 (format #t "Load settings clicked\n"))))))

(define (draw)
  (draw-menu)
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
    (gl/save-screenshot "../gallery/menu.scm.png")
    (exit)))
