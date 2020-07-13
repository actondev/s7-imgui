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

(load "example_inc.scm")
(example/set-screenshot-and-exit "menu.png")
