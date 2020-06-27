(display "loading imgui.scm\n")

(define (setup)
  (display "initializing imgui.scm: in setup\n"))

(define (draw)
  (aod.imgui/begin "s7 window")
  (aod.imgui/text "I like scheme :)")
  (aod.imgui/end)
  ;; done drawing
  )
