(display "loading imgui.scm\n")

(define (setup)
  (display "initializing imgui.scm: in setup\n"))

(define click-counter 0)

(define (draw)
  (aod.imgui/begin "s7 window")
  (aod.imgui/text "I like scheme :)")

  (if (aod.imgui/button (format #f "Click ~A times" click-counter))
      (begin
	(set! click-counter (+ 1 click-counter))
	(format *stdout* "new counter ~A\n" click-counter)))

  (aod.imgui/text "another one")
  (aod.imgui/end)
  ;; done drawing
  )
