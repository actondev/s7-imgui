(display "loading imgui.scm\n")

(define click-counter 0)
(define window1-open ((*c-primitives* 'bool) #t))

;; the exposed functions
(define (setup)
  (display "initializing imgui.scm: in setup\n"))

(define (draw)
  (aod.imgui/begin "s7 window")
  (aod.imgui/text "I like scheme :)")

  (if (aod.imgui/button (format #f "Click ~A times" click-counter))
      (begin
	(set! click-counter (+ 1 click-counter))
	(format *stdout* "new counter ~A\n" click-counter)))
  (when (not (window1-open))
      (when (aod.imgui/button "Open the closeable window")
	(set! (window1-open) #t)))

  (aod.imgui/text "another one")
  (aod.imgui/end)

  (when (window1-open)
    (aod.imgui/begin "s7 closable window" window1-open)
    (aod.imgui/text "I like scheme as well :)")
    (aod.imgui/end))
  ;; done drawing
  )
