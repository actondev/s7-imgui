;; (display "hi from audio player\n")
(require imgui-macros.scm)

(define audio-file #f)

(define (setup)
  (sdl/set-window-size! 500 300))

(define (draw-menu)
  (imgui/m-menu-bar
   ()
   (imgui/m-menu
    ("File")
    (imgui/m-menu-item ("Open")
		       (set! audio-file ((*nfd* 'open)))))))

(define (draw)
  (imgui/m-window
   ("test")
   (imgui/text "hi there"))
  (imgui/m-maximized
   ("s7 audio")
   (draw-menu)
   (imgui/text (format #f "Audio file: ~A" audio-file))
;;   (when audio-file)
   (when (imgui/button "Play")
     (audio/play)
     )
   (when (imgui/button "Stop")
     (audio/stop)
     )
   (when (imgui/button "Free audio buffer (hooray for noise)")
     (audio/free-wav)
     )))
