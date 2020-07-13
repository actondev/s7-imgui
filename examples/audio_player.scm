(display "hi from audio player\n")
(require aod.core)
(require imgui-macros.scm)
(aod/require aod.imgui.macros :as igm)

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
   (when (imgui/button "Glitch!")
     (audio/glitch)
     ))
    
    (igm/begin ("test window 2")
	       ;; no menu flags.. gotta work on the flags thing
	       '(igm/menu-bar ()
			 (igm/menu ("file")))
	       (imgui/text "hi there!")))
