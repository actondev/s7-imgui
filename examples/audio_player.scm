(display "hi from audio player\n")
(require aod.core)
(aod/require aod.imgui.macros :as igm)
(aod/require aod.c.imgui :as ig)

(define audio-file #f)

(define (setup)
  (sdl/set-window-size! 500 300))

(define (draw-menu)
  (igm/menu-bar
   ()
   (igm/menu
    ("File")
    (igm/menu-item ("Open")
		       (set! audio-file ((*nfd* 'open)))))))

#;
(format *stderr* "Expanded: ~A\n" (macroexpand (igm/maximized ("test")
							      (ig/text "hi"))))

(define (draw)

  (igm/maximized
   ("s7 audio")
   (draw-menu)
   (ig/text (format #f "Audio file: ~A" audio-file))
   (when (ig/button "Play")
     (audio/play)
     )
   (when (ig/button "Stop")
     (audio/stop)
     )
   (when (ig/button "Glitch!")
     (audio/glitch)
     ))
    
    (igm/window ("test window 2")
	       ;; no menu flags.. gotta work on the flags thing
	       '(igm/menu-bar ()
			 (igm/menu ("file")))
	       (ig/text "hi there!")))
