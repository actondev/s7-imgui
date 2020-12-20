(ns rootlet
    :require ((aod.c.foreign :as c)
	      (aod.c.imgui :as ig)
	      (aod.imgui.macros :as igm)
	      (aod.c.nfd)
	      (aod.io :as io)
	      (aod.c.imgui.window-flags :as igw)
	      (aod.c.wm :as wm)))


(define windows (wm/list-windows))
(print windows)

(define (draw)
  ;; (print "drawing")
  (igm/maximized
   ("s7 window switcher")
   (for-each (lambda (w)
	       ;;(print "w is " w "title " (w 'title))
	       (when (ig/button (w 'title))
		 (print "clicked!")
		 (wm/raise-window (w 'window))
		 (wm/focus-window (w 'window))
		 )
	      )
	    windows)
   ))
