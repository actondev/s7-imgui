(ns rootlet
    :require ((aod.c.foreign :as c)
	      (aod.c.imgui :as ig)
	      (aod.imgui.macros :as igm)
	      (aod.c.nfd)
	      (aod.io :as io)
	      (aod.c.imgui.window-flags :as igw)
	      (aod.c.wm :as wm)))


(define windows (wm/list-windows))

;; adding boolean *sel?
;; (for-each (lambda (w)
;; 	    (with-let w
;; 		      (define *sel? (c/new-bool #f))))
;; 	  windows)

(define (draw)
  ;; (print "drawing")
  (igm/maximized
   ("s7 window switcher")
   '(for-each (lambda (w)
	       ;;(print "w is " w "title " (w 'title))
	       (when (ig/button (w 'title))
		 (print "clicked!")
		 (wm/raise-window (w 'window))
		 (wm/focus-window (w 'window))
		 )
	      )
	     windows)

   '(ig/list "windows" *isel window-titles)

   (let ((idx 0))
     (ig/begin-group)
     (for-each (lambda (w)
		 (set! idx (inc idx))
		 (when (ig/selectable (w 'title)
;;				      (w '*sel?)
				      #f
				      )
		   (print "window" w "clicked")
		   (when (ig/mouse-double-clicked? 0)
		     (print "double clicked!")
		     (wm/raise-window (w 'window))
		     (wm/focus-window (w 'window))
		     )))
	       windows)
     (ig/end-group))
   ))
