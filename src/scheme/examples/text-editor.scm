(ns-require aod.c.imgui :as ig)
(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.imgui.window-flags :as igw)
(ns-require aod.c.nfd)

(define (setup)
  ()
  )

(define (draw-menu)
  (igm/menu-bar
   ()
   (igm/menu
    ("File")
    (igm/menu-item ("Open 2")
		   (print "called open")
		   (aod.c.nfd/open))
    (igm/menu-item ("Save")
		   (print "Save")
		   (aod.c.nfd/save)))))

(define (draw)
  (igm/maximized
   ("s7 texxt editor" igw/MenuBar)
   (draw-menu)
   (ig/text "hi there")))
