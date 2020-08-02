(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.imgui :as ig)
(ns-require aod.c.foreign :as c)

;; the exposed functions
(define (setup)
  (display "initializing main.scm: in setup\n"))

(define (draw)
  (igm/maximized ("gui repl")
	     (ig/text "hi there"))
  )
