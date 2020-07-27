(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.imgui :as ig)
(ns-require aod.c.foreign :as c)

;; the exposed functions
(define (setup)
  (display "initializing main.scm: in setup\n"))

(define str-size 128)
(define *str (c/new-char[] str-size))

(define (draw)
  (igm/maximized
   ("gui repl")
   (ig/button "hi there")
   (ig/text-input "Gimme some text" *str str-size))
  )

(comment
 )
