;; (ns imgui-sratch)

(ns-require 'aod.c.imgui-sdl :as 'igsdl)
(ns-require 'aod.c.imgui :as 'ig)
(ns-require 'aod.imgui.macros :as 'igm)
(ns-require 'aod.c.gl :as gl) ;; for 'screenshots
(ns-require 'aod.layout :as 'l)
(ns-require 'aod.imgui.helpers :as 'igh)
(ns-require 'aod.colors :as 'colors)
(ns-require 'aod.components.piano-wheel :as 'piano)
(ns-require 'aod.components.sxs-wheel :as 'sxs)

(define *ctx* (igsdl/setup 420 420))

(set! (hook-functions (aod.c.repl '*eval-hook*))
      (cons (lambda (hook)
	      (if (eq? 'ns (car (hook 'form)))
		  (begin
		    ;;(print "just an ns form" (hook 'form))
		    )
		  (begin
		    (draw))
		  )
	      )
	    (hook-functions (aod.c.repl '*eval-hook*))))

(comment
 (set! (hook-functions (aod.c.repl '*eval-hook*)) ())
 )

(define sxs (sxs/new :R 150))
(define piano (piano/new :R 80 :cx 180 :cy 180))

;; upon redefining do-draw funcion
;; the (draw) will get called
(define (do-draw)
  (print "Drawing!!!!!")
  (igm/maximized
   ("imgui scratch")
   (piano/draw piano)
   (sxs/draw sxs)
   ))

(define (draw)
  (igsdl/prepare *ctx*)
  (do-draw)
  (igsdl/flush *ctx*)
  )

(draw)

(comment
 (igsdl/destroy *ctx*)
 (exit)
 
 (gl/save-screenshot "test.png")
 
 ;; documentation
 (ns-doc 'aod.c.gl)
 (ns-doc 'aod.c.imgui)
 (ns-doc 'aod.c.imgui-sdl)
 (ns-doc 'aod.layout)
 (ns-doc 'aod.sxs)
 (ns-doc 'aod.imgui.helpers)

 (keys *nss*)
 (hash-table-entries *nss*)
 )
