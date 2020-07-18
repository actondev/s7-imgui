(ns imgui-sratch)

(ns-require aod.c.imgui-sdl :as igsdl)
(ns-require aod.c.imgui :as ig)
(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.gl :as gl) ;; for screenshots
(ns-require aod.layout :as l)

(define *ctx* (igsdl/setup 400 400))

(define-macro (redefine-and name body then)
  (let ((is-defined (defined? name)))
    `(begin
       (define ,name ,body)
       (when ,is-defined
	 ,then))))

;; upon redefining do-draw funcion
;; the (draw) will get called
(redefine-and do-draw
  (lambda ()
    (igm/maximized
     ("imgui scratch")
     (ig/text "hi you devil")
     (ig/text "")))
  (draw))

(define (draw)
  (igsdl/prepare *ctx*)
  (do-draw)
  (igsdl/flush *ctx*)
  )

(draw)

(comment
 (igsdl/destroy *ctx*)
 (defined? 'watch)
 (define *ctx* (igsdl/setup 400 400))
 (draw)
 (gl/save-screenshot "test.png")
 
 ;; documentation
 (ns-doc 'aod.c.gl)
 (ns-doc 'aod.c.imgui)
 (ns-doc 'aod.c.imgui-sdl)
 (ns-doc 'aod.layout)

 (keys *nss*)
 (hash-table-entries *nss*)
 )
