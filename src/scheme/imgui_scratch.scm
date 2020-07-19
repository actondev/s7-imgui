(ns imgui-sratch)

(ns-require aod.c.imgui-sdl :as igsdl)
(ns-require aod.c.imgui :as ig)
(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.gl :as gl) ;; for screenshots
(ns-require aod.layout :as l)
(ns-require aod.sxs :as sxs)
(ns-require aod.imgui.helpers :as igh)

(define *ctx* (igsdl/setup 420 420))

(define-macro (redefine-and name body then)
  (let ((is-defined (defined? name)))
    `(begin
       (define ,name ,body)
       (when ,is-defined
	 ,then))))
(define color (ig/frgb->u32 1 1 1))

(define* (sxs-element cx cy (phase 0) (n 0))
  ;; we multiply phase with N/3 (N=12) since we want to repeat every 3
  (let ((lines (sxs/lines `(,cx ,cy 30) :phase (* 4 phase))))
    ;; side effect
    ;; (ig/draw-circle `(,cx ,cy 20 ,color))
    ;;(ns-doc 'aod.c.imgui 'draw-text)
    (ig/draw-text cx cy (format #f "~A" n) color)
    (igh/draw-lines-with-color lines color)
    (apply ig/draw-circle `(,cx ,cy 35 ,color))
    )
  )
;; upon redefining do-draw funcion
;; the (draw) will get called
(redefine-and do-draw
  (lambda ()
    (igm/maximized
     ("imgui scratch")
     ;; (ig/text "hi you handsome devil")
     (l/circular sxs-element :N 12 :center '(200 190) :R 150)
     ))
  (draw))

(define (draw)
  (igsdl/prepare *ctx*)
  (do-draw)
  (igsdl/flush *ctx*)
  )

(draw)


;; (igsdl/destroy *ctx*)
;; (exit)

(comment
 (igsdl/destroy *ctx*)
 (exit)
 (defined? 'watch)
 (define *ctx* (igsdl/setup 400 400))
 (draw)
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
