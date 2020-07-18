(ns imgui-sratch)

(ns-require aod.c.imgui-sdl :as igsdl)
(ns-require aod.c.imgui :as ig)
(ns-require aod.imgui.macros :as igm)

(define *ctx* (igsdl/setup 400 400))

(define-macro (define-with-watch name body watch-fn)
  `(begin
     (define ,name ,body)
     (set! (setter ',name)
	   (lambda (s v e)
	     (,watch-fn)
	      v))
     ))

;; (define (do-draw)
;;   (igm/maximized
;;    ("imgui scratch")
;;    (ig/text "hi there, you handsome devil"))
;;   )

(define-with-watch do-draw
  (lambda ()
    (igm/maximized
	      ("imgui scratch")
	      (ig/text "hi devil")
	      (ig/text "hi devil you")))
  (lambda ()
    (draw)
    ))

(define (draw)
  (igsdl/prepare *ctx*)
  (do-draw)
  (igsdl/flush *ctx*)
  )

(draw)

(comment
 (igsdl/destroy *ctx*)
 (defined? 'watch)
 (draw)
 )
