(ns imgui-sratch)

(ns-require aod.c.imgui-sdl :as igsdl)
(ns-require aod.c.imgui :as ig)
(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.gl :as gl) ;; for screenshots
(ns-require aod.layout :as l)
(ns-require aod.sxs :as sxs)
(ns-require aod.imgui.helpers :as igh)
(ns-require aod.colors :as colors)

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

(define color (igh/frgb->u32 '(1 1 1)))
(define R 150)
(begin
  (define r 34)
  (define r-internal (* 0.9 r)))

(define N 12)

(define* (sxs-element cx cy (phase 0) (n 0))
  ;; we multiply phase with 4 cause we want the sigma logo to repeat 4 times
  ;; during the whole circle
  (let ((lines (sxs/lines `(,cx ,cy ,r-internal) :phase (* 4 phase)))
	(color (igh/frgb->u32 (colors/ryb->rgb (colors/triplet-phase phase)))))
    ;; (ig/draw-text cx cy (format #f "~A" n) color)
    (igh/draw-lines lines color)
    (apply ig/draw-circle `(,cx ,cy ,r ,color))
    ;; 
    )
  )
;; upon redefining do-draw funcion
;; the (draw) will get called
(define (do-draw)
  (print "Drawing!!!!!")
  (igm/maximized
   ("imgui scratch")
   (l/circular sxs-element :N N :center '(200 190) :R R :gui #t)
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
