;; A visualization/logo that I came up with
;; to be used for imgui drawing directly
;;
;; Also, the idea is to create components that have some tests of
;; their rendering.  Input state => output a png image. I can then
;; compare the images

(ns aod.components.sxs-wheel)

(ns-require aod.c.imgui :as ig)
(ns-require aod.imgui.macros :as igm)
(ns-require aod.layout :as l)
(ns-require aod.sxs :as sxs)
(ns-require aod.imgui.helpers :as igh)
(ns-require aod.colors :as colors)

(define* (mk-sxs-element (r 34) (r-internal 30))
  (lambda* (cx cy (phase 0) (n 0))
	   (let ((lines (sxs/lines `(,cx ,cy ,r-internal) :phase (* 4 phase)))
		 (color (apply ig/frgb->u32 (colors/ryb->rgb (colors/triplet-phase phase)))))
	     (igh/draw-lines-with-color lines color)
	     (apply ig/draw-circle `(,cx ,cy ,r ,color)))))

;; hm.. mk-state vs make vs new ??
(define* (new (N 12) (R 150) (r 34) (internal-fill 0.9))
  (let ((element (mk-sxs-element :r r :r-internal (* internal-fill r))))
    (curlet)))

(define (draw state)
  (let* ((R (state 'R))
	 (center (+ R (state 'r)))
	 (element (state 'element))
	 (N (state 'N)))
    ;; TODO.. not maximized
    (igm/maximized
     ("###")
     (l/circular element :N N :center (list center center) :R R :gui #t))))

(testgui "SXS Color Wheel snapshot"
	 (ns-require aod.c.imgui-sdl :as igsdl)
	 (ns-require aod.c.gl :as gl)
	 (ns-require aod.c.sdl :as sdl)
	 (ns-require aod.c.img :as c.img)
	 (let* ((R 180)
		(r 40)
		(size (* 2 (+ R r))))
	   (define test-element (new :R R :r r))
	   ;; the size should be 2*(R+r)
	   ;; hm have to add 15.. I guess there is a padding of 7
	   (define *ctx* (igsdl/setup (+ 14 size) (+ 14 size)))
	   (igsdl/prepare *ctx*)
	   
	   (draw test-element)
	   (igsdl/flush *ctx*)
	   (sdl/delay 20)
	   (gl/save-screenshot "test/scheme/assets/sxs-wheel.png")
	   (igsdl/destroy *ctx*)
	   (is (c.img/equivalent? "test/scheme/assets/sxs-wheel.png"
				  "test/scheme/assets/sxs-wheel-snapshot.png"))
	   ))


(comment
 ;; drawing it
  (igsdl/destroy *ctx*)
  (exit)

 
 (ns-require aod.c.gl :as gl)
 (define test-element (new))
 (define *ctx* (igsdl/setup 420 420))
 (igsdl/prepare *ctx*)
 (draw test-element)
 (igsdl/flush *ctx*)
 (gl/save-screenshot "sxs-color-wheel.png")

 
 )

