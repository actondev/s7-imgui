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

(define* (mk-sxs-element highlights (bg '(0 0 0)) (r 34) (r-internal 30))
  (lambda* (cx cy (phase 0) (n 0))
	   (let ((lines (sxs/lines `(,cx ,cy ,r-internal) :phase (* 4 phase)))
		 (highlighted? (highlights n))
		 (rgb (colors/ryb->rgb (colors/triplet-phase phase)))
		 (color32-bg (igh/frgb->u32 bg))
		 )
	     (let ((color32-normal-alpha (igh/frgb->u32 (append rgb '(0.3))));; alpha
		   (color32-normal (igh/frgb->u32 rgb))
		   )
	       (igh/draw-circle `(,cx ,cy ,r)
				:color (if highlighted?
					   color32-normal-alpha
					   color32-bg
					   )
				:thickness 1
				:filled #t)
	       (igh/draw-circle `(,cx ,cy ,r)
				:color color32-normal
				:thickness 1
				:segments 32)
	       (igh/draw-lines lines
			       :color color32-normal
			       :thickness (if highlighted? 2
					      1))))))

;; hm.. mk-state vs make vs new ??
;; TODO calculate optima r if only R is passed
(define* (new (N 12) (R 150) (r 30) (internal-fill 0.8))
  (let* ((highlights (make-vector N #f))
	 (element (mk-sxs-element
		  :highlights highlights
		  :r r :r-internal (* internal-fill r))))
    (curlet)))

(define (draw state)
  (let* ((R (state 'R))
	 (center (+ R (state 'r)))
	 (element (state 'element))
	 (N (state 'N)))
    ;; TODO.. not maximized
    (igm/maximized
     ("###")
     '(igh/draw-circle `( 100 100 100)
		      :filled #t)
     (l/circular element :N N :center (list center center) :R R :gui #t))))

(define (set-highlight state index value)
  (set! ((state 'highlights) index) value))

(comment
 (with-let (rootlet)
	   (provide 'aod.test.gui)
	   (require aod.test))
 
 (set! (hook-functions (aod.c.repl '*eval-hook*))
      (cons (lambda (hook)
	      (igsdl/prepare *ctx*)
	      (draw test-element)
	      (igsdl/flush *ctx*)
	      )
	    (hook-functions (aod.c.repl '*eval-hook*))))
 )

(testgui "SXS Color Wheel snapshot"
	 (ns-require aod.c.imgui-sdl :as igsdl)
	 (ns-require aod.c.gl :as gl)
	 (ns-require aod.c.sdl :as sdl)
	 (ns-require aod.c.img :as c.img)
	 (define R 180)
	 (define r 35)
	 (define size (* 2 (+ R r)))
	 (define test-element (new :R R :r r))
	 ;; the size should be 2*(R+r)
	 ;; hm have to add 14.. I guess there is a padding of 7

	 (define *ctx* (igsdl/setup (+ 14 size) (+ 14 size)))

	 (define (paint)
	   (igsdl/prepare *ctx*)
	   (draw test-element)
	   (igsdl/flush *ctx*))
	 ;; commenting out while developing :)
	 (begin
	   (paint)
	   (sdl/delay 30)
	   (gl/save-screenshot "test/scheme/assets/sxs-wheel.png")
	   (set-highlight test-element 0 #t)
	   (set-highlight test-element 4 #t)
	   (set-highlight test-element 8 #t)
	   (paint)
	   (sdl/delay 30)
	   (gl/save-screenshot "test/scheme/assets/sxs-wheel-highlight-048.png")
	   (igsdl/destroy *ctx*)
	   (is (c.img/equivalent? "test/scheme/assets/sxs-wheel.png"
	   			  "test/scheme/assets/sxs-wheel-snapshot.png"))
	   (is (c.img/equivalent? "test/scheme/assets/sxs-wheel-highlight-048.png"
	   			  "test/scheme/assets/sxs-wheel-highlight-048-snapshot.png"))

	   ;; test the it's not always true :)
	   (is (not (c.img/equivalent? "test/scheme/assets/sxs-wheel-snaphost.png"
				       "test/scheme/assets/sxs-wheel-offset.png"))))
	 
	 )


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

