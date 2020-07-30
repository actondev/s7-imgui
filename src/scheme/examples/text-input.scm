(ns-require aod.imgui.macros :as igm)
(ns-require aod.c.imgui :as ig)
(ns-require aod.c.foreign :as c)
(ns-require aod.components.input :as input)
(ns-require aod.c.imgui.col :as igc)
(ns-require aod.imgui.helpers :as igh)


;; the exposed functions
(define (setup)
  (display "initializing main.scm: in setup\n")
  (ig/set-color igc/FrameBg (igh/frgb->u32 '(0.3 0.3 0.3)))
  )

(define str-size 128)
(define *str (c/new-char[] str-size))
(define *str2 (c/new-char[] str-size))

(define form '(a b c))

(define input-data (input/new '(a b c)))

(define (draw)
  (igm/maximized
   ("gui repl")
   (ig/text-input "Gimme some text 2" *str2 str-size)
   (ig/text "input component")
   (when (input/draw input-data)
     (let ((form (input-data 'value)))
       (print "changed form to " (input-data 'value))
       (when (and (pair? form)
		  (eq? 'eval (car form)))
	 (set! (input-data 'value) (eval (cadr form)))
	 )))))

