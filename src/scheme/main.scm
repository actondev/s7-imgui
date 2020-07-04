(display "loading main.scm\n")

;; either one of the 2
;; (load "imgui_macros.scm")
(require imgui-macros.scm)

;; for the require to worK:
;; main.cpp:
;; 
;; static const char *autoloads[2] = {
;;     "imgui-macros", "imgui_macros.scm", /* each pair of entries is entity name + file name */
;; };
;; s7_autoload_set_names(sc, autoloads, 1);


(define click-counter 0)
(define *win-open ((*foreign* 'new-bool) #t))
(format #t "win open is ~A\n" (*win-open))
(set! (*win-open) #f)
(format #t "win open is ~A\n" (*win-open))
;; (define b ((*foreign* 'new-bool) #t))

;; the exposed functions
(define (setup)
  (display "initializing main.scm: in setup\n"))

(define color-4 ((*foreign* 'new-float[]) 4))
;; that should not crash the program, there should be checks
;; (define color-4 ((*foreign* 'new-bool[]) 4))

(define (adjust-color!)
  (when (defined? 'imgui/clear-color)
    (let ((new-val (imgui/clear-color 0)))
      (if (>= new-val 1)
	  (set! new-val 0))
      (set! (imgui/clear-color 0) (+ 0.01 new-val)))
    )
  )

(define (draw-menu-raw)
  (imgui/begin-main-menu-bar)
  (when (imgui/begin-menu "File")
    (when (imgui/menu-item "Open")
      (format #t "Open clicked\n"))
    (imgui/menu-item "Save")
    (imgui/end-menu))
  (imgui/end-main-menu-bar)
  )

(define (draw-menu)
  (imgui/m-main-menu-bar
   ()
   (imgui/m-menu
    ("File")
    (imgui/m-menu-item
     ("Inc counter!")
     (begin
       ;; note that I can pass multiple statements in the macro
       ;; but for clarity it's nice to group them
       (format #t "inc counter clicked\n")
       (set! click-counter (+ 1 click-counter)))))
   (imgui/m-menu
    ("Settings")
    (imgui/m-menu-item ("Load settings")
		       (begin
			 (format #t "Load settings clicked\n"))))))

(define (draw-window-always-on)
  (imgui/m-window
   ("s7 window")
   (if (imgui/button (format #f "Click ~A times" click-counter))
       (begin
	 (set! click-counter (+ 1 click-counter))
	 (format *stdout* "new counter ~A\n" click-counter)))
   (when (not (*win-open))
     (when (imgui/button "Open the closeable window")
	  (set! (*win-open) #t)))
   (imgui/text "another one")
   (imgui/checkbox "show that other window" *win-open)
   (imgui/color-edit-3 "Here's a color"
		       color-4)))

(define (draw-color-3)
  (imgui/begin "color window")
  (imgui/text "colors:")
  (display "here 1\n")
  (imgui/color-edit-3 "Here's a color"
		      color-4)
  (display "here 2\n")
  (imgui/end))

(define (draw-window-closeable)
  (when (*win-open)
    (imgui/m-window ("s7 closeable" *win-open)
		   (imgui/text "I like scheme.")
		   (imgui/text "I am a closable window"))))

(define *knob-value ((*foreign* 'new-float) 0.5))

(define (draw-knobs)
  (imgui/m-window
   ("knobs")
   (imgui/text "here come the knobs")
   (do ((i 0 (+ i 1)))
       ((= i 3))
     (imgui/knob (format #f "knob ~A" i) *knob-value 0 1)
     )
   (imgui/text "here come the horizontal knobs")
   ;; that works
   '(apply
    imgui/m-horizontal
    `((imgui/knob (format #f "knob 1") ,*knob-value 0 1)
      (imgui/knob (format #f "knob 1") ,*knob-value 0 1))
    )
   (apply
    imgui/m-horizontal
    (map
     (lambda (i)
       (list
	 imgui/knob (format #f "knob ~A" i) *knob-value 0 1)
       )
     '(a b c)))))

(define (draw)
  (draw-menu)
  (draw-window-always-on)
  ;; (draw-color-3)
  (draw-window-closeable)
  (draw-knobs)
  )

