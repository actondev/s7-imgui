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
(define window1-open ((*c-primitives* 'bool) #t))

;; the exposed functions
(define (setup)
  (display "initializing main.scm: in setup\n"))

(define color-4 ((*c-primitives* 'float-arr) 0.5 0.4 0.2))

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
  (imgui/m-window ("s7 window")
   (adjust-color!)

   (if (imgui/button (format #f "Click ~A times" click-counter))
       (begin
	 (set! click-counter (+ 1 click-counter))
	 (format *stdout* "new counter ~A\n" click-counter)))
   (when (not (window1-open))
     (when (imgui/button "Open the closeable window")
       (set! (window1-open) #t)))

   (imgui/text "another one")
   (imgui/checkbox "show that other window" window1-open)

   (imgui/color-edit-3 "Here's a color"
		       color-4)))

(define (draw-window-closeable)
  (when (window1-open)
    (imgui/m-window ("s7 closeable" window1-open)
		   (imgui/text "I like scheme.")
		   (imgui/text "I am a closable window"))))

(define *knob-value ((*c-primitives* 'float) 0.5))
;; (define window1-open ((*c-primitives* 'bool) #t))
(define (draw-knobs)
  (imgui/m-window
   ("knobs")
   (imgui/text "here come the knobs")
   (do ((i 0 (+ i 1)))
       ((= i 10))
     (imgui/knob (format #f "knob ~A" i) *knob-value 0 1)
     )

   ))

(define (draw)
  (draw-menu)
  (draw-window-always-on)
  (draw-window-closeable)
  (draw-knobs)
  )

