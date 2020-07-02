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

(define (draw)
  (imgui/m-begin ("s7 window")
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
		       color-4))

  (when (window1-open)
    (imgui/m-begin ("s7 closeable" window1-open)
		   (imgui/text "I like scheme.")
		   (imgui/text "I am a closable window")))
  ;; done drawing
  )

