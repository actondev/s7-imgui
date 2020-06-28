(display "loading imgui.scm\n")

;; (require libc.scm)
;; (require r7rs.scm)

(define (setup)
  
  (display "initializing imgui.scm: in setup\n"))

;; (define (box-inc! b)
;;   (display "box-inc!\n")
;;   (set-box! b (+ 1 (unbox b)))
;;   )

(define click-counter 0)
;; (define counter-box (box 0))

(define (draw)
  (aod.imgui/begin "s7 window")
  (aod.imgui/text "I like scheme :)")

  (if (aod.imgui/button (format #f "Click ~A times" click-counter))
      (begin
	(set! click-counter (+ 1 click-counter))
	(format *stdout* "new counter ~A\n" click-counter)))

  ;; (if (aod.imgui/button (format #f "Click box ~A times" (unbox counter-box)))
  ;;     (begin
  ;; 	(box-inc! counter-box)))

  (aod.imgui/text "another one")
  (aod.imgui/end)
  ;; done drawing
  )
