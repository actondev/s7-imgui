(define-macro (example/set-screenshot-and-exit filename)
  ;; TODO gensym for frame?
  `(begin
     (define frame 0)
     (define (post-draw)
       (format *stderr* "--- running post draw\n")
       ;; on the first call there wasn't.. enough time? to show it on screen
       ;; the screenshot is black, but if i'm on debug (breakpoint) indeed i can see something and the screenshot is not blank
       ;; so.. 1 frame delay to have actually shown something in the screen
       ;; and then.. on first draw, there is just the menu for some reason, no windows
       ;; so, we take a screenshot on the third frame
       (set! frame (+ 1 frame))
       (when (= frame 3)
	 (gl/save-screenshot (format #f "scheme-examples/~A" ,filename))
	 (exit))))
  )
