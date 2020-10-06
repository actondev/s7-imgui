(ns aod.components.input
    :require ((aod.c.foreign :as c)
	      (aod.imgui.macros :as igm)))

(define char-size 128)

(define* (new (init ()))
  (let ((*str (c/new-char[] char-size))
	(value init)
	(editing? #f))
    (set! (*str) (format #f "~A" value))
    (curlet)))

(define (draw state)
  (let ((ret #f))
    (if (not (state 'editing?))
	(begin
	  (let ((text (format #f "~A" (state 'value))))
	    (igm/horizontal
	     (begin
	       (ig/align-text-to-frame-padding)
	       (ig/text text))
	     (when (ig/button "Edit")
	       (set! (state 'editing?) #t)
	       (set! ((state '*str)) text)
	       ))))
	(begin
	  (ig/set-keyboard-focus-here)
	  ;; TODO store an id in state?
	  (ig/input-text "##text-input" (state '*str) char-size)
	  (cond ((ig/is-item-deactivated-after-edit)
		 (with-input-from-string ((state '*str))
		   (lambda ()
		     (set! (state 'value) (read))
		     (set! (state 'editing?) #f)
		     (set! ret #t))))
		((ig/is-item-deactivated)
		 (set! (state 'editing?) #f)))))
    ret))
