(ns rootlet
    :require ((aod.c.foreign :as c)
	      (aod.c.imgui :as ig)
	      (aod.imgui.macros :as igm)
	      (aod.c.nfd)
	      (aod.c.string :as string)
	      (aod.io :as io)
	      (aod.c.imgui.window-flags :as igw)
	      (aod.c.imgui.keys :as igk)
	      (aod.c.wm :as wm)))


(define windows (wm/list-windows))

(define buffer-size 512)
(define *str (c/new-char[] buffer-size))

(define (filter-windows windows string)
  (filter (lambda (x)
	    (string/search (x 'title)
			   string))
	  windows))

(define sel-idx 0)

(define (raise-and-focus w)
  (print "raising " w)
  (wm/raise-window (w 'window))
  (wm/focus-window (w 'window))
  (exit)
  )

(define (draw)
  (igm/maximized
   ("s7 window switcher")
   (when (ig/key-pressed? igk/Escape)
     (exit))
   (when (ig/key-pressed? igk/DownArrow #t)
     (print "down pressed!")
     (set! sel-idx (inc sel-idx)))
   (when (ig/key-pressed? igk/UpArrow #t)
     (print "up pressed!")
     (set! sel-idx (dec sel-idx)))
   (let ((enter? (ig/key-pressed? igk/Enter)))
     ;; input
     ;; there's (was.. as default) flag to return true on enter. by default it returns true
     ;; on text input
     (when (ig/input-text "search:" *str buffer-size)
       (set! sel-idx 0))
     ;; when setting the focus to the input, the selectables later on
     ;; won't return true
     (ig/set-keyboard-focus-here)
     (let ((idx 0))
       (ig/begin-group)
       (for-each (lambda (w)
		   (ig/selectable (w 'title)
				  (= idx sel-idx))
		   (when (and (ig/is-item-hovered)
			      (ig/mouse-moved?))
		     (set! sel-idx idx))
		   (when (and (= idx sel-idx)
			      (ig/is-item-hovered)
			      (ig/mouse-double-clicked? 0)
			      )
		     (raise-and-focus w))
		   (when (and enter?
			      (= idx sel-idx))
		     (raise-and-focus w))
		   (set! idx (inc idx)))
		 (filter-windows windows (*str)))
       (ig/end-group)))
   ;; /maximized
   ))
