 ;; loading this into rootlet so that from c side I can direclty call
 ;; (draw) etc
(ns rootlet
    :require ((aod.c.foreign :as c)
	      (aod.c.imgui :as ig)
	      (aod.imgui.macros :as igm)
	      (aod.c.nfd)
	      (aod.io :as io)
	      (aod.c.imgui.window-flags :as igw)))

(define buffer-size 2048)
(define *buffer (c/new-char[] buffer-size))

(define (open)
  (and-let* ((file (aod.c.nfd/open))
	     (contents (io/slurp file)))
	    (if (< buffer-size (length contents))
		(print "text bigger than buffer-size!")
		(set! (*buffer) contents))))

(define (save)
  (if-let* ((file (aod.c.nfd/save)))
	   (io/spit file (*buffer))
	   (print "User cancelled!")))

(define (draw-menu)
  (igm/menu-bar
   ()
   (igm/menu
    ("File")
    (igm/menu-item ("Open")
		   (print "Clicked open")
		   (open))
    (igm/menu-item ("Save")
		   (print "Clicked save")
		   (save)))))

(define (draw)
  (igm/maximized
   ("s7 texxt editor" igw/MenuBar)
   (draw-menu)
   (ig/input-text-multiline "##text-input" *buffer buffer-size) 
   ;; 
   ))

