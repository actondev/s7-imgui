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
(define (make-file-contents *char)
  ;; dilambda is a quick way to define getters and setters
  (dilambda
   ;; getter
   (lambda ()
     (*char))
   (lambda (v)
     (if (< buffer-size (length v))
	 (print "text bigger than buffer-size!")
	 (begin
	   (set! (*char) v))))))

(define *file-contents* (make-file-contents *buffer))

(set! (*file-contents*) "S7 text editor")

(define (open)
  (let ((file (aod.c.nfd/open)))
    (when file
      (set! (*file-contents*) (io/get-file-contents file)))))

(define (save)
  (let ((file (aod.c.nfd/save)))
    (when file
      (io/put-file-contents file (*file-contents*)))))

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
