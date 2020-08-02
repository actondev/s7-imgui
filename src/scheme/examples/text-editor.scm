 ;; loading this into rootlet so that from c side I can direclty call
 ;; (draw) etc
(ns (rootlet)
    :require ((aod.c.foreign :as c)
	      (aod.c.imgui :as ig)
	      (aod.imgui.macros :as igm)
	      (aod.c.nfd)
	      (aod.c.imgui.window-flags :as igw)))

(define (setup)
  ()
  )

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

(define (open-file)
  ;; returns the opened file contents
  (let ((file (aod.c.nfd/open)))
    (when file
      (call-with-input-file file
	(lambda (in)
	  (call-with-output-string
	   (lambda (out)
	     (catch 'wrong-type-arg ; s7 raises this error if write-char gets #<eof>
		    (lambda () 
		      (do () ()	; read/write until #<eof>
			(write-char (read-char in) out)))
		    (lambda err 
		      out)))))))))

(define (save contents)
  (let ((file (aod.c.nfd/save)))
    (call-with-output-file file
      (lambda (out)
	(call-with-input-string
	 contents
	 (lambda (in)
	   (catch 'wrong-type-arg ; s7 raises this error if write-char gets #<eof>
		  (lambda () 
		    (do () ()		; read/write until #<eof>
		      (write-char (read-char in) out)))
		  (lambda err ()))))))))

(define (draw-menu)
  (igm/menu-bar
   ()
   (igm/menu
    ("File")
    (igm/menu-item ("Open")
		   (print "Clicked open")
		   (set! (*file-contents*) (open-file)))
    (igm/menu-item ("Save")
		   (print "Clicked save")
		   (save (*file-contents*))))))

(define (draw)
  (igm/maximized
   ("s7 texxt editor" igw/MenuBar)
   (draw-menu)
   (ig/input-text-multiline "##text-input" *buffer buffer-size) 
   ;; 
   ))
