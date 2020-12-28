(ns rootlet
    :require ((aod.c.foreign :as c)
	      (aod.c.imgui :as ig)
	      (aod.imgui.macros :as igm)
	      (aod.c.nfd)
	      (aod.io :as io)
	      (aod.fs) ;; TODO rename
	      (aod.c.string :as string)
	      (aod.io :as io)
	      (aod.c.imgui.window-flags :as igw)
	      (aod.c.imgui.keys :as igk)
	      (aod.c.wm :as wm)))

(define windows (wm/list-windows))
;;(define applications (aod.fs/list-applications-all))

(define (get from . args)
;;  (print "gettin from " from "args" args)
  (if (null? args)
      (error 'get "Could not get any property from ~A" from)
      (if (keyword? (car args))
	  (let ((res (from (car args))))
	    (if (undefined? res)
		(apply get from (cdr args))
		res))
	  ;; else, we consider it a fallback value
	  (car args)
	  )))

(comment
 (get (inlet :a 1 :b 2) :c :a)
 )

(define applications
  (catch #t
	 (lambda ()
	   (eval-string (io/slurp "apps.cache.scm"))
	   )
	 (lambda (tag info)
	   (format *stderr* "Error: ~A\n" tag)
	   ;; info is a list usually
	   ;; car: the formatting string eg "~A is not a list but ~A"
	   ;; and then the arguments for the formatting
	   (apply format *stderr* info)
	   ;; bubbling up the error

	   (let ((applications (aod.fs/list-applications-all)))
	     (io/spit "apps.cache.scm" (format #f "~W" applications))
	     applications)
	   )))

(define entries (append windows applications))
(define filtered-entries entries)

;;(print "all apps " (length applications))

(define buffer-size 512)
(define *str (c/new-char[] buffer-size))

(define (filter-windows windows string)
  (filter (lambda (w)
	    (or
	     (string/search
	      (format #f "~A ~A" (w 'class-name) (w 'title))
	      ;; spaces are meant to be wildcards :)
	      (string/replace string " " ".*")
			    #t ;; ignore-case
			    )))
	  windows))

(define (filter-apps apps string)
  (filter (lambda (app)
	    (or
	     (string/search
	      (app 'name)
	      ;; spaces are meant to be wildcards :)
	      (string/replace string " " ".*")
			    #t ;; ignore-case
			    )))
	  apps))

(define (format-window window)
  (let ((len 30))
    (format #f "~A~NT~A" 
	    (object->string (window 'class-name) #f len)
	    len
	    (object->string (window 'title) #f len))))

(define sel-idx 0)

(define (trigger entry)
  (when-let (handle (get entry :handle #f))
	    (raise-and-focus entry))
  (when-let (exec (get entry :exec #f))
	    (system (format #f "setsid ~A &" exec)))
  (exit))

(comment
 (cond (() 1)
       (#t 2))
 )
(define (raise-and-focus w)
  (print "raising " w)
  (wm/raise-window (w 'handle))
  (wm/focus-window (w 'handle))
  (exit)
  )
(define (format-entry-type entry)
  (or (and (get entry :handle #f) "win")
      (and (get entry :exec #f) "app")
      (error 'invalid-arg "Could not handle entry ~A" entry)
      ))
(comment
 (format-entry-type (inlet :exec 1))
 )

(define (draw-entries entries)
  (let ((idx 0)
	(enter? (ig/key-pressed? igk/Enter)))
       ;; (ig/begin-group)
       (ig/columns 3 "results")
       (ig/set-column-width 0 0.1)
       (ig/set-column-width 1 0.3)
       (ig/set-column-width 2 0.6)
       (for-each (lambda (entry)
		   (ig/text (format-entry-type entry))
		   (ig/next-column)
		   (ig/selectable
		    ;; TODO (entry 'title) returns #<undefind> if it's not there
		    ;; NOT nil () :(
		    (get entry :class-name :name)
				  (= idx sel-idx))
		   (ig/next-column)
		   (ig/text
		    (get entry :title :exec))
		   (ig/next-column)
		   (when (and (ig/is-item-hovered)
			      (ig/mouse-moved?))
		     (set! sel-idx idx))
		   (when (and (= idx sel-idx)
			      (ig/is-item-hovered)
			      (ig/mouse-double-clicked? 0)
			      )
		     (trigger entry))
		   (when (and enter?
			      (= idx sel-idx))
		     (trigger entry))
		   (set! idx (inc idx)))
		 entries)
       (ig/columns 1)
       ;; (ig/end-group)
       ))

(define draw-count 0)

(define (draw)
  (igm/maximized
   ("s7 window switcher")
   (when (ig/key-pressed? igk/Escape)
     (if (equivalent? "" (*str))
	 (exit)
	 (set! (*str) "")))
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
       (set! filtered-entries
	     (append
	      (filter-windows windows (*str))
	      (filter-apps applications (*str))))
       (set! sel-idx 0))
     ;; when setting the focus to the input, the selectables later on
     ;; won't return true
     (ig/set-keyboard-focus-here)
     (draw-entries filtered-entries)
     )
   ;; /maximized
   )
  ;; benchmarking
  (when (= draw-count 3)
      ()
      ;;(exit)
      )
  (set! draw-count (inc draw-count))
  )
