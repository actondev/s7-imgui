(ns aod.fs
    :doc "Accessing the filesystem"
    :require ((aod.c.string :as string)))

(comment
 (string/search "Exec=exo-open --launch WebBrowser
Name=Web Browser" "Exec=(.+)\nName=(.+)")

  (string/search _2 "Exec=(.+)\nName=(.+)")
  (string/)
  (ns-doc 'aod.c.string)
  )

(comment
 (define _1 (system "ls -1 /usr/share/applications/*.desktop" #t))
 (ns-doc 'aod.c.string)
 (string/search-global _1 "(.+.desktop)\n"
		       (lambda ()
			 (format #f "~A" (string/match-at 1))))

 (define _2 (system "grep -1 /usr/share/applications/*.desktop" #t))

 
 (string/match-count)
 (string/match-at 1)
 )

(define* (list-applications (path "/usr/share" ))
  (let* ((out (system
	       (format #f "ls -1 ~A~A" path "/applications/*.desktop")
	       ;;"ls -1 /usr/share/applications/*.desktop"
	       #t))
	 (parsed (string/search-global out "(.+.desktop)\n"
		       (lambda ()
			 (format #f "~A" (string/match-at 1))))))
    (filter let? (map parse-dot-desktop (filter string? parsed)))))

(define (list-applications-all)
  (let ((dirs (-data-dirs)))
    (apply append (map list-applications dirs))
    )
  )

(comment
 (length (list-applications-all))
 (-data-dirs)
 (list-applications "/usr/share/xubuntu")
 (list-applications-all)
 (time (list-applications))
 )
(define (parse-dot-desktop file)
  (let* ((grep-res (system (format #f "grep -e Name= -e Exec= ~A " file) #t))
	(regex "Exec=(.+)\nName=(.+)")
	(exec (if (string/search grep-res "Exec=(.+)")
		  (string/match-at 1)
		  #f))
	(name (if (string/search grep-res "Name=(.+)")
		  (string/match-at 1)
		  #f)))
    (if (and exec name)
	(inlet :name name
	       :exec exec)
	(begin
	  (format #t "no matches for ~A grep res ~A" file grep-res)
	  #f))))

(define (-data-dirs)
  (string/search-global (getenv "XDG_DATA_DIRS") "([^:]+):"
			       (lambda ()
				 ;;(print "matches" (string/match-count))
				 (string/match-at 1))))

(comment
 (-data-dirs)
 )

(comment "data dirs"
	 (define dirs (getenv "XDG_DATA_DIRS"))
	 (string/search-global dirs "([^:]+):"
			       (lambda ()
				 (print "matches" (string/match-count))
				 (string/match-at 1)))
	 )
(comment
 (parse-dot-desktop "/usr/share/applications/exo-web-browser.desktop")
 (define _2 (system (format #f "grep -e Name= -e Exec= ~A " "/usr/share/applications/exo-web-browser.desktop") #t))

 (string/search "Exec=exo-open --launch WebBrowser %u
Name=Web Browser" "Exec=(.+)Name=(.+)")

 (ns-doc 'aod.c.string)
 )
