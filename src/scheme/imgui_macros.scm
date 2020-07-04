(require clj.scm) ;; the (comment) macro is there
(define-macro (imgui/m-safe . body)
  `(catch #t
	   (lambda ()
	     ,@body)
	   (lambda args
	     (apply format #t (cadr args)))))

(define-macro (imgui/m-window args . body)
  `(begin
     (imgui/begin ,@args)
     (imgui/m-safe ,@body)
     (imgui/end)))

;; the top bar, full window, menu
(define-macro (imgui/m-main-menu-bar args . body)
  ;; note: begin-main-menu doesn't take any args,
  ;; but for the sake of consistency we keep this calling format
  ;; (imgui/m-some-macro args . body)
  ;; where args are applied to that first call
  `(begin
     (imgui/begin-main-menu-bar)
     ;; ,@body
     (imgui/m-safe ,@body)
     (imgui/end-main-menu-bar)
     ))

;; a menu (eg File)
(define-macro (imgui/m-menu args . body)
  `(when (imgui/begin-menu ,@args)
     ;; ,@body
     (imgui/m-safe ,@body)
     (imgui/end-menu)))

(define-macro (imgui/m-menu-item args . body)
  `(when (imgui/menu-item ,@args)
     ,@body))

(define-macro* (imgui/m-begin2 (title "") (*open #t) :rest body)
  (if (eq? #t *open)
      `(begin
	 (imgui/begin ,title)
	 ,@body
	 (imgui/end))
      `(begin
	 (imgui/begin ,title ,*open)
	 ,@body
	 (imgui/end))))

(comment "window (begin etc)"
 (macroexpand (imgui/m-window ("title")
			     (imgui/text "hi")
			     (imgui/text "scheme s7"))
			     )
 ;; =>
 (begin (imgui/begin "title") (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))


 (macroexpand (imgui/m-window ("test" 'the-c-object)
			     (imgui/text "hi")
			     (imgui/text "scheme s7")
			     ))
 ;; =>
 (begin (imgui/begin "test" 'the-c-object) (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))


 (macroexpand (imgui/m-window2 :title "always open"
			     (imgui/text "hi")
			     (imgui/text "scheme s7")
			     ))
 ;; =>
 (begin (imgui/begin "always open" (imgui/text "hi")) (imgui/text "scheme s7") (imgui/end))

 (macroexpand (imgui/m-window2 :title "always open"
			      :*open 'the-c-object
			     (imgui/text "hi")
			     (imgui/text "scheme s7")
			     ))
 ;; =>
 (begin (imgui/begin "always open" 'the-c-object) (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))
 
 )

(comment ;; menus
 (macroexpand
  (imgui/m-main-menu-bar
   (imgui/m-menu ("File")
		 (imgui/menu-item "Open")
		 )))
 ;; ! menus
 )
;; layout
(define-macro (imgui/m-horizontal . body)
  (let ((with-same-line-prepended (map
				   (lambda (el)
				     `(begin
					(imgui/same-line)
					,el))
				    (cdr body))))
    `(begin
       ,(car body)
       ,@with-same-line-prepended))
  )
(comment
 (macroexpand (imgui/m-horizontal
	       (imgui/text "text 1")
	       (imgui/text "text 2")
	       (imgui/text "text 3")))
 ;; =>
 (begin (imgui/text "text 1") (begin (imgui/same-line) (imgui/text "text 2")) (begin (imgui/same-line) (imgui/text "text 3")))

 )

(define-macro (imgui/m-horizontal-old . body)
  ;; (display "hi, body is")
  ;; (display body)
  (let ((with-same-line-prepended (map-indexed
				   (lambda (i sexp)
				     (if (eq? i 0)
					 sexp
					 `(begin
					    (imgui/same-line)
					    ,sexp))
				     )
				   body)))
    `(begin
       ,@with-same-line-prepended))
  )

(comment
 (defined? 'imgui/same-line)
 (macroexpand (imgui/m-horizontal-old
	       (imgui/text "text 1")
	       (imgui/text "text 2")
	       (imgui/text "text 3")))
 ;; =>
 (begin (imgui/text "text 1") (begin (imgui/same-line) (imgui/text "text 2")) (begin (imgui/same-line) (imgui/text "text 3")))

 )
