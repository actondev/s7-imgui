(ns aod.imgui.macros
    :doc "Some macros to make life easier while working with ImGui.
The usual syntax is (args . body)
- args are applied to corresponding raw imgui function
- body is executed either in a when block (eg when a menu items is active)
  or wrapped between the begin/end calls"
    :require
    ((aod.c.imgui :as ig)))

(define-macro (-safe . body)
  `(catch #t
	   (lambda ()
	     ,@body)
	   (lambda (tag info)
	     (format *stderr* "Exception occured inside ImGui body: ~A~%" tag)
	     (apply format *stderr* info)
	     (newline))))
(define window
  (let ((+documentation+ "(window args . body)
applies args to imgui/begin, executes body and calls imgui/end"))
    (macro (args . body)
      `(begin
	 (,ig/begin ,@args)
	 (,-safe ,@body)
	 (,ig/end)))))

(define-macro (maximized args . body)
  `(begin
     (,ig/begin-maximized ,@args)
     (,-safe ,@body)
     (,ig/end)))

(define-macro (child args . body)
  `(begin
     (,ig/begin-child ,@args)
     (,-safe ,@body)
     (,ig/end-child)))

(define-macro (group args . body)
  `(begin
     (,ig/begin-group ,@args)
     (,-safe ,@body)
     (,ig/end-group)))

;; the top bar, full window, menu
(define-macro (main-menu-bar args . body)
  ;; note: begin-main-menu doesn't take any args,
  ;; but for the sake of consistency we keep this calling format
  ;; (some-macro args . body)
  ;; where args are applied to that first call
  `(begin
     (,ig/begin-main-menu-bar)
     ;; ,@body
     (,-safe ,@body)
     (,ig/end-main-menu-bar)
     ))

(define-macro (menu-bar args . body)
   `(begin
     (,ig/begin-menu-bar)
      (,-safe ,@body)
     (,ig/end-menu-bar)
     ))

;; a menu (eg File)
(define-macro (menu args . body)
  `(when (,ig/begin-menu ,@args)
     ;; ,@body
     (,-safe ,@body)
     (,ig/end-menu)))

(define-macro (menu-item args . body)
  `(when (,ig/menu-item ,@args)
     ,@body))

(comment "window (begin etc)"
 (macroexpand (window ("title")
			     (imgui/text "hi")
			     (imgui/text "scheme s7"))
			     )
 ;; =>
 (begin (imgui/begin "title") (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))


 (macroexpand (window ("test" 'the-c-object)
			     (imgui/text "hi")
			     (imgui/text "scheme s7")
			     ))
 ;; =>
 (begin (imgui/begin "test" 'the-c-object) (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))


 (macroexpand (window2 :title "always open"
			     (imgui/text "hi")
			     (imgui/text "scheme s7")
			     ))
 ;; =>
 (begin (imgui/begin "always open" (imgui/text "hi")) (imgui/text "scheme s7") (imgui/end))

 (macroexpand (window2 :title "always open"
			      :*open 'the-c-object
			     (imgui/text "hi")
			     (imgui/text "scheme s7")
			     ))
 ;; =>
 (begin (imgui/begin "always open" 'the-c-object) (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))
 
 )

(comment ;; menus
 (macroexpand
  (main-menu-bar
   (menu ("File")
		 (imgui/menu-item "Open")
		 )))
 ;; ! menus
 )
;; layout
(define horizontal
  (let ((+documentation+ "(horizontal . body)
executes first element of body and then inserts any next element with the same-line called before"))
    (macro body
      (let ((with-same-line-prepended (map
				       (lambda (el)
					 `(begin
					    (,ig/same-line)
					    ,el))
				       (cdr body))))
	`(begin
	   ,(car body)
	   ,@with-same-line-prepended)))))
(comment
 (macroexpand (horizontal
	       (imgui/text "text 1")
	       (imgui/text "text 2")
	       (imgui/text "text 3")))
 ;; =>
 (begin (imgui/text "text 1") (begin (imgui/same-line) (imgui/text "text 2")) (begin (imgui/same-line) (imgui/text "text 3")))

 )

(comment
 (defined? 'imgui/same-line)
 (macroexpand (horizontal-old
	       (imgui/text "text 1")
	       (imgui/text "text 2")
	       (imgui/text "text 3")))
 ;; =>
 (begin (imgui/text "text 1") (begin (imgui/same-line) (imgui/text "text 2")) (begin (imgui/same-line) (imgui/text "text 3")))

 )
