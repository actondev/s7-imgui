(require clj.scm) ;; the (comment) macro is there
(define-macro (imgui/m-begin args . body)
  `(begin
     (imgui/begin ,@args)
     ,@body
     (imgui/end)))

(comment "examples"
 (macroexpand (imgui/m-begin ("title")
			     (imgui/text "hi")
			     (imgui/text "scheme s7"))
			     )
 ;; =>
 (begin (imgui/begin "title") (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))


 (macroexpand (imgui/m-begin ("test" 'the-c-object)
			     (imgui/text "hi")
			     (imgui/text "scheme s7")
			     ))
 ;; =>
 (begin (imgui/begin "test" 'the-c-object) (imgui/text "hi") (imgui/text "scheme s7") (imgui/end))
 
 )

