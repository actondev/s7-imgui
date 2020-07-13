(display "loading aod/core\n")
;; putting the autload info here, among other necessary things (that I use often)
(provide 'aod.core)

(autoload 'aod.clj "aod/clj.scm")
;; comment, map-indexed, dotimes, range, mod
;; on the (rootlet)
(require aod.clj)

(autoload 'aod.layout "aod/layout.scm")

(autoload 'imgui-macros.scm
	  ;; fuck, the lambda is not working
	  ;; aaaagggh
	  
	  ;; (lambda (e)
	  ;;   (unless (provided? 'imgui-macros)
	  ;;     (load "aod/imgui_macros.scm")))
	  "aod/imgui_macros.scm"
	  )

(autoload 'aod.imgui.macros "aod/imgui/macros.scm")

(define-macro* (aod/require what (as #f))
  (let ((prefix (symbol->string `,(or as what))))
    ;; (format *stderr* "aod/require ~A :as ~A\n" what prefix)
    (if (defined? what)
	;; bindings from c
	(begin
	  ;; (format *stderr* "requiring foreign bindings ~A as ~A\n" what prefix)
	  `(apply varlet (curlet)
		  (map (lambda (binding)
		  (let ((binding-symbol (string->symbol 
				      (string-append ,prefix "/" (symbol->string (car binding))))))
		    ;; (format *stderr* "binding ~A\n" binding-symbol)
		    (cons binding-symbol 
			  (cdr binding))))
		,what)))
	;; normal autload, symbol "what" not present
	(begin
	  ;; (format *stderr* "requiring autoload symbol ~A as ~A, features ~A\n autoload ~A\n" what prefix *features* (*autoload* what))
	  (if (defined? (string->symbol (string-append prefix "/*features*")))
	    (format *stderr* "WARNING: ~A already required as ~A\n" what prefix)
	    `(apply varlet (curlet)
		    (with-let (unlet)
			      (let ()
				;; note: we use load cause if we required already nothing will happen!
				;; (*autoload* ',what) gives us the file name
				(load (*autoload* ',what) (curlet))
				(map (lambda (binding)
				       (let ((binding-symbol (string->symbol 
							   (string-append ,prefix "/" (symbol->string (car binding))))))
					 ;; (format *stderr* "binding ~A\n" binding-symbol)
					 (cons binding-symbol 
					       (cdr binding))))
				     (curlet))))))))))

(display "loaded aod/core\n")
