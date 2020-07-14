(display "loading aod/core\n")
;; putting the autload info here, among other necessary things (that I use often)
(provide 'aod.core)

(autoload 'aod.clj "aod/clj.scm")
(autoload 'aod.test "aod/test.scm")
(autoload 'aod.geom "aod/geom.scm")
;; comment, map-indexed, dotimes, range, mod
;; on the (rootlet)
(require aod.clj)
;; ignornig tests, unless:
(define-expansion (test . body) #<unspecified>)

(when (eq? (symbol->value '*test*) #t)
  (display "Preparing tests:\n")
  (require aod.test))


(define-macro* (aod/require what (as #f))
  (let* ((prefix (symbol->string `,(or as what)))
	(features-symbol (string->symbol (string-append prefix "/*features*"))))
    `(if (defined? ',features-symbol)
	(format *stderr* "WARNING: ~A already required as ~A\n" ',what ,prefix)
	;; else, doing the bidings:
	(if (defined? ',what)
	    ;; bindings from c
	    (apply varlet (curlet)
		   (map (lambda (binding)
			  (let ((binding-symbol (string->symbol 
						 (string-append ,prefix "/" (symbol->string (car binding))))))
			    ;; (format *stderr* "binding from c ~A\n" binding-symbol)
			    (cons binding-symbol 
				  (cdr binding))))
			,what))
	     ;; normal autload, symbol "what" not present
	    (apply varlet (curlet)
		   (with-let (unlet)
			     (let ()
			       ;; note: we use load cause if we required already nothing will happen!
			       ;; (*autoload* ',what) gives us the file name
			       (load (*autoload* ',what) (curlet))
			       (map (lambda (binding)
				      (let ((binding-symbol (string->symbol 
							     (string-append ,prefix "/" (symbol->string (car binding))))))
					;; (format *stderr* "binding from autoload ~A\n" binding-symbol)
					(cons binding-symbol 
					      (cdr binding))))
				    (curlet)))))))))

(autoload 'aod.layout "aod/layout.scm")
(autoload 'imgui-macros.scm
	  ;; fuck, the lambda is not working
	  ;; aaaagggh
	  
	  ;; (lambda (e)
	  ;; (display "WARNING! please use aod.imgui.macros")
	  ;;   (unless (provided? 'imgui-macros)
	  ;;     (load "aod/imgui_macros.scm")))
	  "aod/imgui_macros.scm"
	  )
(autoload 'aod.imgui.macros "aod/imgui/macros.scm")
(autoload 'aod.colors "aod/colors.scm")

(define (filter pred col)
  (let loop ((res (list ))
	   (s col))
    (if (pair? s)
	(begin
	  (when (pred (car s))
	      (set! res (append res (list (car s)))))
	  (loop res (cdr s)))
	res)))

(comment
 (filter (lambda (x)
	   (> x 0))
	 '( 0 1 2 -1 -5 10))
 ;; => (1 2 10)
 )


(define (print . args)
  (format *stderr* "~A\n" (apply string-append
		  (map
		   (lambda (x)
		     (format #f "~A " x)
		     )
		   args))))

(comment
 (print 'a 'b "aasa" '(a b c))
 )
