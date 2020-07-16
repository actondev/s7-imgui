(display "loading aod/core\n")
;; putting the autload info here, among other necessary things (that I use often)
(provide 'aod.core)

(load "aod/autoloads.scm")
;; comment, map-indexed, dotimes, range, mod
;; on the (rootlet)
(require aod.clj)
(require aod.ns)
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
					 ;; maybe skip the ones starting with dash - ? they're "private"
					 ;; (format *stderr* "binding from autoload ~A\n" binding-symbol)
					 ;; (print " binding symbol " binding-symbol " with value " ((curlet) (car binding)))
					 (cons binding-symbol 
					       (cdr binding))))
				     (curlet)))))))))

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


;; WIP: trying to make it so that
;; let's say im requiring aod.geom :as geom
;; I want to change the definition of some function
;; (with-let aod.geom/*curlet* (define (mk-circle ..) ..))
;; and then in the namespace that i required it, when i run
;; geom/mk-circle , I should see the reflecting changes
(define-macro* (aod/require-ns ns (force #f))
  (let* ((features-symbol (string->symbol (string-append (symbol->string ns) "/*ns*"))))
    `(if (and (defined? ',features-symbol) (not ,force))
	 (format *stderr* "WARNING: ns ~A already loaded~%" ',ns)
	 ;; else, load the namespace
	 ;; loading the namespace means:
	 ;; creating top level macros with the full forms of the ns functions
	 ;; eg foo.bar/fun
	 ;; the tricky part: instead of creating this symbol, it has to be a macro...?
	 (apply varlet (rootlet)
		(with-let (unlet)
			  (let ()
			    ;; note: we use load cause if we required already nothing will happen!
			    ;; (*autoload* ',what) gives us the file name
			    (load (*autoload* ',ns) (curlet))
			    (print "loaded " ',ns " and curlet is " (curlet) )
			    (print "echo is " ((curlet) 'echo))
			    (define *ns* ',ns) ;; Note: that should be defined by the (ns ) macro
			    (define *curlet* (curlet))
			    ;; (with-let (rootlet)
			    ;; 	       (define ,(string-append (symbol->string ',ns) "/*curlet*" )))


			    ;; apend...
			    '(list (cons (string->symbol
					  (string-append (symbol->string ',ns) "/*curlet*"))
					 (curlet)))
			    (map (lambda (binding)
				   (let ((binding-symbol (string->symbol
							  (string-append (symbol->string ',ns) "/" (symbol->string (car binding))))))
				     ;; maybe skip the ones startign with dash - ? they're "private"
				     (format *stderr* "binding from autoload ~A\n" binding-symbol)
				     (print " binding symbol " binding-symbol " with value " ((curlet) (car binding)))
				     (cons binding-symbol
					   ;; (cdr binding)
					   (if (procedure? (cdr binding))
					       ;; TODO: do the macro only when *develop* is #t
					       ;; on "production" we don't want this, it has some performance impact I imagine!
					       (macro args
						 `((,*curlet* ',(car binding))
						   ,@args)
						 )
					       (cdr binding))
					   )))
				 (curlet))
			    ))))))

(comment
 (aod/require-ns aod.geom :force #t)
 aod.geom/*curlet*
 aod.geom/*ns*
 (aod.geom/echo)

 (define gecho aod.geom/echo)
 ;; (gecho)
 
 (aod/require-ns aod.geom)

 ;; aod.geom/*ns*

 (with-let aod.geom/*curlet*
	   (define (echo) (print "geom echo modified 3!")))

 (with-let aod.geom/*curlet*
	   (define (mk-circle) (print "hi! i'm a circle")))

 ;; this reflects the changes
 ((aod.geom/*curlet* 'echo))
 ;; this not
 (aod.geom/echo)

 (procedure-source aod.geom/echo)
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

 (let->list (curlet))
 ((curlet) (string->symbol "lines"))
 geom/echo
 )
