;; clojure-like namespaces
;; usage:
;; (ns foo.bar
;;  (require some.lib :as lib)
;;  (require some.lib.other :as other)
;; )
;; Inside the ns form the require is replace by the  ns-require macro
;; This
;; loads in the rootlet the some.lib/*curlet* and some.lib.other/*curlet*
(require aod.clj) ;; for (comment .. )
(define *ns* (rootlet))
(define ns-curlet-postfix "/*ns*")

(define (ns-should-bind-globally? symbol)
  (let ((first-char ((symbol->string symbol) 0)))
    (not (equal? #\- first-char))))

(define (ns-should-bind-locally? symbol)
  (let* ((symbol-string (symbol->string symbol))
	(first-char (symbol-string 0)))
    (and (not (equal? #\- first-char))
	 (not (equal? #\* first-char))
	 ;; sub namespaces
	 ;; (not (equal? #\\ first-char))
	 ;; (not (char-position #\/ symbol-string))
	 )))


(comment
 (ns-should-bind-globally? '*curlet*)
 (ns-should-bind-globally? '-private)
 (equal? #\- ("-asdsa" 0))
 )


;; requires the namespace in the rootlet
;; fully quialified methods, defined as macros => can dynamically reload
(define* (ns-require-global the-ns (force #f))
  (let ((ns-curlet-symbol (string->symbol
			   (string-append (symbol->string the-ns) ns-curlet-postfix))))
    (if (or
	 (equal? #<undefined> ((rootlet) (string->symbol
					  (string-append (symbol->string the-ns) ns-curlet-postfix))))
	 force)
	(begin
	  (print "--- ns-require-global (loading) " the-ns)
	  (varlet (rootlet)
		 (with-let (unlet)
			   (let ()
			     (#_load (*autoload* the-ns) (curlet))
			     (print "loaded " the-ns " and curlet is " (curlet))
			     (cons ns-curlet-symbol
				   (curlet))))))
	(begin
	  (print "Skipping global require for " the-ns))))
  )

(define* (ns-require-env-as env as (target-env (curlet)))
  (print "requiring env " env " as " as "target " target-env)
  (apply varlet target-env
		   (with-let (unlet)
			     (let ()
			       (map (lambda (binding)
				      (let ((binding-symbol (string->symbol
							     (string-append (symbol->string as) "/" (symbol->string (car binding))))))
					(if (not (ns-should-bind-locally? (car binding)))
					    (values)
					    (begin
					      (print "in ns " (target-env '*ns-name*) " binding " binding-symbol)
					      (cons binding-symbol
						    (if (procedure? (cdr binding))
							;; TODO: do the macro only when *develop* is #t
							;; on "production" we don't want this, it has some performance impact I imagine!
							(macro args
							  `((,env ',(car binding))
							    ,@args)
							  )
							(begin
							  ;; (print "not a procedure, just binding it?")
							  (cdr binding)))
						    )))))
				    env)
			       ))))

(define-macro* (ns-require the-ns (as #f) (force #f))
  (print "- ns-require: " the-ns " as " as)
  `(begin
     (ns-require-global ',the-ns :force ,force)
     (ns-require-env-as ((rootlet) (string->symbol
				    (string-append (symbol->string ',the-ns) ns-curlet-postfix)))
			(or ',as ',the-ns)
			:target-env *ns-target*)))

(define (ns-load file . args)
  (#_load file *ns*))

(define (ns-make-empty-let)
  (with-let (unlet)
	    (let ()
	      (curlet))))


(define-macro (ns the-ns . body)
  ;; (print "===> ns: defining ns " the-ns)
  ;; defining the top level namespace, curlet
  ;; accessible from <ns>/*ns*
  ;; or should I prefer /*ns* ???
  (let ((env-symbol (string->symbol
		     (string-append (symbol->string the-ns) ns-curlet-postfix))))
    (unless (defined? env-symbol)
      (print "Creating ns " the-ns)
      (eval `(define ,env-symbol
		 (ns-make-empty-let))
	      (rootlet)))
    (set! ((rootlet) '*ns*) (symbol->value env-symbol))
    (let ((current-ns ((rootlet) '*ns*)))
      `(begin
	 (with-let ((rootlet) '*ns*)
		   (define *ns-name* ',the-ns)
		   (let ((require ns-require)
			 (load ns-load)
			 (*ns-target* ,current-ns))
		     (map (lambda (sexp)
			    ;; (print "evaling sexp " sexp " ns before " ((rootlet) '*ns*))
			    (eval sexp (curlet))
			    ;; (print "executed " sexp "ns now " ((rootlet) '*ns*) " will set to " ,current-ns)
			    ;; reverting *ns* (changed by subsequent (ns ..) calls
			    (set! ((rootlet) '*ns*) ,current-ns)
			    )
			  ',body)
		     )
		   (print "finished ns with loads/requires " ',the-ns "curlet " (curlet)))))))

(comment

 (with-let user/*curlet*
	   (load "ns-test2.scm" (curlet)))
 
 )
