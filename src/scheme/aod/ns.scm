;; clojure-like namespaces
;; usage:
;; (ns foo.bar
;;  (require some.lib :as lib)
;;  (require some.lib.other :as other)
;; )
;; Inside the ns form the require is replace by the  ns-require macro
;; This
;; loads in the rootlet the some.lib/*curlet* and some.lib.other/*curlet*
(require aod.clj)

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
	 (not (char-position #\/ symbol-string)))))


(comment
 (ns-should-bind-globally? '*curlet*)
 (ns-should-bind-globally? '-private)
 (equal? #\- ("-asdsa" 0))
 )


;; requires the namespace in the rootlet
;; fully quialified methods, defined as macros => can dynamically reload
(define* (ns-require-global the-ns (force #f))
  (if (or
       (equal? #<undefined> ((rootlet) (string->symbol
					(string-append (symbol->string the-ns) "/*ns*"))))
       force)
      (begin
	;; (print "--- ns-require-global (loading) " the-ns)
	(apply varlet (rootlet)
	       (with-let (unlet)
			 (let ()
			   (load (*autoload* the-ns) (curlet))
			   (print "loaded " the-ns " and curlet is " (curlet))
			   ;; (print "echo is " ((curlet) 'echo))
			   ;; (define *ns* ',the-ns) ;; Note: that should be defined by the (ns ) macro
			   (define *ns* the-ns)
			   (define *curlet* (curlet))
			   (map (lambda (binding)
				  (if (not (ns-should-bind-globally? (car binding)))
				      (values)
				    (let ((binding-symbol (string->symbol
							   (string-append (symbol->string the-ns) "/" (symbol->string (car binding))))))
				      ;; maybe skip the ones startign with dash - ? they're "private"
				      (print " binding globally " binding-symbol)
				      (cons binding-symbol
					    (if (procedure? (cdr binding))
						;; TODO: do the macro only when *develop* is #t
						;; on "production" we don't want this, it has some performance impact I imagine!
						(macro args
						  `((,*curlet* ',(car binding))
						    ,@args)
						  )
						(begin
						  ;; (print "not a procedure, just binding it?")
						  (cdr binding)))
					    ))))
				(curlet))
			   ))))
      (begin
	(print "Skipping global require for " the-ns)))
  )

(define* (ns-require-env-as env as (target-env (curlet)))
  (apply varlet target-env
		   (with-let (unlet)
			     (let ()
			       (map (lambda (binding)
				      (let ((binding-symbol (string->symbol
							     (string-append (symbol->string as) "/" (symbol->string (car binding))))))
					(if (not (ns-should-bind-locally? (car binding)))
					    (values)
					    (begin
					      (print " binding " binding-symbol)
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
     (when ',as
       (print "here, want to require " ',the-ns "as " ',as)
              (print "requiring as " ',as " the env  " ((rootlet) 'ns.bar/*curlet*))
	      (ns-require-env-as ((rootlet) (string->symbol
					     (string-append (symbol->string ',the-ns) "/*curlet*")))
				 ',as
				 :target-env (outlet (curlet)))
	 ))
  )


(define-macro (ns the-ns . body)
  (print "ns: defining ns " the-ns)
  `(let ((require  ns-require))
     ;; inlet: we replaced the require
     () ;; just in case there is no body
     ,@body
     ))
