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
  (let ((ns-curlet-symbol (string->symbol
			   (string-append (symbol->string the-ns) "/*curlet*"))))
    (if (or
	 (equal? #<undefined> ((rootlet) (string->symbol
					  (string-append (symbol->string the-ns) "/*curlet*"))))
	 force)
	(begin
	  (print "--- ns-require-global (loading) " the-ns)
	  (varlet (rootlet)
		 (with-let (unlet)
			   (let ()
			     (load (*autoload* the-ns) (curlet))
			     (print "loaded " the-ns " and curlet is " (curlet))
			     (cons ns-curlet-symbol
				   (curlet))))))
	(begin
	  (print "Skipping global require for " the-ns))))
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
					      ;; (print "in ns " (target-env '*ns-name*) " binding " binding-symbol)
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
				    (string-append (symbol->string ',the-ns) "/*curlet*")))
			(or ',as ',the-ns)
			:target-env (outlet (curlet)))
     ;; (print "finished requiring, in " ',*ns-name*)
	 )
  )


(define-macro (ns the-ns . body)
  (print "ns: defining ns " the-ns)
  ;; (set! ((rootlet) '*ns*) (unlet))
  ;; (eval `(define *ns-name* ',the-ns) ((rootlet) '*ns*))
  `(begin
     (define *ns-name* ',the-ns)
     (let ((require ns-require))
       ()
       ,@body
       ;; (print "done require, in ns " ',*ns-name*)
       )))
