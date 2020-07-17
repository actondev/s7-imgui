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
(define *nss* (make-hash-table))
(define ns-curlet-postfix "/*ns*")
(define (ns-get-or-create the-ns)
  (unless (*nss* the-ns)
    (set! (*nss* the-ns) (unlet)))
  (*nss* the-ns))

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
  (print "here, require global " the-ns " blah " (symbol->string the-ns))
  ;; creating the ns
  (ns-get-or-create the-ns)
  (let ((ns-curlet-symbol (string->symbol
			   (string-append (symbol->string the-ns) ns-curlet-postfix))))
    (if (or
	 (equal? #<undefined> ((rootlet) (string->symbol
					  (string-append (symbol->string the-ns) ns-curlet-postfix))))
	 force)
	(begin
	  (apply varlet (*nss* the-ns)
		 ;; hmm.. unlet?
		 (with-let (unlet)
			   ;; (let ())
			   (print "here, curlet " (curlet))
			   (#_load (*autoload* the-ns) (*nss* the-ns))
			   (print "loaded " the-ns " and curlet is " (curlet))
			   ;; (print "let list " (let->list (curlet)))
			   (let->list (curlet))
			   ))
	  ;;(print "here (*nss* the-ns) " (*nss* the-ns))
	  )
	(begin
	  (print "Skipping global require for " the-ns))))
  (print "required globally? ns is " (*nss* the-ns))
  )

(define* (ns-require-ns-as the-ns as (target-env (outlet (curlet))))
  (print "here require ns " the-ns " as " as "in target-env " target-env)
  (apply varlet target-env
  		   (with-let (unlet)
  			     (map (lambda (binding)
  				    (let ((binding-symbol (string->symbol
  							   (string-append (symbol->string as) "/" (symbol->string (car binding))))))
  				      (if (not (ns-should-bind-locally? (car binding)))
  					  (values)
  					  (begin
  					    (print "in ns " target-env " binding " binding "car" (car binding) " as " binding-symbol "cdr procedure? " (cdr binding))
  					    (cons binding-symbol
  						  (if (procedure? (cdr binding))
  						      ;; TODO: do the macro only when *develop* is #t
  						      ;; on "production" we don't want this, it has some performance impact I imagine!
						      
  						      (macro args
							`(begin
							   ;; (print "calling " ',binding-symbol "which calls" ',(car binding) " in env " (*nss* ',the-ns))

							   (with-let (*nss* ',the-ns)
								     ;; (print "function is " (symbol->value ',(car binding)))
								     ;; (print "documentation is " (documentation
								     ;; 				 (symbol->value ',(car binding))))
								    ((symbol->value ',(car binding))
								     ,@args)
								    ))
  							)
  						      (begin
  							(print "not a procedure, just binding it?")
  							(cdr binding)))
  						  )))))
  				  (*nss* the-ns))
  			     ))
  )

(define-macro* (ns-require the-ns (as #f) (force #f) )
  (print "- ns-require: " the-ns " as " as "in " *ns* )
  (let ((current-ns *ns*))
    `(begin
      ;; (print " ... ns require outlet " (outlet (curlet)))
      (ns-require-global ',the-ns :force ,force)
      (with-let ,current-ns
      	      (ns-require-ns-as ',the-ns
				 (or ',as ',the-ns)
				 :target-env ,current-ns))
      (print "finished require, *ns* is  " *ns*)
      (set! *ns* ,current-ns)
      (print "reverted *ns*" *ns*))))

(define (ns-load file . args)
  (#_load file *ns*))

(define (ns-make-empty-let)
  (with-let (unlet)
	    (let ()
	      (curlet))))


(define-macro (ns the-ns . body)
  (print "===> ns: defining ns " the-ns)
  ;; defining the top level namespace, curlet
  ;; accessible from <ns>/*ns*
  ;; or should I prefer /*ns* ???

  ;; (define *ns* ,current-ns)
  ;; (print "here")
  ;; (define *ns-name* ',the-ns)
    
  `(begin
     (set! *ns* (ns-get-or-create ',the-ns))
     (with-let *ns*
	       (define *ns-name* ',the-ns))
     )
  ;; now, the definitions will be parsed
  ;; (define *ns* ,current-ns)
  ;; (print "here, after ns body, curlet " (curlet))
  ;;(print "finished ns with loads/requires " ',the-ns "curlet " (curlet))
  )

(define-macro (with-ns the-ns . body)
  `(with-let (*nss* ',the-ns)
	    ,@body))

(comment

 (with-let user/*curlet*
	   (load "ns-test2.scm" (curlet)))
 
 )
