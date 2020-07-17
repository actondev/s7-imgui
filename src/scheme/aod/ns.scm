;; clojure-like namespaces
;; usage:
;; (ns foo.bar)
;; (ns-require some.lib :as lib)
;; (ns-require some.lib.other :as other)
;;
;; 
;; TODO
;; maybe try the more clojure-like form
;; (ns foo.bar
;;   (require some.lib :as lib)
;;   (require some.lib.other :as other))
;;
(require aod.clj) ;; for (comment .. )
(provide 'aod.ns)

;; holds the current namespace
;; the repl running on the c-side shoudl read this to eval incoming expressions in there
(define *ns* (rootlet))

;; can be thought either of plurar (namespaces)
;; or ns store..
(define *nss* (make-hash-table))
(define (ns-make-empty-let)
  (with-let (unlet)
	    (let ()
	      (curlet))))
(define-macro (ns-create the-ns)
  (format *stderr* "Creating namespace ~A\n" the-ns)
  (set! (*nss* the-ns) (ns-make-empty-let))
  `(with-let ,(*nss* the-ns)
	    (define *ns-name* ',the-ns)
	    ))
(define (ns-get-or-create the-ns)
  (unless (*nss* the-ns)
    ;; note: ns-create is a macro, gotta call it with apply
    (apply ns-create (list the-ns)))
  (*nss* the-ns))

(define (ns-should-bind-globally? symbol)
  (let ((first-char ((symbol->string symbol) 0)))
    (not (equal? #\- first-char))))
(define (ns-should-bind-locally? symbol)
  (let* ((symbol-string (symbol->string symbol))
	(first-char (symbol-string 0)))
    (and (not (equal? #\- first-char))
	 (not (equal? #\* first-char))
	 ;; sub namespaces ??
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
  ;; (print "here, require global " the-ns " blah " (symbol->string the-ns))
  ;; creating the ns
  (ns-get-or-create the-ns)
  ;; TODO not require/load again, unless force #t
  (begin
    (apply varlet (*nss* the-ns)
	   ;; hmm.. unlet?
	   (with-let (unlet)
		     (let ()
		       ;; (print "here, curlet " (curlet))
		       (#_load (*autoload* the-ns) (*nss* the-ns))
		       ;; (print "loaded " the-ns " and curlet is " (curlet))
		       ;; (print "let list " (let->list (curlet)))
		       (let->list (curlet)))
		     ))
    ;;(print "here (*nss* the-ns) " (*nss* the-ns))
    )
  (begin
    (print "Skipping global require for " the-ns))
  ;; (print "required globally? ns is " (*nss* the-ns))
  )

(define* (ns-require-ns-as the-ns as (target-env (outlet (curlet))))
  ;; (print "here require ns " the-ns " as " as "in target-env " target-env)
  (apply varlet target-env
	 (with-let (unlet)
		   (let ()
		     ;; hmm.. here I still have acces to the-ns, as, etc
		     (map (lambda (binding)
			    (let ((binding-symbol (string->symbol
						   (string-append (symbol->string as) "/" (symbol->string (car binding))))))
			      (if (not (ns-should-bind-locally? (car binding)))
				  (values)
				  (begin
				    ;; (print "in ns " target-env " binding " binding "car" (car binding) " as " binding-symbol "cdr procedure? " (cdr binding))
				    ;; (print "binding " binding-symbol)
				    (cons binding-symbol
					  (if (procedure? (cdr binding))
					      (let ((ns-internal-target the-ns)
						    (ns-internal-func (car binding)))
						(lambda args
						  (begin
						    ;; (print "target-env is " ns-internal-target)
						    (eval `(apply ,ns-internal-func ',args) (*nss* ns-internal-target))
							      ;; (print "documentation is " (documentation
							      ;; 				 (symbol->value ',(car binding))))
							      ;; (eval `(,(car binding) ,@args))
							      
							      )
						  ))
					      (begin
						(print "not a procedure, just binding it?")
						(cdr binding)))
					  )))))
			  (*nss* the-ns)))
  			     ))
  )

(define-macro* (ns-require the-ns (as #f) (force #f) )
  ;; (print "- ns-require: " the-ns " as " as "in " *ns* )
  (let ((current-ns *ns*))
    `(begin
      ;; (print " ... ns require outlet " (outlet (curlet)))
      (ns-require-global ',the-ns :force ,force)
      (with-let ,current-ns
      	      (ns-require-ns-as ',the-ns
				 (or ',as ',the-ns)
				 :target-env ,current-ns))
      ;; (print "finished require, *ns* is  " *ns*)
      (set! *ns* ,current-ns)
      ;; (print "reverted *ns*" *ns*)
      )))

(define (ns-load file . args)
  (#_load file *ns*))


(define-macro (ns the-ns . body)
  `(begin
     (set! *ns* (ns-get-or-create ',the-ns))
     ',the-ns)
  )

(define-macro (with-ns the-ns . body)
  `(with-let (*nss* ',the-ns)
	    ,@body))
