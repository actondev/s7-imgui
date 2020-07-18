;; clojure-like namespaces
;; usage:
;; (ns foo.bar)
;; (ns-require some.lib :as lib)
;; (ns-require some.lib.other :as other)
;;
;; 
;; Wanted to make it more clojure-like style:
;; (ns foo.bar
;;   (require some.lib :as lib)
;;   (require some.lib.other :as other))
;;
;; but proved more difficult (and didn't know what I was doing at the
;; time) so switched back to the simpler form.  And I think it's
;; better to stick with this. Easier to maintain/adjust I think.
;;
;;

(require aod.clj) ;; for (comment .. )
(provide 'aod.ns)

;; Holding all the namespaces, to be able to switch.
;; 
;; mnemonic: NameSpaceS
(define *nss* (make-hash-table))

;; Setting to #t when loading a file (ns-load-file "foo/bar.scm")
;;
;; We use it to "merge" with any (ns..) declaration hacky, but it
;; works for now See more comments in the (ns-load-file)
(define *ns-load-mode* #f)
(define (ns-make-empty-let)
  (with-let (unlet)
	    (let ()
	      (curlet))))

;; Holds the current namespace.
;; 
;; The repl running on the c-side should read this to eval incoming
;; expressions in there
(define *ns* (ns-make-empty-let))

(define-macro (ns-create the-ns)
  ;; should check about the *ns-load-mode*
  ;; (format *stderr* "Creating namespace ~A\n" the-ns)
  (if *ns-load-mode*
      (begin
	(print "we're in load mode")
	(set! (*nss* the-ns) *ns*)
	(set! *ns-load-mode* #f))
      (set! (*nss* the-ns) (ns-make-empty-let)))
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

;; Loads the namespace in its own environment
;; and puts it in the *nss* hash table, under the 'the-ns key
(define* (ns-load the-ns (force #f))
  (if (or (eq? #f (*nss* the-ns))
	  force)
      (begin
	(ns-get-or-create the-ns)
	(if (and (defined? the-ns)
		 (let? (symbol->value the-ns)))
	    (begin
	      ;; (print the-ns "is already defined globally. Probably c bindings")
	      (set! (*nss* the-ns) (symbol->value the-ns))
	      )
	    ;; else: loading a file with autoload info
	    ;; TODO.. check if autoload info exists? if not it should error
	    (apply varlet (*nss* the-ns)
		   (with-let (unlet)
			     (let ()
			       (#_load (*autoload* the-ns) (*nss* the-ns))
			       (let->list (curlet)))))))
      (begin
	(print "Skipping already ns-require'd" the-ns)
	)))

;; Creates the dynamic bindings in the target-env
;; 
;; eg
;; (ns my.ns)
;; (ns-require foo.bar :as b)
;;
;; That creates the bindings b/fun inside my.ns
;; that call ((*nss* 'foo.bar) 'fun) and so on
;;
;; This is what enables the redifinitions to work!
;; If i'm in the foo.bar namespace and change the definition
;; of fun, this will be reflected inside my.ns
(define* (ns-require-alias the-ns as (target-env (outlet (curlet))))
  (apply varlet target-env
	 (with-let (unlet)
		   (let ()
		     ;; hmm.. here I still have acces to the-ns, as, etc.
		     ;; good, but not clear why
		     (map (lambda (binding)
			    (let ((binding-symbol (string->symbol
						   (string-append (symbol->string as) "/" (symbol->string (car binding))))))
			      (if (not (ns-should-bind-locally? (car binding)))
				  (values)
				  (begin
				    ;; (print "binding " binding-symbol)
				    (cons binding-symbol
					  (if (procedure? (cdr binding))
					      (let ((ns-internal-target the-ns)
						    (ns-internal-func (car binding)))
						;; TODO copy the documentatoin of the forwarding function?
						;; Could be useful in the future if something like geiser support
						;; is added, to show the documentation of the symbol under cursor
						(lambda args
						  (eval `(apply ,ns-internal-func ',args) (*nss* ns-internal-target))
						  ))
					      (begin
						;; (print "not a procedure, just binding it?")
						(cdr binding)))
					  )))))
			  (*nss* the-ns))))))

(define-macro* (ns-require the-ns (as #f) (force #f) )
  (let ((current-ns *ns*))
    `(begin
      (ns-load ',the-ns :force ,force)
      (with-let ,current-ns
      	      (ns-require-alias ',the-ns
				 (or ',as ',the-ns)
				 :target-env ,current-ns))
      (set! *ns* ,current-ns))))

;; Loads a file and if it has a (ns) definition,
;; correctly loads it there.
;;
;; The proces:
;; - loading the file in a new empty environment
;; - setting ns-load-mode to #t
;; 
;; this makes the (ns ..) form to not create a new environmnet
;; but use the existing *ns*. it also immediately unsets this flag
(define (ns-load-file file . args)
  (set! *ns-load-mode* #t)
  (set! *ns* (ns-make-empty-let))
  (load file *ns*))

;; maybe I should make this a normal function
;; keep things simpler..
(define-macro (ns the-ns . body)
  `(begin
     (set! *ns* (ns-get-or-create ',the-ns))
     ',the-ns)
  )

(define-macro (with-ns the-ns . body)
  `(with-let (*nss* ',the-ns)
	    ,@body))

(define-macro (with-temp-ns . body)
  (let ((ns-symbol (gensym "temp-ns"))
	(previous-ns (gensym "*ns-temp*")))
    `(begin
       (set! (*nss* ',previous-ns) *ns*)
       (ns ',ns-symbol)
       (catch #t
	      (lambda ()
		,@body)
	      (lambda args
		;; hm cleaning up again
		(print "with-temp-ns cleanup in catch")
		(set! *ns* (*nss* ',previous-ns))
		(set! (*nss* ',previous-ns) #f)
		(apply throw args)))
       (set! *ns* (*nss* ',previous-ns))
       (set! (*nss* ',previous-ns) #f))))
(comment
 (require aod.test)
 )

(test "Temp ns"
      (with-temp-ns
       (define x 1)
       (is (defined? 'x))
       (is (= x 1)))

      (with-temp-ns
       (is (not (defined? 'x))))
      )

(test "with-temp-ns cleanup"
      (define caught-something #f)
      (catch #t
	     (lambda ()
	       (with-temp-ns
		(define x 1)
		(is (defined? 'x))
		i-am-going-to-fail
		))
	     (lambda args
	       (set! caught-something #t)))
      (is (eq? #t caught-something))
      (is (not (defined? 'x)))
      )

