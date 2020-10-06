;; clojure-like namespaces
#|
usage:

(ns foo.bar)
(ns-require 'some.lib :as 'lib)
(ns-require 'some.lib.other :as 'other)

Or, more clojure like syntax

(ns foo.bar
  :doc "My foo.bar library"
  :require ((some.lib :as lib)
	    (other.lib :as other)))
|#
(require aod.clj) ;; for (comment .. )

;; Holding all the namespaces, to be able to switch.
;; 
;; mnemonic: NameSpaceS
(define *nss* (make-hash-table))
(define *ns-require-dynamic* #t)

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
(define *ns*
  ;; by default *ns* is the rootlet
  ;; this helps with (ns-load-file ) if there is no (ns) form
  ;; in that case the bindings will be loaded to rootlet
  ;;
  ;; Also, with the repl. when we start we should be in the (rootlet)
  (rootlet)
  )

(define (ns-create the-ns)
  ;; should check about the *ns-load-mode*
  ;; (format *stderr* "Creating namespace ~A\n" the-ns)
  (if *ns-load-mode*
      (begin
	;; (print "we're in load mode")
	(set! (*nss* the-ns) *ns*)
	(set! *ns-load-mode* #f))
      (set! (*nss* the-ns) (ns-make-empty-let)))
  ;; note: the-ns is not available in the with-let form
  ;; thus we use eval and then unquote ;)
  (eval `(with-let (*nss* the-ns)
		  (define *ns-name* ',the-ns))))

(define (ns-get-or-create the-ns)
  ;; can also do (ns (rootlet) :require ...)
  (cond ((eq? 'rootlet the-ns)
	 (rootlet))
	(else
	 (begin
	   (unless (*nss* the-ns)
	     (ns-create the-ns))
	   (*nss* the-ns)))))

(define (ns-should-bind-globally? symbol)
  (let ((first-char ((symbol->string symbol) 0)))
    (not (equal? #\- first-char))))

(define (ns-should-bind-locally? symbol)
  (let* ((symbol-string (symbol->string symbol))
	 (first-char (symbol-string 0)))
    (and (not (equal? #\- first-char))
	 (not (equal? #\* first-char))
	 ;; check if there is a slash already
	 ;; if so, it's a (ns-require) of another ns-require'd
	 (eq? #f (char-position #\/ symbol-string))
	 )))

(define (ns-should-doc? symbol)
  (let* ((symbol-string (symbol->string symbol))
	 (first-char (symbol-string 0)))
    (and (not (equal? #\- first-char))
	 ;; (not (equal? #* first-char))
	 (not (and (> (length symbol-string) 3)
		   (equal? "*ns-" (substring symbol-string 0 4))))
	 ;; check if there is a slash already
	 ;; if so, it's a (ns-require) of another ns-require'd
	 (eq? #f (char-position #\/ symbol-string))
	 )))

;; Loads a file and if it has a (ns) definition,
;; correctly loads it there.
;;
;; The proces:
;; - loading the file in a new empty environment
;; - setting ns-load-mode to #t
;; 
;; this makes the (ns ..) form to not create a new environmnet
;; but use the existing *ns*. it also immediately unsets this flag
(define (ns-load-file file)
  (set! *ns-load-mode* #t)
  (set! *ns* (ns-make-empty-let))
  (load file *ns*)
  ;; if there was no (ns .. ) form in the loaded file, we add the
  ;; bindings to the rootlet
  (with-let *ns*
	    (if (not (defined? '*ns-name*))
		(apply varlet (rootlet)
		       (let->list *ns*))
		;; (print "indeed loaded a file with (ns ..) : " *ns-name*)
		)))

;; this simply loads the file into the *nss* hash-table under the <the-ns> key
(define (ns-load-from-file the-ns file)
  (load file (*nss* the-ns))
  (let ((loaded-ns ((*nss* the-ns) '*ns-name*)))
    (unless (eq? the-ns loaded-ns)
      (throw 'invalid-namespace "expecting to load ns ~A from file ~A but got ~A~%"
	     the-ns
	     file
	     loaded-ns))))

;; eg returns foo/bar.scm given 'foo.bar
(define (ns-resolve-file the-ns)
  (format #f "~A.scm"
	  (string-replace-char #\. #\/ (symbol->string the-ns))))

;; Loads the namespace in its own environment
;; and puts it in the *nss* hash table, under the 'the-ns key
(define* (ns-load the-ns (force #f))
  (if (or (eq? #f (*nss* the-ns))
	  force)
      (begin
	(ns-get-or-create the-ns)
	(cond
	 ;; the-ns is already a let. probably a c binding
	 ((and (defined? the-ns)
		    (let? (symbol->value the-ns)))
	    
	  (set! (*nss* the-ns) (symbol->value the-ns)))
	 ;; we have autoload info
	 ((*autoload* the-ns)
	  (begin
	    (ns-load-from-file the-ns (*autoload* the-ns))))
	 ;; assuming the file path
	 (#t
	  (ns-load-from-file the-ns (ns-resolve-file the-ns)))))
      (begin
	()
	;; (print "Skipping already ns-require'd" the-ns)
	)))

;; Creates the dynamic bindings in the target-env
;; 
;; eg
;; (ns my.ns)
;; (ns-require 'foo.bar :as 'b)
;;
;; That creates the bindings b/fun inside my.ns
;; that call ((*nss* 'foo.bar) 'fun) and so on
;;
;; This is what enables the redifinitions to work!
;; If i'm in the foo.bar namespace and change the definition
;; of fun, this will be reflected inside my.ns
(define* (ns-create-bindings the-ns as target-env dynamic)
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
					  ;; dynamic bindings for functions:
					  ;; allows us "redefine" a functions in its own namespace
					  ;; and have this change reflected in the other namespaces
					  ;; that have required this
					  ;;
					  ;; This is enabled when we're dealing with procedures and the dynamic options is #t
					  (if (and (procedure? (cdr binding))
						  dynamic)
					      (lambda args
						;; TODO copy the documentatoin of the forwarding function?
						;; Could be useful in the future if something like geiser support
						;; is added, to show the documentation of the symbol under cursor
						(eval `(apply ,(car binding) ',args) (*nss* the-ns))
						)
					      ;; if it's not a procedure, just binding its value to where we called ns-require from
					      (cdr binding))
					  )))))
			  (*nss* the-ns))))))

;; TODO throw an error if ns not found
;; was scratching my head until I realised that I had a typo in the definition
;; and then i was requiring with the "correct" name.. but the ns was not defined!
(define* (ns-require the-ns (as #f) (force #f) (dynamic *ns-require-dynamic*))
  (set! *ns-load-mode* #f)
  (let ((current-ns *ns*))
    (begin
      (ns-load the-ns :force force)
      (eval `(ns-create-bindings ',the-ns
				 :as (or ',as ',the-ns)
				 :target-env ,current-ns
				 :dynamic ,dynamic)
		current-ns)
      (set! *ns* current-ns))))

;; maybe I should make this a normal function
;; keep things simpler..
(define-macro* (ns the-ns (require ()) (doc ""))
  `(begin
     (set! *ns* (ns-get-or-create ',the-ns))
     (eval `(define *ns-doc* ,,doc) *ns*)
     (map (lambda (require-form)
	    (apply ns-require require-form))
	  ',require)
     ',the-ns))

(define-macro (with-ns the-ns . body)
  `(with-let (*nss* ',the-ns)
	    ,@body))

(define-macro (with-temp-ns . body)
  (let ((ns-symbol (gensym "temp-ns"))
	(previous-ns (gensym "temp-ns-prev")))
    `(begin
       (set! (*nss* ',previous-ns) *ns*)
       ;; ughhh ns is macro
       ;; calling (ns ',ns-symbol) results the ns-symbol
       ;; to be quoted twice!
       ;; (apply ns (list ',ns-symbol))
       ;; or, don't quote it
       (ns ,ns-symbol)
       (catch #t
	      (lambda ()
		,@body)
	      (lambda (tag info)
		;; hm cleaning up again
		(set! *ns* (*nss* ',previous-ns))
		(set! (*nss* ',previous-ns) #f)
		(set! (*nss* ',ns-symbol) #f)
		(apply throw tag info)))
       (set! *ns* (*nss* ',previous-ns))
       (set! (*nss* ',previous-ns) #f)
       (set! (*nss* ',ns-symbol) #f))))
(comment
 (require aod.test)
 )

(test "Temp ns"
      (with-temp-ns
       (define x 1)
       (is-true (defined? 'x))
       (is = x 1))

      (with-temp-ns
       (is-false (defined? 'x)))
      )

(test "with-temp-ns cleanup"
      (define caught-something #f)
      (catch #t
	     (lambda ()
	       (with-temp-ns
		(define x 1)
		(is-true (defined? 'x))
		i-am-going-to-fail
		))
	     (lambda args
	       (set! caught-something #t)))
      (is-true caught-something)
      (is-false (defined? 'x))
      )

(define* (ns-doc the-ns fun)
  ;; it could be that it's not loaded in *nss* but it's defined
  ;; from the c side. In that case (symbol->value ..) gives us the
  ;; environment
  (let ((env (if (let? the-ns)
		 the-ns
		 (or (*nss* the-ns)
		     (symbol->value the-ns)))))
    (when env
      (if fun
	  (documentation (env fun))
	  (begin
	    ;; documenting the whole ns
	    (map (lambda (fn-pair)
		   (if (not (ns-should-doc? (car fn-pair)))
		       ;; ignoring things that
		       ;; start with - or *
		       ;; contain / (subnamespaces functions)
		       (values)
		       (cons
			(car fn-pair)
			(documentation (cdr fn-pair)))))
		 (let->list env)))))))

(comment
 (ns-doc 'aod.c.gl 'save-screenshot)
 (ns-doc 'aod.c.gl)
 (ns-doc 'aod.c.imgui)
 (ns-doc 'aod.c.foreign)
 (documentation ((*nss* 'aod.c.gl) 'save-screenshot))
 (let->list (*nss* 'aod.c.gl))
 )

(provide 'aod.ns)
