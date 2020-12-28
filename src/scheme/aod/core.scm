;; (display "loading aod/core\n")
;; putting the autload info here, among other necessary things (that I use often)
(load "aod/autoloads.scm")
;; comment, map-indexed, dotimes, range, mod
;; on the (rootlet)

;; ignornig tests: test expansion/macro replaced in aod.test
(unless (provided? 'aod.core)
  ;; defining test and testgui only the first time
  ;; cause i replace them later on. if for some reason this file is reloaded
  ;; don't want it to replace that definition
  (define-expansion (test . body) #<unspecified>)
  (define-expansion (testgui . body) #<unspecified>))

(require aod.clj)

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

;; returns the last argument
;; useful for in-drop debugging, printing what we return
(define (print-ret . args)
  (apply print args)
  (if (pair? args)
      (car (reverse args))
      ()))

(comment
 (print 'a 'b "aasa" '(a b c))

 (let->list (curlet))
 ((curlet) (string->symbol "lines"))
 geom/echo
 )

;; hmm not sure how it's useful
;; from s7.html
(define (concat . args)
  (apply append (map (lambda (arg) (map values arg)) args)))

;; aod.ns has tests and may make some use of the rest of
;; internal funtions, so requiring at the end
(require aod.ns)
(define (memoize fn)
  (let ((mem (make-hash-table)))
    (lambda args
      (or (mem args)
	  (begin
	    ;; (print "not found, fn" fn "args " args)
	    (let ((ret (apply fn args)))
	      ;; (print "ret " ret)
	      (set! (mem args) ret)
	      ret))))))
;; if-let, when-let
;; only for one variable
;; TODO
;; - validate input? only one
;; syntax is clj like, passing one list (symbol val)
;; eg (when-let (x #f) ..)
(define-macro (if-let binding then else)
  `(let ((,(car binding) ,(cadr binding)))
     (if ,(car binding)
	 ,then
	 ,else)))

(define-macro (when-let binding . body)
  `(let ((,(car binding) ,(cadr binding)))
     (when ,(car binding)
       ,@body)))

;; from s7 stuff.scm
(define-macro (and-let* vars . body)      ; bind vars, if any is #f stop, else evaluate body with those bindings
  (if (list? vars)
      `(let () (and ,@(map (lambda (v) (cons 'define v)) vars) (begin ,@body)))
      (error 'wrong-type-arg "and-let* var list is ~S" vars)))

(define when-let* and-let*)

(define-macro (if-let* vars then else)      ; bind vars, if all are #t evaluate "then", otherwise "else"
  (if (list? vars)
      `(let ()
	 (if (and ,@(map (lambda (v) (cons 'define v)) vars))
	     ,then
	     ,else))
      (error 'wrong-type-arg "and-let* var list is ~S" vars)))

(comment
 (if-let (x #t)
	 1
	 2)

 (when-let (x #t)
	   (print "one")
	   (print "two")
	   1)
 
 (if-let* ((x #t)
	  (y #t))
	 (print "true?")
	 (print "false?"))

 (when-let* ((x #f))
	   (print "this")
	   (print "that"))
 )

(define (sign x)
  (cond ((> x 0) 1)
	((< x 0) -1)
	(#t 0)))

;; needed in ns.scm
(define* (string-replace-char from to string)
  (let ((res ""))
    (let loop ((start 0)
	       (end (char-position from string 0)))
      (if (eq? #f end)
	  (begin
	    ;; the end, appending what's left of the string
	    (set! res (format #f "~A~A" res (substring string start (length string))))
	    res)
	  (begin
	    (set! res (format #f "~A~A~A" res (substring string start end) to))
	    (loop (inc end) (char-position from string (inc end))))))))

(test "memoize"
      (with-let (unlet)
		(define x 1)
		(define (inc-x amount)
		  (set! x (+ x amount)))
		(define inc-x-mem (memoize inc-x))
		(is = 3 (inc-x 2))
		;; first time we do the call
		(is = 5 (inc-x-mem 2))
		;; then not
		(is = 5 (inc-x-mem 2))
		))
(test "string replace char"
      (is equivalent? "test/foo/bar"
	  (string-replace-char #\. #\/ "test.foo.bar"))
      (is equivalent? "test/foo/bar/"
	  (string-replace-char #\. #\/ "test.foo.bar.")))

(provide 'aod.core)
