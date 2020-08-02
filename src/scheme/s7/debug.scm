(provide 'debug.scm)

(let-temporarily (((*s7* 'debug) 0))          ; trace-in call should not be added to trace-in!

  (define trace-in
    (let (;; these are for external use
	  (*debug-port* *stderr*)             ; an output (string) port or #f
	  (*debug-stack* #f)                  ; either a vector or #f (provides a C-style call stack)
	  (*debug-function* #f)               ; either #f or a function of 3 arguments, the current function, the calling expression, and the current environment
	  ;; these are normally internal
	  (*debug-repl* (lambda (call e) #f)) ; called by break
	  (*debug-breaks* ())                 ; list of functions with active breakpoints
	  ;; rest are for output formatting etc
	  (*debug-spaces* 0)                  ; indentation (when reporting result)
	  (*debug-max-spaces* (*s7* 'max-format-length))
	  (*debug-start-output* (lambda (p) #f))
	  (*debug-end-output* newline)
	  (*debug-curlet* #f))                ; currently just for debug-frame

      (set! (setter '*debug-spaces*) integer?)
      (set! (setter '*debug-max-spaces*) integer?)
      (set! (setter '*debug-breaks*) list?)
      (set! (setter '*debug-start-output*) procedure?)
      (set! (setter '*debug-end-output*) procedure?)

      (set! (setter '*debug-port*)
	    (lambda (s v)
	      (if (or (output-port? v) (not v))
		  v
		  (error 'wrong-type-arg "~S can't be set! to ~S" s v))))

      (set! (setter '*debug-stack*)
	    (lambda (s v)
	      (if (or (vector? v) (not v))
		  v
		  (error 'wrong-type-arg "~S can't be set! to ~S" s v))))

      (set! (setter '*debug-function*)
	    (lambda (s v)
	      (if (or (not v)
		      (and (procedure? v)
			   (= (car (arity v)) 3)))
		  v
		  (error 'wrong-type-arg "~S must be #f or a procedure of 3 arguments" s))))


      (define trace-out
	(let ((funcname #f))
	  (lambda (e val)              ; report value returned
	    (let-temporarily (((*s7* 'history-enabled) #f))
	      (set! *debug-spaces* (max 0 (- *debug-spaces* 2)))
	      (let-temporarily (((*s7* 'openlets) #f)) ; val local object->string might cause an infinite loop
		(format *debug-port* "~NC  -> ~S" (min *debug-spaces* *debug-max-spaces*) #\space val))

	      (set! funcname (*function* e :name))
	      (when (symbol? funcname)
		(let ((sig (signature (symbol->value funcname e))))   ; check result type, if a signature exists
		  (when (and (pair? sig)
			     (not ((symbol->value (car sig)) val)))
		    (format *debug-port* ", result is not ~S" (car sig))))))

	    (*debug-end-output* *debug-port*)
	    val)))


      (lambda (e)                   ; trace-in: report function call and return value
	(when (> (*s7* 'debug) 0)
	  (let-temporarily (((*s7* 'history-enabled) #f))

	    (let* ((func (*function* e))
		   (funcname (if (pair? func) (car func) func))
		   (args (let-temporarily (((*s7* 'debug) 0)) ; keep s7 from adding trace-in to this map function
			   (map (let ((n 0)
				      (func-arity (and (symbol? funcname) 
						       (arity (symbol->value funcname e)))))   
				  (lambda (x)
				    (set! n (+ n 1))
				    (if (and (symbol? funcname)
					     (pair? x)
					     (pair? (cdr x))
					     (pair? func-arity)
					     (= (cdr func-arity) 536870912)
					     (> n (car func-arity)))
					(apply values (cdr x))
					(if (and (or (pair? (cdr x))
						     (and (symbol? (cdr x))
							  (not (keyword? (cdr x)))))
						 (not (and func (macro? (symbol->value funcname e)))))
					    (list 'quote (cdr x))
					    (cdr x)))))
				e)))

		   (call (let-temporarily (((*s7* 'openlets) #f))  ; ignore local object->string methods
			   (format #f "(~S~{~^ ~S~})"
				   (or funcname '_)
				   args))))

	      (set! *debug-curlet* e)

	      (if (vector? *debug-stack*)
		  (vector-set! *debug-stack* (max 0 (min (- (length *debug-stack*) 1)
							 (/ *debug-spaces* 2)))
			       call))

	      (cond ((and *debug-function*
			  (*debug-function* func call e))) ; if it returns #f, try rest of possibilities

		    ((and funcname
			  (memq funcname *debug-breaks*))
		     (*debug-repl* call e))

		    (else
		     (*debug-start-output* *debug-port*)
		     (format *debug-port* "~NC~A" (min *debug-spaces* *debug-max-spaces*) #\space call)
		     (if (pair? func)
			 (format *debug-port* "    ; ~S: ~A[~D]" (car func) (cadr func) (caddr func)))
		     (if (and (> (port-line-number) 0)
			      (not (string=? (port-filename) "*stdin*"))
			      (or (not (pair? func))
				  (not (= (caddr func) (port-line-number)))
				  (not (string=? (cadr func) (port-filename)))))
			 (if (and (pair? func)
				  (string=? (cadr func) (port-filename)))
			     (format *debug-port* " called from line ~D?"
				     (port-line-number))
			     (format *debug-port* "~A called from ~A[~D]"
				     (if (pair? func) "" "    ;")
				     (port-filename)
				     (port-line-number))))

		     (when (symbol? funcname)
		       (let ((sig (signature (symbol->value funcname e))))   ; check arg types, if a signature exists
			 (when (pair? sig)
			   (let ((ctr 1))
			     (for-each (lambda (typer value)
					 (when (and (symbol? typer)
						    (not ((symbol->value typer) value)))
					   (format *debug-port* "~%    ; ~S arg ~D not ~S" funcname ctr typer))
					 (set! ctr (+ ctr 1)))
				       (cdr sig) args)))))

		     (newline *debug-port*)))
	      (set! *debug-spaces* (+ *debug-spaces* 2))))

	  (dynamic-unwind trace-out e)))))

  (define debug-port (dilambda
		      (lambda ()
			((funclet trace-in) '*debug-port*))
		      (lambda (new-port)
			(set! ((funclet trace-in) '*debug-port*) new-port))))
  
  (define debug-stack (dilambda
		       (lambda ()
			 ((funclet trace-in) '*debug-stack*))
		       (lambda (new-stack)
			 ;; (set! (debug-stack) (make-vector 64 #f)) or (set! (debug-stack) #f)
			 (set! ((funclet trace-in) '*debug-stack*) new-stack))))
  
  (define debug-function (dilambda
			  (lambda ()
			    ((funclet trace-in) '*debug-function*))
			  (lambda (new-function)
			    (set! ((funclet trace-in) '*debug-function*) new-function))))
  
  (define debug-repl (dilambda
		      (lambda ()
			((funclet trace-in) '*debug-repl*))
		      (lambda (new-repl)
			(set! ((funclet trace-in) '*debug-repl*) new-repl)))))


;;; -------- trace
(define-bacro* (trace (func :unset))
  ;; a bacro because we can be called anywhere (repl top-level-let), we're being asked to trace something in that let probably,
  ;;   but outlet(trace), if trace is a macro, is (rootlet) if (load "debug.scm"), so trace thinks func is undefined.
  ;;   a macro might work if the entire body were returned, and a second quasiquote level used for the set!?
  (if (eq? func :unset)
      (set! (*s7* 'debug) 3)
      (catch #t
	(lambda ()

	  (let ((func-val (eval func)))         ; func can be arbitrarily complex
	   (if (not (or (procedure? func-val)
			(macro? func-val)))
	       (format (debug-port) "trace: ~S is not a procedure or macro" func)

	       (let ((func-name (object->string func-val))
		     ;; the name is usually a string internally, so this isn't as wasteful as it appears
		     (source (procedure-source func-val))                        ; (lambda (x) (+ x 1))
		     (setf (setter func-val)))                                   ; preseve possible setter
		 (set! func-name (if (char=? (func-name 0) #\#) '_ (symbol func-name)))

		 (set! (*s7* 'debug) (max 1 (*s7* 'debug))) ; not sure about this, but it's a bother to type in the repl

		 (if (pair? source)
		     (unless (and (pair? (caddr source))
				  (eq? (caaddr source) 'trace-in))
		       (let ((new-source (cons (car source)                      ; lambda
					       (cons (cadr source)               ; args
						     (cons '(trace-in (curlet))  ; (trace-in (curlet))
							   (cddr source)))))     ; body
			     (out (outlet (funclet func-val))))                  ; preserve possible local closure
			 (if setf
			     `(set! ,func (with-let ,out 
					    (dilambda (let () 
							(define ,func-name ,new-source))
						      ,setf)))
			     `(set! ,func (with-let ,out 
					    (let () 
					      (define ,func-name ,new-source)))))))
		     ;; we need to use define to get the function name saved for *function* later, but
		     ;;   we also need to clobber the old definition (not just shadow it) so that existing calls
		     ;;   will be traced.  So, use a redundant define in a let returning its new value, setting the current one.

		     (let ((old-func (gensym)))
		       (if setf
			   `(set! ,func (dilambda
					 (let ()
					   (define ,func-name
					     (let ((,old-func ,func))
					       (lambda args
						 (trace-in (curlet))
						 (apply ,old-func args)))))
					 ,setf))
			   `(set! ,func (let ()
					  (define ,func-name
					    (let ((,old-func ,func))
					      (lambda args
						(trace-in (curlet))
						(apply ,old-func args)))))))))))))
	(lambda (type info)
	  (format (debug-port) "can't trace ~S: ~S~%" func (apply format #f info))))))


;;; -------- untrace
(define-bacro* (untrace (func :unset))
  (if (eq? func :unset)
      (set! (*s7* 'debug) 0)

      (catch #t
	(lambda ()

	  (let ((func-val (eval func)))         ; func can be arbitrarily complex
	   (if (not (or (procedure? func-val)
			(macro? func-val)))
	       (format (debug-port) "untrace: ~S is not a procedure or macro" func)

	       (let ((func-name (symbol (object->string func-val)))
		     (source (procedure-source func-val))
		     (setf (setter func-val)))

		 (when (and (pair? source)
			    (pair? (caddr source))
			    (eq? (caaddr source) 'trace-in))
		   (let ((cdddr-source (cdddr source)))
		     (if (and (eq? (caar cdddr-source) 'apply)
			      (gensym? (cadar cdddr-source)))
			 (let ((orig-func (symbol->value (cadar cdddr-source) (funclet func-val))))
			   (if setf
			       `(set! ,func (dilambda ,orig-func ,setf))
			       `(set! ,func ,orig-func)))
			 (let ((new-source (cons (car source)
						 (cons (cadr source)
						       cdddr-source)))
			       (out (outlet (funclet func-val))))
			   (if setf
			       `(set! ,func (with-let ,out
					      (dilambda (let ()
							  (define ,func-name
							    ,new-source))
							,setf)))
			       `(set! ,func (with-let ,out
					      (let ()
						(define ,func-name
						  ,new-source)))))))))))))
	(lambda (type info)
	  (format (debug-port) "can't untrace ~S: ~S~%" func (apply format #f info))))))


;;; -------- break
(define-bacro (break func)
  (let ((func-val (eval func)))
    (if (not (or (procedure? func-val)
		 (macro? func-val)))
	(format (debug-port) "break: ~S is not a procedure or macro" func)
	(let ((func-name (symbol (object->string func-val))))
	  (unless (memq func-name ((funclet trace-in) '*debug-breaks*))
	    `(begin
	       (trace ,func)
	       (set! ((funclet trace-in) '*debug-breaks*)
		     (cons ',func-name 
			   ((funclet trace-in) '*debug-breaks*)))))))))

;;; -------- unbreak
(define-bacro* (unbreak (func :unset))
  (if (eq? func :unset)
      (set! ((funclet trace-in) '*debug-breaks*) ())
      (let ((func-name (symbol (object->string (eval func)))))
	`(set! ((funclet trace-in) '*debug-breaks*)
	       (let remove ((lst ((funclet trace-in) '*debug-breaks*)) (new-lst ()))
		 (if (null? lst)
		     (reverse new-lst)
		     (remove (cdr lst)
			     (if (eq? (car lst) ',func-name)
				 new-lst
				 (cons (car lst) new-lst)))))))))


;;; -------- watch
(define-macro (watch var)   ; notification if var set!
  (if (pair? var)
      `(with-let ,(car var)
	 (set! (setter ,(cadr var))
	       (let ((old-setter (setter ,(cadr var))))
		 (lambda (s v e)
		   (format (debug-port) "let-set! ~S to ~S~%" s v)
		   (if old-setter
		       (if (eqv? (cdr (arity old-setter)) 2)
			   (old-setter s v)
			   (old-setter s v e))
		       v)))))
      `(set! (setter ',var)
	     (let ((old-setter (setter ',var)))
	       (lambda (s v e)
		 (format (debug-port) "~S set! to ~S~A~%" s v
			 (if (let? e) ; might be (rootlet) == ()
			     (let ((func (*function* e)))
			       (if (memq func '(#f #<undefined>)) "" (format #f ", ~S" func)))
			     ""))
		 (if old-setter
		     (if (eqv? (cdr (arity old-setter)) 2)
			 (old-setter s v)
			 (old-setter s v e))
		     v))))))

;;; -------- unwatch
(define-macro (unwatch var)
  (if (pair? var)
      `(with-let ,(car var)
	 (set! (setter ,(cadr var)) (with-let (funclet (setter ,(cadr var))) old-setter)))
      `(set! (setter ',var) (with-let (funclet (setter ',var)) old-setter))))


;;; -------- stack
(define (show-debug-stack)
  (let ((stack (debug-stack))
	(depth ((funclet trace-in) '*debug-spaces*)))
    (when stack
      (format (debug-port) "~NCstack:\n" depth #\space)
      (do ((i 0 (+ i 1)))
	  ((or (= i (length stack))
	       (> i (/ ((funclet trace-in) '*debug-spaces*) 2))
	       (not (string? (vector-ref stack i)))))
	(format (debug-port) "~NC~A~%" (+ depth 2) #\space (vector-ref stack i)))
      ;(newline (debug-port))
      #f)))


;;; -------- debug-frame
(define (debug-frame n)
  (do ((p ((funclet trace-in) '*debug-curlet*) (outlet p))
       (i 0 (+ i 1)))
      ((or (= i n) (not (let? p)))
       (format (debug-port) "~S~%" p))))


#|
;; trace with a function to call at the trace point, rather than trace-in's code
(set! (debug-function) (lambda (func call e) ...)) ; return #f to include normal tracing

;; trace one function specially:
(set! (debug-function)
      (lambda (func call e)             ; if this returns #f, trace-in goes on as normally
        (and (eq? func desired-func)
             ... 
             #t)))

;; break one function specially, or break-if: this is as above but (*debug-repl* call e)

;; log trace info
(call-with-output-file "test.log"
  (lambda (port)
    (let-temporarily (((debug-port) port))
      ...)))

;; debug-stack in s7_error if debug.scm loaded, debug>1 and stack exists
;;   if sc->debug>1, we know trace-in is loaded, so closure_let(symbol->value(sc, make_symbol(sc, "trace-in"))) has *debug-stack* etc

;; in gtk|motif-snd, break does not stop or give the right prompt, but curlet is correct??
;;   (load "debug.scm")
;;     #<lambda (call e)>
;;   (set! (*s7* 'debug) 1)
;;     1
;;   (define (g1 x)  (+ x 1))
;;     g1
;;   (break g1)
;;     (g1)
;;   (g1 2)  -> 3
;;     3
;;   (curlet)
;;     (inlet 'x 2)
;; nogui-snd does work (it's repl.scm based)
;; watching for c-q via dynamic-unwind does not work right, tmp/snd-xen.c
;; perhaps a (listener) function for Snd? (include event loop etc: check_for_event in snd-motif.c and snd-gutils.c)
;;   also failed in motif (listener widget callbacks are inactive??)
|#


(when (defined? 'debug.scm-init) ; connect to the repl, if any
  ((symbol->value 'debug.scm-init)))
