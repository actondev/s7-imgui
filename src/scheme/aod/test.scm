(provide 'aod.test)
(define-macro* (assert assertion (msg ""))
  ;; hm msg is not used
  `(begin
     (if ,assertion
	 (begin
	   ;; (format *stderr* "~A: ok~%" ',assertion)
	   #t)
	 (begin
	   ;; (format *stderr* "------ ~A: ~A failed~%" (*function*) ',assertion)
	   (throw 'assertion-failed "~A: ~A~%" (*function*) ',assertion)
	   #f))))

(define is assert)

(define *aod.test* (let ((ht (make-hash-table)))
		     (set! (ht 'fail) 0)
		     (set! (ht 'pass) 0)
		     ht))

(define-macro (test name . body)
  (let ((test-header (or (*ns* '*ns-name*) "")))
    `(catch #t
	    (lambda ()
	      (with-let (if (and (defined? '*ns*)
				 (let? *ns*))
			    *ns*
			    (curlet))
			(let ((*test-env* (curlet)))
			  (call-with-exit
			   (lambda (return)
			     (map (lambda (e)
				    ;; (print "===> eval " e)
				    (eval e *test-env*))
				  ',body)
			     (set! (*aod.test* 'pass) (+ 1 (*aod.test* 'pass)))
			     (format *stderr* "PASS: ~A \"~A\"~%" ',test-header ,name)
			     #t
			     )))))
	    (lambda args
	      (set! (*aod.test* 'fail) (+ 1 (*aod.test* 'fail)))
	      (let ((exc-format-data (cadr args))
		    (exc-info (car args)))
		(format *stderr* "FAIL: ~A \"~A\" \n\t~A~%\t~A~%"
			',test-header
			,name
			exc-info
			(apply format #f exc-format-data)
			))))))
