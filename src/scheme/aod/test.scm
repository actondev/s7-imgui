(display "in aod/test.scm\n")
(provide 'aod.test)
(define-macro* (assert assertion (msg ""))
  `(begin
     (if ,assertion
	 (begin
	   ;; (format *stderr* "~A: ok~%" ',assertion)
	   #t)
	 (begin
	   (format *stderr* "~A: ~A failed : \"~A\"~%" (*function*) ',assertion ,msg)
	   #f))))

(define is assert)

(define *aod.test* (let ((ht (make-hash-table)))
		     (set! (ht 'fail) 0)
		     (set! (ht 'pass) 0)
		     ht))

(define-macro (test name . body)
  `(catch #t
     (lambda ()
       (with-let (if (and (defined? '*ns*)
			  (let? *ns*))
		     *ns*
		     (curlet))
		 (call-with-exit
		  (lambda (return)
		    (map (lambda (e)
			   (let ((res (eval e)))
			     (unless res
			       (format *stderr* "FAIL: ~A~%" ,name)
			       (set! res #f)
			       (set! (*aod.test* 'fail) (+ 1 (*aod.test* 'fail)))
			       (return))))
			 ',body)
		    (set! (*aod.test* 'pass) (+ 1 (*aod.test* 'pass)))
		    (format *stderr* "PASS: ~A~%" ,name)
		    #t
		    ))))
     (lambda args
       (format *stderr* "FAIL: ~A~%\texception caught:~%\t~A~%"
	       ,name
	       (apply format #f (cadr args))))))
