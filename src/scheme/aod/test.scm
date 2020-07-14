(display "in aod/test.scm\n")
(define-macro* (assert assertion (msg ""))
  `(begin
     (if ,assertion
	 (begin
	   ;; (format *stderr* "~A: ok~%" ',assertion)
	   #t)
	 (begin
	   (format *stderr* "~A: ~A failed : \"~A\"~%" (*function*) ',assertion ,msg)
	   #f))))

(define-macro (test name . body)
  `(catch #t
	   (lambda ()
	     (call-with-exit
	       (lambda (return)
		 (map (lambda (e)
			(let ((res (eval e)))
			  (unless res
			    (format *stderr* "FAIL: ~A~%" ,name)
			    (set! res #f)
			    (return))))
		      ',body)
		 (format *stderr* "PASS: ~A~%" ,name)
		 #t
		 ))
	     )
	   (lambda args
	     (format *stderr* "FAIL: ~A~%\texception caught:~%\t~A~%"
		     ,name
		     (apply format #f (cadr args))))))
