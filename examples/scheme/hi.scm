(display "hi from scheme")
(newline)

(set! (hook-functions *error-hook*) 
  (list (lambda (hook)
	  (format *stderr*
		  "~%~A: ~A~%~A:~A~%~A~%"
		  (hook 'type)
		  (apply format #f (hook 'data))
		  (port-filename)
		  (port-line-number); error file location
		  (stacktrace))
	  )))


(if (provided? 'linux)
    (display "have linux\n")
    (display "dont have linux\n"))

(reader-cond ((provided? 'linux)
	      (begin
		(display "line 1\n")
		(display "line 2\n")
		(display "line 3\n"))))

(display "here\n")

(load "repl.scm")
