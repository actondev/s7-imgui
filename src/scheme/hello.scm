(display "hello from s7 scheme")
(newline)
(display "(one more line)")
(newline)

(define-macro (time expr) 
  `(let ((start (*s7* 'cpu-time)))
     (let ((res (list ,expr))) ; expr might return multiple values
       (list (car res)
	     (- (*s7* 'cpu-time) start)))))


(load "scheme/fib.scm")
(display "fib 34\n")
(display
 (time (fib 34))
	 )
(newline)
