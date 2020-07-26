(ns aod.benchmark)
(provide 'aod.benchmark)

(define (fib n)
  (if (<= n 1) 1
      (+ (fib (- n 1))
	 (fib (- n 2)))))

(define-macro (time expr) 
  `(let ((start (*s7* 'cpu-time)))
     (let ((res (list ,expr))) ; expr might return multiple values
       (list (car res)
	     (- (*s7* 'cpu-time) start)))))
