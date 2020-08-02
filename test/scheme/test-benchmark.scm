;; testing performance of (ns-require)

(with-let (rootlet)
	  ;; aod.core already required by repl in cpp
	  (require aod.core)
	  ;; WIP: if providing aod.test.gui the (testgui .. ) blocks will be ran
	  ;; (provide 'aod.test.gui)
	  (require aod.test))

(ns aod.test-benchmark)

(with-let (rootlet)
	  ;; what fib to call
	  (define bench-fib-n 5)
	  ;; loops
	  ;; 1M
	  (define bench-fib-N 1000000)
	  ;; that yields double time in the dynamic require
	  )

(ns-require aod.benchmark :as b-dynamic)
(test "Dynamic (ns-require)"
      (print
       "ns-require: Time fib" bench-fib-n "times x" bench-fib-N
       (b-dynamic/time
	(dotimes (i bench-fib-N)
		 (b-dynamic/fib bench-fib-n)))))

(require aod.benchmark)
(test "Normally required"
      (with-let (unlet)
		(require aod.benchmark)
		(print
		 "require: Time fib" bench-fib-n "times x" bench-fib-N
		 (time
		  (dotimes (i bench-fib-N)
			   (fib bench-fib-n))))
		))

(ns-require aod.benchmark :as b-non-dynamic :dynamic #f)
(test "ns-require with dynamic #f"
      (print
       "ns-require :dynamic #f : Time fib" bench-fib-n "times x" bench-fib-N
       (b-non-dynamic/time
	(dotimes (i bench-fib-N)
		 (b-non-dynamic/fib bench-fib-n))))
      )

(set! *ns-require-dynamic* #f)
(ns-require aod.benchmark :as b-non-dynamic2)
(test "ns-require with dynamic #f - v2"
      (print
       "ns-require :dynamic #f v2 : Time fib" bench-fib-n "times x" bench-fib-N
       (b-non-dynamic2/time
	(dotimes (i bench-fib-N)
		 (b-non-dynamic2/fib bench-fib-n))))
		)

#|
ns-require: Time fib 5 times x 1000000 (#t 1.686106) 
PASS: aod.test-benchmark "Dynamic (ns-require)"
require: Time fib 5 times x 1000000 (#t 0.8520600000000003) 
PASS: aod.benchmark "Normally required"
Skipping already ns-require'd aod.benchmark 
ns-require :dynamic #f : Time fib 5 times x 1000000 (#t 0.8458799999999997) 
PASS: aod.benchmark "ns-require with dynamic #f"
Skipping already ns-require'd aod.benchmark 
ns-require :dynamic #f v2 : Time fib 5 times x 1000000 (#t 0.8527930000000001) 
PASS: aod.benchmark "ns-require with dynamic #f - v2"

|#


(exit (*aod.test* 'fail))
