(with-let (rootlet)
	  ;; aod.core already required by repl in cpp
	  (require aod.core)
	  (require aod.test))

(ns aod.all-tests)

;; testing files:
(ns-require aod.ns)
(ns-require aod.geom)
(ns-require aod.sxs)

(print "======")
(print "Passed:" (*aod.test* 'pass))
(print "Failed:" (*aod.test* 'fail))
(exit (*aod.test* 'fail))
