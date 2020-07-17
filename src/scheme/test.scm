(require aod.core)
(require aod.test)

;; testing files:
(require aod.geom)

(with-let (unlet)
	  (load "test/ns_test.scm")
	  )

(require aod.sxs)

;; (with-let (unlet)
;; 	  (define some-variable 1)
;; 	  (test "Test isolation"
;; 		(is (= some-variable 1))))

(print "Passed:" (*aod.test* 'pass))
(print "Failed:" (*aod.test* 'fail))
(exit (*aod.test* 'fail))
