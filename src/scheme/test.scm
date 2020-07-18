(with-let (rootlet)
	  ;; aod.core already required by repl in cpp
	  (require aod.core)
	  (require aod.test))

(ns aod.all-tests)

;; testing files:
(ns-load 'aod.ns)
(ns-load 'aod.geom)
(ns-load 'aod.sxs)
(ns-load-file "test/c_foreign_test.scm")


(print "======")
(print "Passed:" (*aod.test* 'pass))
(print "Failed:" (*aod.test* 'fail))
(print ">")
;; (exit (*aod.test* 'fail))

(comment
 (+ 1 2 3)
 *nss*
 (defined? 'environment?)
 (defined? 'let?)
 (let? aod.c.imgui)
 (defined? 'aod.c.foreign)
 aod.c.foreign
 (*nss* 'aod.all-tests)
 
 (let? )
 ()
 (print "hi")
 (defined? 'aod.c.foreign)
 )
