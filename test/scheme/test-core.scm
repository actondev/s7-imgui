(ns test.core)

(with-let (rootlet)
	  (require aod.test))

(load "aod/core.scm")
(load "aod/clj.scm")

(exit (*aod.test* 'fail))
