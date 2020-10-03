(ns test.core)

(with-let (rootlet)
	  (require aod.test))

(load "aod/core.scm")

(exit (*aod.test* 'fail))
