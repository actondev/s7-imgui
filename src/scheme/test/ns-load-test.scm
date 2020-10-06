(ns test.ns-load-test
    :require ((test.foo :as f)))

(define (myinc x)
  (+ 1 x)
  )

(print "res" (f/foo-double (myinc 2)))
(exit 0)
