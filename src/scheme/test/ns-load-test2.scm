;; no ns-form
;; default is rootlet?
(ns-require 'test.foo :as 'f)

(define (myinc x)
  (+ 1 x)
  )

(print "curlet here" (curlet))

(print "res" (f/foo-double (myinc 2)))
;; (exit 0)

