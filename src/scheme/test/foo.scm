(ns test.foo
    :require ((test.bar :as b)))

(define (foo-double x)
  (* 2 x))

(define (foo-nested-double x)
  (b/double x))
