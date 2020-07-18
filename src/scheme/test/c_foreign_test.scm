(ns aod.test.c.foreign)
(ns-require aod.c.foreign :as c)

(test "Bool, int, etc"
      (define b1 (c/new-bool #f))
      (is (eq? #f (b1)))
      (set! (b1) #t)
      (is (eq? #t (b1)))
      )

(comment
 (eq? is assert)
 )
