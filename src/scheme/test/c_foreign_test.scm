(ns aod.test.c.foreign)
(ns-require aod.c.foreign :as c)

(test "new bool"
      (with-temp-ns
       (is-false (defined? 'x))
       (define x (c/new-bool #f))
       (is-false (x))
       (set! (x) #t)
       (is-true (x)))
      )

(test "new bool[]"
      (with-temp-ns
       (is-false (defined? 'x))
       (define x (c/new-bool[] 3))
       (is-false (x 0))
       (is-false (x 1))
       (is-false (x 2))
       (set! (x 1) #t)
       (is-true (x 1))
       (is-false (x 2))
       ;;
       ))

(test "new int"
      (with-temp-ns
       (is-false (defined? 'x))
       (define x (c/new-int 10))
       (is eq? 10 (x))
       (set! (x) 20)
       (is eq? 20 (x))
       ;;
       ))

(test "new int[]"
      (with-temp-ns
       (is-false (defined? 'x))
       (define x (c/new-int[] 3))
       (is eq? 0 (x 0))
       (is eq? 0 (x 1))
       (is eq? 0 (x 2))
       (set! (x 1) 11)
       (is eq? 11 (x 1))
       (is eq? 0 (x 2)))
      )
