(ns aod.test.c.foreign)
(ns-require aod.c.foreign :as c)

(test "new bool"
      (with-temp-ns
       (is (not (defined? 'x)))
       (define x (c/new-bool #f))
       (is (eq? #f (x)))
       (set! (x) #t)
       (is (eq? #t (x))))
      )

(test "new bool[]"
      (with-temp-ns
       (is (not (defined? 'x)))
       (define x (c/new-bool[] 3))
       (is (eq? #f (x 0)))
       (is (eq? #f (x 1)))
       (is (eq? #f (x 2)))
       (set! (x 1) #t)
       (is (eq? #t (x 1)))
       (is (eq? #f (x 2)))
       ;;
       ))

(test "new int"
      (with-temp-ns
       (is (not (defined? 'x)))
       (define x (c/new-int 10))
       (is (eq? 10 (x)))
       (set! (x) 20)
       (is (eq? 20 (x)))
       ;;
       ))

(test "new int[]"
      (with-temp-ns
       (is (not (defined? 'x)))
       (define x (c/new-int[] 3))
       (is (eq? 0 (x 0)))
       (is (eq? 0 (x 1)))
       (is (eq? 0 (x 2)))
       (set! (x 1) 11)
       (is (eq? 11 (x 1)))
       (is (eq? 0 (x 2))))
      )
