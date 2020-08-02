(require aod.clj)
(ns foo.bar)
(ns-require aod.benchmark :as bench)

(test "Normal required calls"
      (is (= 89 (bench/fib 10)))
      )
