(ns test.core)

(with-let (rootlet)
	  (require aod.test))

(test "memoize"
      (with-let (unlet)
		(define x 1)
		(define (inc-x amount)
		  (set! x (+ x amount)))
		(define inc-x-mem (memoize inc-x))
		(is (= 3 (inc-x 2)))
		;; first time we do the call
		(is (= 5 (inc-x-mem 2)))
		;; then not
		(is (= 5 (inc-x-mem 2)))
		))

(exit (*aod.test* 'fail))
