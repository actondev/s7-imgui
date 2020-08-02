(ns test.partial)

(define (add-double a b)
  (+ a (* 2 b)))

(define add1-double
  (partial add-double 1))

(add1-double 2)

(define* (add-double* (a 0) (b 0))
  (+ a (* 2 b)))

(define add1-double*
  (partial add-double* :a 1))

(with-let (rootlet)
	  (require aod.test))
(test "partial"
      (= 5 (add-double 1 2))
      (= 5 (add1-double 2))
      (= 5 (add-double* 1 2))
      (= 5 (add-double* :a 1 :b 2))
      (= 5 (add1-double* :b 2))
      )
