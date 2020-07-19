(ns aod.layout)
(require aod.clj) ;; range

;; places the elements in a circular fashion
;; starting from 12 o'clock and going clock-wise

(define circular
  (let ((+documentation+ "Circular layout. Starts from 12 o'clock, going clock-wise Calls (cb x y :phase {0...(N-1)/N} :n {0...N}"))
    (lambda* (cb (N 12) (R 100) (center '(0 0)) (theta-offs (/ pi 2)))
      (let ((d-theta (/ (* 2 pi) N)))
	(map (lambda (i)
	       (let* ((theta (+ theta-offs (* i d-theta )))
		      (x (+ (center 0) (* R (- (*)) (cos theta))))
		      (y (+ (center 1) (* R (sin theta)))))
		 (cb x y :phase (/ i N) :n i)))
	     (range N))))))

(test "Circular layout"
      (with-temp-ns
       (define* (el x y :allow-other-keys)
	 `(el ,x ,y))
       (is (equivalent? '((el 0.0 1.0)
			  (el 1.0 0.0)
			  (el 0.0 -1.0)
			  (el -1.0 0.0))
			(circular el :N 4 :R 1)))
       ))

