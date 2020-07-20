(ns aod.layout)

(define circular
  (let ((+documentation+ "Circular layout.
Calls (cb x y :phase [0 .. (N-1)/N] :n [0 .. N]

If the gui flag is passed (and indeed working with computer graphics)
the elements will be drawn clock-wise starting from 12 o'clock. The
theta-offset and clock-wise flags won't have any effect. Let's call it
a known-issue"))
    (lambda* (cb (N 12) (R 100) (center '(0 0)) (clock-wise #f) (theta-offset 0) (gui #f))
	     (when gui
	       (set! theta-offset (- (/ pi 2)))
	       (set! clock-wise #f) ;; when in gui, with clock-wise #f the drawing is.. clock-wise
	       )
	     (let ((d-theta (/ (* 2 pi) N)))
	       (map (lambda (i)
		      (let* (
			     (theta (+ theta-offset (* i d-theta )))
			     (theta (if clock-wise
					(- theta)
					theta))
			     (x (+ (center 0) (* R (cos theta))))
			     (y (+ (center 1) (* R (sin theta)))))
			(cb x y :phase (/ i N) :n i)))
		    (range N))))))

(test "Circular layout"
      (with-temp-ns
       (define* (el x y :allow-other-keys)
	 `(el ,x ,y))
       (is (equivalent? '((el 1.0 0.0)
			  (el 0.0 1.0)
			  (el -1.0 0.0)
			  (el 0.0 -1.0))
			(circular el :N 4 :R 1 )))

       (is (equivalent? '((el 1.0 0.0)
			  (el 0.0 -1.0)
			  (el -1.0 0.0)
			  (el 0.0 1.0))
			(circular el :N 4 :R 1 :clock-wise #t)))
       ))

