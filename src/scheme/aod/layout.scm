;; places the elements in a circular fashion
;; starting from 12 o'clock and going clock-wise
(provide 'aod.layout)

(define* (layout/circular cb (N 12) (R 100) (r 20))
  (let ((+documentation+ "Calls (cb x y)"))
    (let ((d-theta (/ (* 2 pi) N))
	  (center R)
	  (R (- R r)))
      (dotimes (i N)
	(let* ((theta (- (+ (* i d-theta ) (/ pi 2) )))
	       (x (+ center (* R (- (*)) (cos theta))))
	       (y (+ center (* R (sin theta)))))
	  ;; (format *stderr* "here x ~A\n" x)
	  (cb x y :phase (/ i N) :n i)
	  )))))
