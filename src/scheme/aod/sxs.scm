(define *test* #t)
(require aod.core)
(aod/require aod.geom :as geom)
(provide 'aod.sxs) ;; sigma-x-square
(display "loading aod/sxs.scm\n")
;; private
(define* (-arrow-angles (dir 'right))
  ;; or.. I could just hardcode it.. wtf
  (let* ((angle-a (* 0.75 pi))
	 (angle-b (+ angle-a (/ pi 2))))
    (cond ((eq? dir 'right)
	   (list angle-a angle-b))
	  (else
	   (list (- angle-a pi)
		 (- angle-b pi))))))

(test "Arrow angles"
      (assert (equivalent? '(135.0 225.0)
			   (map geom/rad->deg
				(-arrow-angles :dir 'right))))
      (assert (equivalent? '(-45 45)
			   (map geom/rad->deg
				(-arrow-angles :dir 'left)))))


;; (define* (arrow-lines cx cy r (dir :right) (color (ig/frgb->u32 0.5 0.5 0.5)))
;;   (map (lambda (theta)
;; 	 (sxs/circular-line :cx cx :cy cy :r r
;; 			    :theta theta))
;;        (sxs/-arrow-angles :dir dir)))

(define* (polar-line circle theta
		     (mod-theta identity))
  (let* ((cx (circle 0))
	 (cy (circle 1))
	 (r (circle 2))
	 (theta (mod-theta theta))
	 (x (* r (cos theta)))
	 (y (* r (sin theta))))
    (list cx cy
	  (+ cx x)
	  (+ cy y))))

(test "Polar lines"
      (assert (equivalent? '(0 0 10 0)
			   (polar-line (geom/mk-circle :cx 0 :cy 0 :r 10)
				       :theta 0)))
      (assert (equivalent? '(0 0 0 10)
			   (polar-line (geom/mk-circle :cx 0 :cy 0 :r 10)
				       :theta (/ pi 2))
			   
			   ))
      (assert (equivalent? '(0 0 1 1)
			   (polar-line (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
				       :theta (/ pi 4)))
	      "Slope of 1, radius (sqrt 2) => x=y=1")

      (assert (equivalent? '(0 0 -1 -1)
			   (polar-line (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2)) :theta (+ pi (/ pi 4)))))

      ;; fucking rounding
      (assert (equivalent? '(0 0 -10.0 1.0e-15)
			   (polar-line (geom/mk-circle :cx 0 :cy 0 :r 10)
				       :theta 0 :mod-theta (lambda (x) (+ x pi))))
	      "Mod-theta: useful for imgui drawing (needs to be clockwise")
      ;; / polar lines
      )

(define* (arrow-lines circle (dir 'right))
  (map (lambda (theta)
	 (polar-line circle :theta theta))
       (-arrow-angles :dir dir))
  )

(test "Drawing arrows"
      (assert (equivalent? '((0 0 -1 1) (0 0 -1 -1))
			   (arrow-lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
					:dir 'right)))
      (assert (equivalent? '((0 0 1 -1) (0 0 1 1))
			   (arrow-lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
					:dir 'left)))
      )

(define (-logo/x)
  (let ((cx 100)
	(cx 100)
	(r 100))
    
    ))
