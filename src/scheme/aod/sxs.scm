(ns aod.sxs)
(ns-require aod.geom :as geom)

;; note: if i direclty pass this file to the repl executable
;; i need to laod the aod.test in the (rootlet) in order for
;; the tests to run
(comment
 (with-let (rootlet)
	   (require aod.test))
 )

;; The needed length of the arrow in order to form a perfect square
;; Cause r*cos45 = sqrt(2)/2 .. we want it to be 2/3
;; x * sqrt(2)/2 = 2/3 * r
;; => x= 4/(3* sqrt(2)) * r
(define (arrow-length r)
  (/ (* 4 r)
     (* 3 (sqrt 2))))

(define factor-arrow-length
  (/ 4
     (* 3 (sqrt 2))))

(define factor-clip-radius
  (/ 1 factor-arrow-length))

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

;; like geom/radius-line, but we need to adjust r
(define (sxs-radius-line circle theta)
  (let* ((cx (circle 0))
	 (cy (circle 1))
	 (r (* factor-arrow-length (circle 2)))
	 (x (* r (cos theta)))
	 (y (* r (sin theta))))
    (list cx cy
	  (+ cx x)
	  (+ cy y))))

(define* (arrow-lines circle (dir 'right))
  (map (lambda (theta)
	 (sxs-radius-line circle theta))
       (-arrow-angles :dir dir)))

(test "Drawing arrows"
      (assert (equivalent? '((0 0 -0.9428090415820632 0.9428090415820634) (0 0 -0.9428090415820636 -0.9428090415820632))
			   (arrow-lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
					:dir 'right)))
      (assert (equivalent? '((0 0 0.9428090415820634 -0.9428090415820632) (0 0 0.9428090415820634 0.9428090415820632))
			   (arrow-lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
					:dir 'left)))
      )

(define (-arrow-right circle)
  (map (lambda (theta)
	 (sxs-radius-line circle theta))
       (-arrow-angles :dir 'right)))

(define (-arrow-left circle)
  (map (lambda (theta)
	 (sxs-radius-line circle theta))
       (-arrow-angles :dir 'left)))

(define (arrows-right circle)
  (let* ((r (circle 2))
	(offset (* 2 r)))
    (geom/repeat-lines
     (-arrow-right circle)
     `((,(- offset) 0)
       (0 0)
       (,offset 0)))))

(define (arrows-left circle)
  (let* ((r (circle 2))
	(offset (* 2 r)))
    (geom/repeat-lines
     (-arrow-left circle)
     `((,(- offset) 0)
       (0 0)
       (,offset 0)))))

(test "SXS repeat line"
      (assert (equivalent? '((0 2 10 12) (0 -2 10 8))  (geom/repeat-line '(0 0 10 10) '((0 2) (0 -2))))))

;; The sigma-x-square lines to draw the logo
;;
;; Could make every other function "private" actually
(define lines
  (let ((+documentation+ "(lines circle (phase 0) (clip #t))
Returns a list of the lines to be drawn to paint the sxs logo.

The circle is of the (cx cy r) form.
A line is of the (x1 y1 x2 y2) form."))
    (lambda* (circle (phase 0) (clip #t))
	     (let* ((phase (mod (+
				 (/ 2 3) ;; 2/3 offset to start at sigma
				 ;; if not, it starts with x
				 phase) 1))
		    ;; offsets: (list offset-x offset-y)
		    (offset-right (list (* 2 phase (circle 2))
				       0))
		   (offset-left (list (- (* 2 phase (circle 2)))
				      0)))
	       (let ((lines
		      (append
		       (geom/lines-offset (arrows-right circle)
					  offset-right)
		       (geom/lines-offset (arrows-left circle)
					  offset-left)
		       )))
		 (if clip
		     ;; clipping with a slightly bigger circle
		     ;; to show the complete sigma
		     ;; should I use this here, or on the lines.. where?
		     (let ((circle-modified `(,(circle 0)
					      ,(circle 1)
					      ,(* factor-clip-radius (circle 2) ))
					    ))
		       (geom/clip-lines-in-circle lines circle-modified))
		     lines))))))
	  
(test "SXS lines non-clipped - X"
      (is (equivalent?
	       '((-2.8284271247461903 0 -3.7712361663282534 0.9428090415820634) (0.0 0 -0.9428090415820632 0.9428090415820634) (2.8284271247461903 0 1.8856180831641272 0.9428090415820634) (-2.8284271247461903 0 -3.771236166328254 -0.9428090415820632) (0.0 0 -0.9428090415820636 -0.9428090415820632) (2.8284271247461903 0 1.8856180831641267 -0.9428090415820632) (-2.8284271247461903 0 -1.885618083164127 -0.9428090415820632) (0.0 0 0.9428090415820634 -0.9428090415820632) (2.8284271247461903 0 3.7712361663282534 -0.9428090415820632) (-2.8284271247461903 0 -1.885618083164127 0.9428090415820632) (0.0 0 0.9428090415820634 0.9428090415820632) (2.8284271247461903 0 3.7712361663282534 0.9428090415820632))
	       (lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
		      :clip #f
		      :phase (/ 1 3))

	       ))
      )



(test "SXS lines clipped - X"
      (is (equivalent? '((0.0 0 -0.9428090415820632 0.9428090415820634) (0.0 0 -0.9428090415820636 -0.9428090415820632) (0.0 0 0.9428090415820634 -0.9428090415820632) (0.0 0 0.9428090415820634 0.9428090415820632))
		       (lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
			      :clip #t
			      :phase (/ 1 3))))
      )
(test "SXS lines clipped - <> (square)"
      (is (equivalent? '((0.9428090415820635 0 2.220446049250313e-16 0.9428090415820634) (0.9428090415820635 0 -1.1102230246251565e-16 -0.9428090415820632) (-0.9428090415820635 0 -1.1102230246251565e-16 -0.9428090415820632) (-0.9428090415820635 0 -1.1102230246251565e-16 0.9428090415820632))
		       (lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
			      :clip #t
			      :phase (/ 2 3))))
      )

(comment
 ;; returns some empty lists inside
 ;; ..fixed
 (lines (geom/mk-circle :cx 0 :cy 0 :r 100)
	:phase 0.3555555555555
	:clip #t)
 )


;; (print "loaded aod.sxs, *ns* " *ns*)
