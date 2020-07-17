;; note: if i direclty pass this file to the repl executable
;; i need to laod the aod.test in the (rootlet) in order for
;; the tests to run
'(with-let (rootlet)
	  (require aod.test))

(ns aod.sxs)
(ns-require aod.geom :as geom)

(comment
 *ns*
	 )

;; (test "geom loaded?"
;;       (assert (begin
;; 		(print "herreeeeee")
;; 		(geom/echo)
;; 		#t))
;;       (assert (equivalent? '(0 0 10)
;; 			   (let ((circle 
;; 				  (geom/mk-circle :cx 0 :cy 0 :r 10)))
;; 			     (print "created circle " circle)
;; 			     circle))))

;; (test "sxs: geom line offset"
;;       (let ((line (geom/mk-line :x1 0 :y1 0 :x2 10 :y2 10)))
;; 	(is (equivalent? '(0 0 10 10)
;; 			 line))
;; 	(is (equivalent? '(0 2 10 12)
;; 			 (geom/line-offset line '(0 2))))))

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


(comment
 "offsetting/duplicating lines"
 )
;; the sigma-x-square lines to be drawn

(define (-arrow-right circle)
  (map (lambda (theta)
	 (polar-line circle :theta theta))
       (-arrow-angles :dir 'right)))

(define (-arrow-left circle)
  (map (lambda (theta)
	 (polar-line circle :theta theta))
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



(define* (lines circle (phase 0) (clip #t))
  (let ((offset-right (list (* 2 phase (circle 2))
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
	  (geom/clip-lines-in-circle lines circle)
	  lines))))
	  
(comment
 (lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
	     :clip #t)

 
 )

(test "SXS lines non-clipped"
      (is (equivalent?
	       '((-2.8284271247461903 0 -3.8284271247461903 1.0000000000000002) (0.0 0 -1.0 1.0000000000000002) (2.8284271247461903 0 1.8284271247461903 1.0000000000000002) (-2.8284271247461903 0 -3.8284271247461907 -1.0) (0.0 0 -1.0000000000000002 -1.0) (2.8284271247461903 0 1.82842712474619 -1.0) (-2.8284271247461903 0 -1.82842712474619 -1.0) (0.0 0 1.0000000000000002 -1.0) (2.8284271247461903 0 3.8284271247461907 -1.0) (-2.8284271247461903 0 -1.82842712474619 1.0) (0.0 0 1.0000000000000002 1.0) (2.8284271247461903 0 3.8284271247461907 1.0))
	       (lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
		      :clip #f)

	       ))
      )

(test "SXS lines clipped"
      (is (equivalent? '((0.0 0 -1.0 1.0) (0.0 0 -1.0 -1.0) (0.0 0 1.0 -1.0) (0.0 0 1.0 1.0))
		       (lines (geom/mk-circle :cx 0 :cy 0 :r (sqrt 2))
			      :clip #t)))
      )

;;FIXME
(comment
 ;; FIXME
 ;; returns some empty lists inside
 (lines (geom/mk-circle :cx 0 :cy 0 :r 100)
	:phase 0.3555555555555
	:clip #t)
 )


;; (print "loaded aod.sxs, *ns* " *ns*)
