(provide 'aod.geom)
(require aod.core)
(display "in aod.geom\n")

;; TODO
;; working only for non-vertical lines
;; leaving that case for now, not needing it atm
;;
;; For the line - circle intersection see:
;; see
;; - https://rosettacode.org/wiki/Line_circle_intersection
;; - https://math.stackexchange.com/questions/228841/how-do-i-calculate-the-intersections-of-a-straight-line-and-a-circle
(define *eps* 0.000001)

(define (distance-sq p1 p2)
  (+
   ;; using abs because for example
   ;; (expt -2.2321321321 2) return a complex number
   (pow (abs (- (p1 0) (p2 0)))
	2)
   (pow (abs (- (p1 1) (p2 1)))
	2)))
(define (distance p1 p2)
  (sqrt (distance-sq p1 p2)))

(comment
 (distance-sq '(0 0) '(1 1))
 (distance-sq '(0 0) '(7.0710678118654755 -7.0710678118654755))
 ;; => 2
 )

(define (point-in-circle? p circle)
  (let ((x (p 0))
	(y (p 1))
	(cx (circle 0))
	(cy (circle 1))
	(r (circle 2))
	)
    (< (distance p `(,cx ,cy))
       (+ r *eps*))
    ))

(test "Point in circle"
      ;; contained
      (assert (eq? #t (point-in-circle? '(0 0) '(0 0 10))))
      (assert (eq? #t (point-in-circle? '(10 0) '(0 0 10))))
      (assert (eq? #t (point-in-circle? '(0 10) '(0 0 10))))
      ;; not contained
      (assert (eq? #f (point-in-circle? '(11 0) '(0 0 10))))
      (assert (eq? #f (point-in-circle? '(0 11) '(0 0 10))))
      ;; the end
 )

(define (sq x)
  (* x x))

;; needs cx cy x1 y1 x2 y2 r
;; binds A B C a b c d
(define-macro (-expand-A-B-C-etc . body)
  `(let* ((A (- y2 y1))
	   (B (- x1 x2))
	   (C (- (* x2 y1) (* x1 y2)))
	   (a (+ (sq A) (sq B)))
	   (b ;; not vertical line..
	    (* 2 (+ (* A C)
		    (* A B cy)
		    (- 0 (* (sq B)
			    cx)))))
	   (c (- (+ (sq C)
		   (* 2 B C cy))
		 (* (sq B)
		    (- (sq r)
		       (sq cx)
		       (sq cy)))))
	   (d-sq ;; discriminant
	    (- (sq b)
	       (* 4 a c)))
	   (d (sqrt d-sq)))
      ,@body
      )
  )

(define (-fx-intersect A B C x)
  (- 0
     (/ (+ (* A x) C)
	B)))

;; for vertical lines
;; (define (-fy-intersect A B C x)
;;   (- 0
;;      (/ (+ (* B y) C)
;; 	A)))

(define (filter-points-in-circle points circle)
  (filter (lambda (p)
	    (point-in-circle? p circle)
	    )
	  points))
(test "Filter points in circle"
      (assert (equivalent? '((0 0) (10 0) (-10 0) (2 2))
			   (filter-points-in-circle  '((0 0) ;; in
							   (10 0) ;; in
							   (-10 0) ;; in
							   (-12 0) ;; out
							   (0 11) ;; out
							   (2 2) ;; in
							   )
							 '(0 0 10))))
 )

(define (point-in-segment? point segment)
  (let ((p1 (list (segment 0) (segment 1)))
	(p2 (list (segment 2) (segment 3))))
    (let ((d1 (distance p1 p2))
	  (d2 (distance p1 point))
	  (d3 (distance p2 point)))
      (< (abs (- d1 d2 d3))
	 *eps*))))

(comment
 (eq? #t (point-in-segment? '(0 0) '(0 0 10 10)))
 (eq? #t (point-in-segment? '(10 10) '(0 0 10 10)))
 (eq? #t (point-in-segment? '(1 1) '(0 0 10 10)))
 (eq? #f (point-in-segment? '(0 1) '(0 0 10 10)))
 )

(define (filter-points-in-segment points segment)
  (filter (lambda (p)
	    (point-in-segment? p segment))
	  points))

(define (clip-line-in-circle line circle)
  (let ((x1 (line 0))
	(y1 (line 1))
	(x2 (line 2))
	(y2 (line 3))
	(cx (circle 0))
	(cy (circle 1))
	(r (circle 2)))
    (let ((p1-in (point-in-circle? `(,x1 ,y1) circle))
	  (p2-in (point-in-circle? `(,x2 ,y2) circle)))
      (cond ((and p1-in p2-in) line)
	    (else (-expand-A-B-C-etc
		   ;; this makes the calculations..
		   ;; dirty but... will do for now
		   (let* ((px1 (/ (- d b)
				  (* 2 a)))
			  (py1 (-fx-intersect A B C px1))
			  (px2 (/ (- 0 b d)
				  (* 2 a)))
			  (py2 (-fx-intersect A B C px2)))
		     ;; (format *stderr* "intersect: (~A, ~A) (~A, ~A)" px1 py1 px2 py2 )
		     (apply append (filter-points-in-circle
				    (filter-points-in-segment `((,x1 ,y1)
								(,x2 ,y2)
								(,px1 ,py1)
								(,px2 ,py2))
							      line)
				    circle)))
		   ))))))

(test "Clip line in circle"
      (assert
       (equivalent? '(0.1 0.1 0.2 0.2)
		    (clip-line-in-circle '(0.1 0.1 0.2 0.2) `(0 0 ,(* 2 (sqrt 2)))))
       "Contained line")
      (assert
       (equivalent? '(0 0 2.0 2.0)
		    (clip-line-in-circle '(0 0 100 100) `(0 0 ,(* 2 (sqrt 2)))))
       "Line with one crossing")
      (assert
       (equivalent? '(0 0 -2.0 -2.0)
		    (clip-line-in-circle '(-100 -100 0 0) `(0 0 ,(* 2 (sqrt 2)))))
       "Line with one crossing")
      (assert
       (equivalent? '(2.0 2.0 -2.0 -2.0)
		    (clip-line-in-circle '(-100 -100 100 100) `(0 0 ,(* 2 (sqrt 2)))))
       "Line that exceeds in both ends"))

(define (rad->deg rad)
  (/ (* 180 rad) pi))

(test "Angle conversions"
      (assert (= 90 (rad->deg (/ pi 2)))))

(define (line-offset line offs)
  (let ((offs-x (offs 0))
	(offs-y (offs 1)))
    (list (+ offs-x (line 0))
	  (+ offs-y (line 1))
	  (+ offs-x (line 2))
	  (+ offs-y (line 3)))))

(test "Line offset"
      (assert (equivalent? '(1 1 2 2)
			   (line-offset '(0 0 1 1)
					'(1 1))
			   )))

(define* (mk-circle cx cy r)
  (list cx cy r))

(define* (mk-line x1 x2 y1 y2)
  (list x1 y1 x2 y2))
