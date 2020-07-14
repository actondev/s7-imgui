(provide 'aod.geom)
(require aod.core)

(define (distance-sq p1 p2)
  (+
   (pow (- (p1 0) (p2 0))
	2)
   (pow (- (p1 1) (p2 1))
	2)))

(comment
 (distance-sq '(0 0) '(1 1))
 ;; => 2
 )

(define (p-in-circle p circle)
  (let ((x (p 0))
	(y (p 1))
	(cx (circle 0))
	(cy (circle 1))
	(r (circle 2))
	)
    (<= (distance-sq p `(,cx ,cy))
     (* r r))
    ))

(comment
 (eq? #t (p-in-circle '(0 0) '(0 0 10)))
 (eq? #t (p-in-circle '(10 0) '(0 0 10)))
 (eq? #t (p-in-circle '(0 10) '(0 0 10)))
 (eq? #f (p-in-circle '(11 0) '(0 0 10)))
 (eq? #f (p-in-circle '(0 11) '(0 0 10)))
 )

;; TODO
(define (clip-line-in-circle line circle)
  (let ((x1 (line 0))
	(y1 (line 1))
	(x2 (line 2))
	(y2 (line 3))
	(cx (circle 0))
	(cy (circle 1))
	(r (circle 2)))
    (let ((p1-in (p-in-circle `(,x1 ,y1) circle))
	  (p2-in (p-in-circle `(,x2 ,y2) circle)))
      (format *stderr* "p1in ~A p2 in ~A\n" p1-in p2-in)
      (cond ((and p1-in p2-in) line)
	    ((not (or p1-in p2-in)) ())
	    (else 'todo))
      
      )
    ;; TODO
    ;; return the new line (x1 y1 x2 y2) that is contained inside the circle
    ;; if it's not contained at all returns ()
    ))

(comment
 (equivalent? '(0 0 8 0) (clip-line-in-circle '(0 0 8 0) '(0 0 8)))
 (eq? ()  (clip-line-in-circle '(10 10 20 20) '(0 0 8)))
 
 )

(comment (let ((theta (atan
			     (/ ( - y2 y1)
				(- x2 x1)
				))))
		 theta))


