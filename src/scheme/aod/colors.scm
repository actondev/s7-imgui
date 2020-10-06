(ns aod.colors
    :require ((aod.c.colors)))

;; https://en.wikipedia.org/wiki/Hue#/media/File:HSV-RGB-comparison.svg
;; starts with the "red"
(define (-triplet-ramp1 phase360)
  (let ((phase360 (mod phase360 360)))
    (cond
     ((< phase360 60) 1)
     ((< phase360 120) ;; falling
      (- 1 (/ (- phase360 60)
              60)))
     ((< phase360 240) 0)
     ((< phase360 300) ;; rising
      (/ (- phase360 240)
         60))
      (else 1)
      )))

;; phase is 0.0 .. 1.0
(define (rgb-phase phase)
  (let* ((phase (mod phase 1))
	 (phase (* phase 360)))
    (map 
     (lambda (phase3)
       (let ((phase (- phase phase3)))
	 (-triplet-ramp1 phase)))
     '(0 120 240))))

(define (triplet-phase phase)
  (let* ((phase (mod phase 1))
	 (phase (* phase 360)))
    (map 
     (lambda (phase3)
       (let ((phase (- phase phase3)))
	 (-triplet-ramp1 phase)))
     '(0 120 240))))

(define (rgb-wheel steps)
  (map
   (lambda (x)
     (triplet-phase (/ x steps)))
   (range steps)))

(test "RGB phase"
      (is equivalent? '(1 0 0)
	  (rgb-phase 0))

      (is equivalent? '(1 0 0)
	  (rgb-phase 1))
      )
(test "RGB wheel"
      (is equivalent? '((1 0 0) (0 1 0) (0 0 1))
	  (rgb-wheel 3))
      ;; see https://www.w3schools.com/colors/colors_wheels.asp
      ;; the RGB color wheel
      (is equivalent? '((1 0 0)
			(1 1/2 0)
			(1 1 0)
			(1/2 1 0)
			(0 1 0)
			(0 1 1/2)
			(0 1 1)
			(0 1/2 1)
			(0 0 1)
			(1/2 0 1)
			(1 0 1)
			(1 0 1/2))
	  (rgb-wheel 12)))

(define (ryb->rgb ryb)
  (aod.c.colors/ryb->rgb ryb))

(test "RYB -> RGB"
      (is equivalent? '(1 0 0)
	  (ryb->rgb '(1 0 0)))

      (is equivalent? '(1 1 0)
	  (ryb->rgb '(0 1 0)))
 )
