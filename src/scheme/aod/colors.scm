(display "loading aod/colors.scm\n")

(provide 'aod.colors)
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
  (let ((phase (* phase 360)))
    (map 
     (lambda (phase3)
       (let ((phase (- phase phase3)))
	 (colors/-triplet-ramp1 phase)))
     '(0 120 240))))

(define (rgb-steps steps)
  (map
   (lambda (x)
     (colors/rgb-phase (/ x steps)))
   (range steps))
)

(comment
 (equivalent?
  (rgb-phase 0)
  '(1 0 0))
 
 (equivalent?
  (rgb-phase 1)
  '(1 0 0))

 (rgb-steps 3)
 ;; =>
 ((1 0 0) (0 1 0) (0 0 1))
 
 (rgb-steps 12)
 ;; =>
 ((1 0 0)
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
 
 (range 3) ;; => (0 1 2)
 (iota 3) ;; => (0 1 2)
 )
