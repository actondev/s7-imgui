(ns stochastic.core)


;; from notes from the metalevel
(define (pick-list lst)
  (let* ((len (length lst))
	 (pos (random len)))
    (list-ref lst pos)))

(define (chance? prob)
  (< (random 1.0) prob))
;; </> notes from the metalevel


(define (snap val permitted)
  (let loop ((incd val)
	     (decd val))
    (cond ((member incd permitted) incd)
	  ((member decd permitted) decd)
	  (#t (loop (inc incd) (dec decd))))))

(test "snap"
      (is eq? -3 (snap 2 '(10 -3)))
      (is eq? 10 (snap 5 '(10 -3))))


(define* (markov (init 0)
		 (transitions '(0))
		 (snap-fn (lambda (x) x))
		 (steps 1))
  (let loop
      ((init init)
       (i 1)
       (state (list init)))
    (if (= i steps)
	(begin
	  state)
	(let ((new-step (snap-fn (+ init (pick-list transitions)))))
	  (loop
	   new-step
	   (inc i)
	   (append state (list new-step)))))))

(comment
 (type-of identity)
 (markov :init 0 :transitions '(0 1 -1 3 -3 5 -5 7 -7) :steps 8)
 (ns-require 'aod.midi.core)
 (markov :init 0 :transitions '(0 1 -1 3 -3 5 -5 7 -7 12 -12 12 -12) :steps 2
	 :snap-fn (let ((snaps (aod.midi.core/add-sub-octave '(0 3 5 7 9 12))))
		    (lambda (x)
		      (snap x snaps))))

 (let ((x -13)
       (T 12))
   (* (sign x) (mod (abs x) T)))

 (snap 11 '(0 3 5 7 9 12))
 
 (pick-list '(a b c d e f))
 (chance? 0.8)
 )

(comment
 (snap 2 '(10 -3))
 )

(comment
 (let ((snap '(0 3 5 7))
       (test-val 12))
   (let loop ((incd test-val)
	      (decd test-val))
     (print "incd " incd "decd" decd)
     (cond ((member incd snap) incd)
	   ((member decd snap) decd)
	   (#t (loop (inc incd) (dec decd))))))

(let ((snap '(0 3 5 7))
       (test 2))
   (print "here.."))
 ;;
 )




(comment
 (markov)
 )
