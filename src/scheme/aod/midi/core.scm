(ns aod.midi.core
    :require ((aod.c.midi :as midi)
	      (aod.c.string :as s)))

(define (snap-octave+- x)
  (* (sign x) (mod (abs x) 12)))

(define (snap-octave+ x)
  (mod (abs x) 12))

;; TODO fix
(define (add-sub-octave intervals)
  (map (lambda (x)
	 (if (= x 0) x
	     (values x (- x 12))))
       intervals))

(define (octave x)
  (if (>= x 0)
      (inc (floor (/ x 12)))
      (dec (ceiling (/ x 12)))))

(define -the-notes
  (inlet :C 0 :DO 0
	 :C# 1 :DO# 1
	 :D 2 :RE 2
	 :D# 3 :RE# 3
	 :E 4 :MI 4
	 :F 5 :FA 5
	 :F# 6 :FA# 6
	 :G 7 :SOL 7 :SO 7
	 :G# 8 :SOL# 8 :SO# 8
	 :A 9 :LA 9
	 :A# 10 :LA# 10
	 :B 11 :SI 11 :TI 11
	 ))

(comment
 (-the-notes 'DO#)
 )

(define-macro* (note the-note (octave 4))
  (let ((c-based (-the-notes
		  (string->symbol
		   (s/uppercase (symbol->string the-note)))
		  )))
    (+ c-based (* 12 (inc octave)))))

(test "human notes"
      (is eq? 60 (note c 4))
      (is eq? 72 (note c 5))
      (is eq? 60 (note do 4))
      (is eq? 60 (note DO 4))
      (is eq? 62 (note re 4))
      (is eq? 21 (note a 0)))

(comment
 (note c 4)
 )
(comment
 (floor (/ -1 12))
 )

(test "octave number (maybe rethinkg about it?)"
      (is eq? 1 (octave 0))
      (is eq? 1 (octave 1))
      (is eq? 1 (octave 11))
      (is eq? 2 (octave 12))
      ;; minus
      (is eqv? -1 (octave -1))
      (is eqv? -1 (octave -11))
      (is eqv? -2 (octave -12))
      ;;
      )

(test "adding sub octave"
      (eqv? '(0 1 -1 3 -3 5 -5)
	    (add-sub-octave '(0 1 3 5))))

(test "snapping in octave +-"
      (is eq? 4 (snap-octave+- 4))
      (is eq? 4 (snap-octave+- 16))
      ;; hm.. (eq? -4 -4) returns false, wtf
      (is eqv? -4 (snap-octave+- -4))
      (is eqv? -4 (snap-octave+- -16)))

(test "aod.c.midi Basic"
      (define c1-note-on '(144 12 127))
      (define c1-note-off '(128 12 0))
      ;; note on message, but velocity 0
      ;; happens in my midi keyboard with RtMidi in linux
      (define c1-note-off-2 '(144 12 0))

      (is-true (apply midi/note-on? c1-note-on))
      (is-false (apply midi/note-off? c1-note-on))
      (is = 12 (apply midi/note-number c1-note-on))
      ;; 
      (is-false (apply midi/note-on? c1-note-off))
      (is-true (apply midi/note-off? c1-note-off))
      (is = 12 (apply midi/note-number c1-note-off))

      (is-false (apply midi/note-on? c1-note-off-2))
      (is-true (apply midi/note-off? c1-note-off-2))
      )
