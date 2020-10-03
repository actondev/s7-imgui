(ns aod.midi)
(ns-require aod.c.midi :as midi)

(test "Basic midi"
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
