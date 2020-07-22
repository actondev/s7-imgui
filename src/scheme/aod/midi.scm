(ns aod.midi)
(ns-require aod.c.midi :as midi)

(test "Basic midi"
      (define c1-note-on '(144 12 127))
      (define c1-note-off '(128 12 0))

      (is (eq? #t (apply midi/note-on? c1-note-on)))
      (is (eq? #f (apply midi/note-off? c1-note-on)))
      (is (= 12 (apply midi/note-number c1-note-on)))
      ;; 
      (is (eq? #f (apply midi/note-on? c1-note-off)))
      (is (eq? #t (apply midi/note-off? c1-note-off)))
      (is (= 12 (apply midi/note-number c1-note-off)))
      )
