(ns aod.scales)

;; Scales given as a vector of semitones from the root
;;
;; - 0 denotes the root/tonic
;; - 0 -> 1 denotes a half step
;; - 3 -> 5 denotes a whole step
;; - 3 -> 4 denotes a half step
;;
;; Thanks to:
;; - https://learningmusic.ableton.com/advanced-topics/modes.html
;; - https://www.bandnotes.info/tidbits/scales/half-whl.htm

(define chromatic (range 12))
;; Whole - Whole - Half - Whole - Whole - Whole - Half
(define major '(0 2 4 5 7 9 11))

;; (natural minor)
;; Whole - Half - Whole - Whole - Half - Whole - Whole
;; Also called Aeolian
(define minor '(0 2 3 5 7 8 10))


(comment
 ;; from my clojure app
 (def chromatic
  [0 1 2 3 4 5 6 7 8 9 10 11 #_12])

(def major
  "Whole - Whole - Half - Whole - Whole - Whole - Half"
  [0 2 4 5 7 9 11 #_12])

(def minor
  "(natural minor)
  Whole - Half - Whole - Whole - Half - Whole - Whole
  Also called Aeolian"
  [0 2 3 5 7 8 10 #_12])

(def minor-harmonic
  "R, W, H, W, W, H, 1 1/2, H"
  [0 2 3 5 7 8 11 #_12])

(def minor-melodic-up
  "R, W, H, W, W, W, W, H"
  [0 2 3 5 7 9 11 #_12])

(def minor-melodic-down
  "R, W, W, H, W, W, H, W"
  [0 2 4 5 7 9 10 #_12])

(def dorian
  "Whole - Half - Whole - Whole - Whole - Half - Whole"
  [0 2 3 5 7 9 10 #_12])

(def phrygian
  "Half - Whole - Whole - Whole - Half - Whole - Whole"
  [0 1 3 5 7 8 10 #_12])

(def lydian
  "Whole - Whole - Whole - Half - Whole - Whole - Half"
  [0 2 4 6 7 9 11 #_12])

(def mixolydian
  "Whole - Whole - Half - Whole - Whole - Half - Whole"
  [0 2 4 5 7 9 10 #_12]
  )

(def locrian
  "Half - Whole - Whole - Half - Whole - Whole - Whole"
  [0 1 3 5 6 8 10 #_12])

)
