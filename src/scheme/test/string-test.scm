(ns test.string-test
    :require ((aod.c.string :as s)))
(test "regex-search simple"
      (is-true (s/search "abcdef" "abc"))
      (is-false (s/search "abcdef" "abd")))

(test "regex-search beginning of line"
      (is-true (s/search "abcdef" "^abc"))
      (is-true (s/search "abcdef" "bc"))
      (is-false (s/search "abcdef" "^bc")))

(test "regex-search case"
      (is-true (s/search "abcdef" "^abc"))
      (is-false (s/search "abcdef" "^ABC")))

(test "regex-search capture"
      (is-true (s/search "abcdef" "^ab(.)(.)ef"))
      (is eq? 3 (s/count-matches))
      (is equivalent? "abcdef" (s/match-at 0))
      (is equivalent? "c" (s/match-at 1))
      (is equivalent? "d" (s/match-at 2))
      )

(test "regex-search capture real case"
      (is-true (s/search "Track: split track at time selection" "^Track: (.*)"))
      (is eq? 2 (s/count-matches))
      (is equivalent? "split track at time selection" (s/match-at 1))
      (is-false (s/search "Track: split track at time selection" "^Item: (.*)"))
      )

(test "regex-replace"
      (is equivalent? "now-what-next" (s/replace "now what_next" "[ _]" "-")))

(test "convert upper lower"
      (is equivalent? "demo text" (s/lowercase "Demo Text"))
      (is equivalent? "DEMO TEXT" (s/uppercase "Demo Text")))

(test "negative look ahead"
       (let ((regex "^SWS: (?!.*[0-9']).*track.*$"))
	 ;; we don't want anything  containing quote ' or numbers
	 (is-false (s/search "SWS: Open console with 'V' to set track(s) volume" regex))
	 (is-false (s/search "SWS: Restore saved track selection number 01 ok" regex))
	 ;; rest are ok
	 (is-true (s/search "SWS: Restore saved track selection" regex))
	 ;;
	 ))
