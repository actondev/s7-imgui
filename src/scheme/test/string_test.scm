(ns test.string
    :require ((aod.c.string :as s)))
(test "regex-search simple"
      (is (eq? #t (s/search "abcdef" "abc")))
      (is (eq? #f (s/search "abcdef" "abd"))))

(test "regex-search beginning of line"
      (is (eq? #t (s/search "abcdef" "^abc")))
      (is (eq? #t (s/search "abcdef" "bc")))
      (is (eq? #f (s/search "abcdef" "^bc"))))

(test "regex-search case"
      (is (eq? #t (s/search "abcdef" "^abc")))
      (is (eq? #f (s/search "abcdef" "^ABC"))))

(test "regex-search capture"
      (is (eq? #t (s/search "abcdef" "^ab(.)(.)ef")))
      (print "matches " (s/count-matches) "0: "(s/match-at 0) " 1:" (s/match-at 1))
      (is (eq? 3 (s/count-matches)))
      (is (equivalent? "abcdef" (s/match-at 0)))
      (is (equivalent? "c" (s/match-at 1)))
      (is (equivalent? "d" (s/match-at 2)))
      )

(test "regex-search capture real case"
      (is (eq? #t (s/search "Track: split track at time selection" "^Track: (.*)")))
      (is (eq? 2 (s/count-matches)))
      (is (equivalent? "split track at time selection" (s/match-at 1)))
      (is (eq? #f (s/search "Track: split track at time selection" "^Item: (.*)")))
      )

(test "regex-replace"
      (is (equivalent? "now-what-next" (s/replace "now what_next" "[ _]" "-"))))

(test "convert upper lower"
      (is (equivalent? "demo text" (s/lowercase "Demo Text")))
      (is (equivalent? "DEMO TEXT" (s/uppercase "Demo Text"))))
