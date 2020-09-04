(ns test.regex
    :require ((aod.c.regex :as r)))
(test "regex-search simple"
      (is (eq? #t (r/search "abcdef" "abc")))
      (is (eq? #f (r/search "abcdef" "abd"))))

(test "regex-search beginning of line"
      (is (eq? #t (r/search "abcdef" "^abc")))
      (is (eq? #t (r/search "abcdef" "bc")))
      (is (eq? #f (r/search "abcdef" "^bc"))))

(test "regex-search case"
      (is (eq? #t (r/search "abcdef" "^abc")))
      (is (eq? #f (r/search "abcdef" "^ABC"))))

(test "regex-search capture"
      (is (eq? #t (r/search "abcdef" "^ab(.)(.)ef")))
      (print "matches " (r/count-matches) "0: "(r/match-at 0) " 1:" (r/match-at 1))
      (is (eq? 3 (r/count-matches)))
      (is (equivalent? "abcdef" (r/match-at 0)))
      (is (equivalent? "c" (r/match-at 1)))
      (is (equivalent? "d" (r/match-at 2)))
      )

(test "regex-search capture real case"
      (is (eq? #t (r/search "Track: split track at time selection" "^Track: (.*)")))
      (is (eq? 2 (r/count-matches)))
      (is (equivalent? "split track at time selection" (r/match-at 1)))
      (is (eq? #f (r/search "Track: split track at time selection" "^Item: (.*)")))
      )

(test "regex-replace"
      (is (equivalent? "now-what-next" (r/replace "now what_next" "[ _]" "-"))))
