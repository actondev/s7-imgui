(ns aod.string)

(define* (count-char-occurs char string (limit +inf.0))
  (let ((occurs -1))
    (let loop ((pos -1))
      (if (or
	   (= occurs limit)
	   (eq? #f pos))
	  occurs
	  (begin
	    (set! occurs (inc occurs))
	    (loop (char-position char string (inc pos))))))))

(test "Count char occurences in string"
      (is (= 4 (count-char-occurs #\. "1.2.3.4.")))
      (is (= 0 (count-char-occurs #\, "1.2.3.4.")))
      )

(test "Count char occurences in string, with stop limit"
      (is (= 2 (count-char-occurs #\. "1.2.3.4." :limit 2)))
      )
