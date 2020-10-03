;; (require aod.test) ;; require didn't work
(load "aod/test.scm")
(ns freesound.core
    :require ((aod.c.curl :as curl) ;; todo :refer curl
	      (aod.c.json :as json)))


;; defvar.. ?
;; some dynamic variable?
(define *token* "YOUR_FREESOUND_SECRET_TOKEN")

(define* (make-filter
	  (duration #f) ;; duration is a list (from to)
	  )
  (let ((filter ""))
    (when duration
      (set! filter (format #f "~Aduration:[~A TO ~A]"
			   filter (car duration) (cadr duration))))

    filter))

(define -fmt-query-filter "https://freesound.org/apiv2/search/text/?token=~A&query=~A&filter=~A")


(define* (search query
		 ;; optionals
		 (filter ""))
  (print "searching " query)
  (curl/curl (print-ret (format #f
				-fmt-query-filter
				*token*
				(curl/easy-escape query)
				(curl/easy-escape filter)))
	     :opts (inlet 'ssl-verify-peer 0)))

(define* (search&random query (filter ""))
  (let* ((res (search query :filter filter))
	 (decoded (json/parse res)))
    (print "res " res)
    (let* ((count (decoded "count"))
	   (idx
	    0 ;; TODO the count returned responses to all the results
	    ;; but our response is paginated (doesn't contain until #count-1)
	    ;; (random count)
	    ))
      (print "decoded " decoded "idx " idx)
      (decoded "results" idx))))

;; for tests
;; I have secrets.scm file
(when (provided? 'aod.test)
  (ns-require secrets)
  (set! *token* secrets/freesound))

'(test "duration filter"
      (assert (equivalent?
	       "duration:[0.0 TO 0.1]"
	       ;; 0.3 fails, it's formatted as 0.30000000000000004 heh
	       (print-ret (make-filter :duration '(0.0 0.1))))))

(test "search with query and duration filter"
      (let ((filter (make-filter :duration '(0.0 1.0)))
	    (result (search "amen snare" :filter filter)))
	(print result)))

'(test "search&random with query and duration filter"
      (let* ((filter (make-filter :duration '(0.0 1.0)))
	    (result (search&random "amen snare" :filter filter)))
	result))
