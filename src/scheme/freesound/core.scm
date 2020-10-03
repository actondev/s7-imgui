;; (require aod.test) ;; require didn't work
;; (load "aod/test.scm")
(ns freesound.core
    :require ((aod.c.curl :as curl) ;; todo :refer curl
	      (aod.c.json :as json)))


;; defvar.. ?
;; some dynamic variable?
(define *token* "YOUR_FREESOUND_SECRET_TOKEN")
(define *default-preview* "preview-hq-ogg")

(define* (make-filter
	  ;; duration is a list (from to)
	  (duration #f))
  (let ((filter ""))
    (when duration
      (set! filter (format #f "~Aduration:[~A TO ~A]"
			   filter (car duration) (cadr duration))))
    filter))

(define (-make-fields fields)
  ;; comma separated strings
  (apply append (map (lambda (s)
		      (append s ","))
		     fields)))

(define* (-make-search-url query (filter #f))
  (let ((url (format #f "https://freesound.org/apiv2/search/text/?token=~A&query=~A"
		     *token*
		     (curl/easy-escape query))))
    (when filter
      (cond ((string? filter)
	     () ;; ok, don't modify
	     )
	    ((list? filter)
	     (set! filter (apply make-filter filter)))
	    ;; else.. TODO exception
	    (t (print "INVALID FILTER PASSED! ignoring")))
      ;; now filter is a string
      (set! url (format #f "~A&filter=~A" url
			(curl/easy-escape filter))))
    url))

(comment
 (-make-search-url "amen break")
 )
(test "search url"
      (let-temporarily
       ((*token* "XYZ"))
       (is equivalent?
	   "https://freesound.org/apiv2/search/text/?token=XYZ&query=amen%20break"
	   (-make-search-url "amen break"))
       ;; with text filter
       (is equivalent?
	   "https://freesound.org/apiv2/search/text/?token=XYZ&query=amen%20break&filter=duration%3A%5B0.0%20TO%201.0%5D"
	   (-make-search-url "amen break" :filter (make-filter :duration '(0.0 1.0))))
       ;; with list filter
       (is equivalent?
	   "https://freesound.org/apiv2/search/text/?token=XYZ&query=amen%20break&filter=duration%3A%5B0.0%20TO%201.0%5D"
	   (-make-search-url "amen break" :filter '(:duration (0.0 1.0))))))

(define* (-make-get-url id (fields #f))
  (let ((url (format #f "https://freesound.org/apiv2/sounds/~A/?token=~A" id *token*)))
    (when fields
      (set! url (format #f "~A&fields=~A" url (-make-fields fields))))
    url))

(test "get sound url"
      ;; https://freesound.org/docs/api/resources_apiv2.html#sound-resources
      (is equivalent?
	  "https://freesound.org/apiv2/sounds/25667/?token=XYZ&fields=previews,tags,"
	  (let-temporarily ((*token* "XYZ"))
			   (-make-get-url 25667 '("previews" "tags")))))

(define* (search query
		 ;; optionals
		 (filter #f))
  (let ((url (-make-search-url query :filter filter)))
    (curl/curl url
	       :opts '(:ssl-verify-peer 0))))

;; returns a json object
(define* (search&random query (filter #f))
  (let* ((res (search query :filter filter))
	 (decoded (json/parse res)))
    (let* ((count (decoded "results" 'count))
	   (idx (random count)))
      (if (> count 0)
	  (decoded "results" idx)
	  ;; else.. return #f or () aka nil?
	  #f))))

(define* (get id (fields #f))
  (let ((get-url (-make-get-url id :fields fields)))
    (curl/curl get-url
	       :opts '(:ssl-verify-peer 0))))

(comment
 (get 25667)
 )

(define* (get-preview id (type *default-preview*))
  (let* ((res (get id :fields '("previews")))
	 (decoded (json/parse res)))
    (decoded "previews" type)))

(comment
 (get-preview 25667)
 )

(define* (search&random-preview
	  query
	  (filter #f)
	  (preview-type *default-preview*))
  (when-let (res (search&random query :filter filter))
	    ;; (print "res " res)
	    (let ((id (res "id")))
	      (get-preview id :type preview-type))))

(comment
 (search&random-preview "amen snare")
 (search&random "snare")
 )

;; for tests
;; I have secrets.scm file
'(when (provided? 'aod.test)
  (ns-require secrets)
  (set! *token* secrets/freesound))

(test "duration filter"
      (is equivalent?
	  "duration:[0.0 TO 0.1]"
	  ;; 0.3 fails, it's formatted as 0.30000000000000004 heh
	  (make-filter :duration '(0.0 0.1))))

'(test "search with query and duration filter"
      (let ((filter (make-filter :duration '(0.0 1.0)))
	    (result (search "amen snare" :filter filter)))
	(print result)))

'(test "search&random with query and duration filter"
      (let* ((filter (make-filter :duration '(0.0 1.0)))
	    (result (search&random "amen snare" :filter filter)))
	(print result)))
