;; some basic functionality that I miss from clojure

(define-macro (comment . body)
  `())

(define map-indexed
  (let ((+documentation+ "(map-indexed f coll)
Applies f to the collection coll.
f should accept to arguments, i and the element")
	(+signature+ '(f coll)))
    (lambda (f coll)
      (let ((i -1))
	(map (lambda (el)
	       (set! i (+ 1 i))
	       (f i el))
	     coll)))))

(comment
 (map-indexed (lambda (i el)
		(format *stdout* "i ~A el ~A\n" i el))
	      '(a b c))
;; i 0 el a
;; i 1 el b
;; i 2 el c
 )
