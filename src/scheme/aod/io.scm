(ns aod.io)

(define (copy in out)
  (catch 'wrong-type-arg ; s7 raises this error if write-char gets #<eof>
	 (lambda () 
	   (do () ()			; read/write until #<eof>
	     (write-char (read-char in) out)))
	 (lambda err 
	   #<eof>)))

(define (get-file-contents file)
  (call-with-input-file file
    (lambda (in)
      (call-with-output-string
       (lambda (out)
	 (copy in out))))))
;; clojure style
(define slurp get-file-contents)

(define (put-file-contents file contents)
  (call-with-output-file file
    (lambda (out)
      (call-with-input-string contents
       (lambda (in)
	 (copy in out))))))

;; clojure style
(define spit put-file-contents)

(comment
 (call-with-input-file "/home/actondev/Desktop/s7.txt"
   (lambda (in)
     (write-from-to in *stderr*)))

 (get-file-contents "/home/actondev/Desktop/s7.txt")
 (put-file-contents "/home/actondev/Desktop/s7.txt"
		    "Erase & rewind, cause I've been changing my mind")
 )
