(ns aod.io)

(define (write-from-to from to)
  (catch 'wrong-type-arg ; s7 raises this error if write-char gets #<eof>
	 (lambda () 
	   (do () ()			; read/write until #<eof>
	     (write-char (read-char from) to)))
	 (lambda err 
	   #<eof>)))

(define (get-file-contents file)
  (call-with-input-file file
    (lambda (in)
      (call-with-output-string
       (lambda (out)
	 (write-from-to in out))))))

(define (put-file-contents file contents)
  (call-with-output-file file
    (lambda (out)
      (call-with-input-string contents
       (lambda (in)
	 (write-from-to in out))))))

(comment
 (call-with-input-file "/home/actondev/Desktop/s7.txt"
   (lambda (in)
     (write-from-to in *stderr*)))

 (get-file-contents "/home/actondev/Desktop/s7.txt")
 (put-file-contents "/home/actondev/Desktop/s7.txt"
		    "Erase & rewind, cause I've been changing my mind")
 )
