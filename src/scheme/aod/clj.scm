;; some basic functionality that I miss from clojure
(display "loading aod/clj.scm\n")

#;
(define-macro (comment . body)
   `())

;; #<unspecified> or () is better?
(define-expansion (comment . body) #<unspecified>)

;; hm that fails
;; (define-expansion (comment . body) (values))

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

;; got from s7.. stuff.scm ?
(define-macro (dotimes spec . body)	;; spec = (var end . return)
	(let ((e (gensym))
	      (n (car spec)))
	  `(do ((,e ,(cadr spec))
		(,n 0 (+ ,n 1)))
	       ((>= ,n ,e) ,@(cddr spec))
	     ,@body)))

(comment
 (dotimes (i 3)
   ;; (display "here")
   (format *stderr* "i is ~A\n" i)
   )
 "
i is 0
i is 1
i is 2
"
 => #t

 (dotimes (i 3 (format #f "finished with i ~A" i)) ;; <= the 3rd i is the return statement. could be anything
   ;; (display "here")
   (format *stderr* "i is ~A\n" i)
   )
 "i is 0
i is 1
i is 2
"
 => "finished with i 3"
 )

(define iota
  (let ((+documentation+ "(iota n (start 0) (incr 1)) returns a list counting from start for n:\n\
    (iota 3) -> '(0 1 2)"))
    (lambda* (n (start 0) (incr 1))
      (if (or (not (integer? n))
	      (< n 0))
	  (error 'wrong-type-arg "iota length ~A should be a non-negative integer" n))
      (let ((lst (make-list n)))
	(do ((p lst (cdr p))
	     (i start (+ i incr)))
	    ((null? p) lst)
	  (set! (car p) i))))))

(define range iota)
;; reverse range
(define (rrange n)
  (range n :start (dec n) :incr -1))

(define mod modulo)

(define-macro (not= . args)
  `(not (= ,@args)))

#;
(define-expansion (pow base power)
(expt base power))

(define pow expt)

(define-expansion (identity what)
  `,what)

(define-macro (watch var fn)
  `(set! (setter ',var) 
	 (lambda (s v e)
	   ;; calling fn with old and new value
	   (,fn (e ',var) v)
	   v)))

(comment
 (define x 1)
 (watch x (lambda (old new)
		(print "x changed from" old "to" new)))
 ((curlet) 'x)
 (define x 2)
 )

(define (keys coll)
  (if (or (hash-table? coll)
	  (let? coll))
      (map (lambda  (el)
	     (car el))
	   coll)
      (error 'wrong-type-arg "keys arg ~A is not a hash-table nor a let" coll)))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (partial fn . args)
  (lambda rest-args
    (apply fn (append args rest-args))
    ))

;; same as in clj
(define (interleave c1 c2)
  (let loop ((res ())
	     (c1 c1)
	     (c2 c2))
    (if (or (null? c1)
	    (null? c2))
	res
	(loop (append res (list (car c1) (car c2)))
	      (cdr c1)
	      (cdr c2)))))

(comment
 (interleave '(a b c) '(1 2 3))
 ;; (a 1 b 2 c 3)
 (interleave '(a b c) '(1 2))
 ;; (a 1 b 2)
 (interleave '(a b) '(1 2 3))
 ;; (a 1 b 2)
 )

;; useful for the letd : let with list destructuring
(define (interleave-pairs c1 c2)
  (let loop ((res ())
	     (c1 c1)
	     (c2 c2))
    (if (or (null? c1)
	    (null? c2))
	res
	(loop (append res (list (list (car c1) (car c2))))
	      (cdr c1)
	      (cdr c2)))))
(comment
 (interleave-pairs '(a b c) '(1 2 3))
 ;; ((a 1) (b 2) (c 3))
 )

;; let with list destructuring
;; Note: the _ is normally bound.. that ok?
;; 
;; bacro cause we want the environment of where this is called. From
;; the doc: A bacro is a macro that expands its body and evaluates the
;; result in the calling environment.
(define-bacro (letd bindings . body)
  `(let
       ,(apply
	 append
	 (map
	  (lambda (param+exp)
	    ;; the {s} is for param or params
	    (let ((param{s} (car param+exp)))
	      (if (pair? param{s})
		  ;; pair => destructuring
		  (let ((vals (eval (cadr param+exp))))
		    (interleave-pairs param{s} vals))
		  ;; normal symbol
		  (list (list (car param+exp) (cadr param+exp))))))
	  bindings))
     ,@body))

(comment
 (letd ((a 1)
	(b 2)
	((c d) (list (+ 1 2) 4)))
       (list a b c d ))
 ;; (1 2 3 4)
 (letd ((a 1)
	(b 2)
	((c d) (list (+ 1 2) 4))
	(e (+ 1 d)))
       (list a b c d e))
 ;; will fail (e doesn't know about d)
 )

;; let* with list destructuring
(define-bacro (letd* bindings . body)
  `(let*
       ,(apply
	 append
	 (map
	  (lambda (param+exp)
	    (print "param+exp" param+exp)
	    ;; the {s} is for param or params
	    (let ((param{s} (car param+exp)))
	      (if (pair? param{s})
		  ;; pair => destructuring
		  (let ((vals (eval (cadr param+exp))))
		    (interleave-pairs param{s} vals))
		  ;; normal symbol
		  (list (list (car param+exp) (cadr param+exp))))))
	  bindings))
     ,@body))

(comment
 (letd* ((a 1)
	(b 2)
	((c d) (list (+ 1 2) 4))
	(e (+ 1 d)))
	(list a b c d e))
 ;; (1 2 3 4 5)
 )

;; -> ->>
(comment
 (defmacro ->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

 (defmacro as->
  "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  {:added "1.5"}
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))
 )

(define (nnull? x)
  (not (null? x)))

(define (drop n coll)
  (let loop ((i 0)
	     (coll coll))
    (if (= i n)
	coll
	(loop (inc i)
	      (cdr coll)))))

(test "drop"
      (is equivalent?
	  '(c d)
	  (drop 2 '(a b c d))))

;; https://github.com/clojure/clojure/blob/clojure-1.10.1/src/clj/clojure/core.clj#L1677
(define-macro (-> x . forms)
  (let loop ((x x)
	     (forms forms))
    (if (nnull? forms)
	(let* ((form (car forms))
	      (threaded (if (list? form)
			    `(,(car form) ,x ,@(cdr form))
			    `(,form ,x))))
	  (loop threaded (cdr forms)))
	x)))

(define-macro (->> x . forms)
  (let loop ((x x)
	     (forms forms))
    (if (nnull? forms)
	(let* ((form (car forms))
	      (threaded (if (list? form)
			    `(,(car form) ,@(cdr form) ,x) ;; only the order here changes
			    `(,form ,x))))
	  (loop threaded (cdr forms)))
	x)))

(test "-> and ->>"
      (is = 5.0 (-> 10.0
		   (/ 2)
		   ))

      (is = 0.2 (->> 10.0
		   (/ 2)
		   )))

(comment
 (-> 10
     inc
     (* 2))

 (-> 1
     (< 2))

 (->> 1
     (< 2))

 (->> 10
     inc
     (* 2))
 )

(comment
 (apply + 1)
 (if (not (null? (cdr (list 1))))
     1
     0)

 (-> 10
     inc
     (* 2))
 )
(provide 'aod.clj)
