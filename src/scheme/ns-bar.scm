(ns ns.bar)

(define (-private-info)
  "ns.bar private info")

(define echo
  (let ((+documentation+ "bar echo"))
    (lambda ()
      (print "echo from bar " (-private-info)))))


;; (echo-doc)
;; (print (documentation echo-doc))
(print "--- ns.bar loaded, curlet " (curlet))
