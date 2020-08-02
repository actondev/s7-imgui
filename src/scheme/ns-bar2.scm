(ns ns.bar2)

(ns-require ns.bar :as bar-alias)

;; (print "bar2 after require curlet " (curlet))

(define echo
  (let ((+documentation+ "bar2 echo"))
    (lambda ()
      (print "bar2: echo")
      ;; (print "curlet is " (curlet))
      (bar-alias/echo)))
  )

