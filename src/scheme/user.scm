(begin (require aod.core)
       (autoload 'ns.bar "ns-bar.scm")
       (autoload 'ns.bar2 "ns-bar2.scm"))
(ns user)
(ns-require ns.bar2 :as bar2)
;; (ns-require ns.bar :as bar)

    

(comment
 *nss*
 *ns*

 (map
  (lambda (binding)
    (print "binding " binding))
  (*nss* 'ns.bar2))

 (*nss* 'user)
 (*nss* 'ns.bar2)

 (with-ns ns.bar2
	  (bar-alias/echo))

 (with-ns user
	  (bar2/echo))

 (with-let (*nss* 'user)
	   (bar/echo))
 
 (with-ns user
	  (bar/echo))

 (with-ns ns.bar
	  (echo))

 (with-ns ns.bar2
	  (echo))

 (*nss* 'ns.bar2)
  )

;; (print "user loaded, ns is " *ns* " curlet " (let->list (curlet)))
