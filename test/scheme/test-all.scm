(with-let (rootlet)
	  ;; aod.core already required by repl in cpp
	  (require aod.core)
	  ;; WIP: if providing aod.test.gui the (testgui .. ) blocks will be ran
	  (provide 'aod.test.gui)
	  (require aod.test))

(ns aod.test-all)

;; testing files:
(ns-load 'aod.ns)
(ns-load 'aod.geom)
(ns-load 'aod.sxs)
(ns-load 'aod.layout)
(ns-load 'aod.colors)
(ns-load 'aod.midi)
(ns-load 'aod.string)
(ns-load 'aod.imgui.helpers)
(ns-load 'freesound.core)
(ns-load-file "test/c_foreign_test.scm")
(ns-load-file "test/partial_test.scm")
(ns-load-file "test/string_test.scm")
(ns-load-file "test/c-os-test.scm")


;; Testing gui components?

(ns-load 'aod.components.sxs-wheel)


(print "======")
(print "Passed:" (*aod.test* 'pass))
(print "Failed:" (*aod.test* 'fail))

(exit (*aod.test* 'fail))

(comment
 (map (lambda (ns)
		   (print "ns" ns)
		   (if (let? (cdr ns))
		       (cons (car ns)
			     (car ns))
		       (cons (car ns)
			     "not ns")))
      *nss*)
 )

(comment
 (+ 1 2 3)
 *nss*
 (defined? 'environment?)
 (defined? 'let?)
 (let? aod.c.imgui)
 (defined? 'aod.c.foreign)
 aod.c.foreign
 (*nss* 'aod.all-tests)
 
 (let? )
 ()
 (print "hi")
 (defined? 'aod.c.foreign)
 )
