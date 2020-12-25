(ns rootlet
    :require (
	      (aod.c.foreign)
	      (aod.c.midi)
	      (aod.c.imgui)
	      ;; (aod.c.imgui-sdl)
	      (aod.c.imgui.window-flags)
	      (aod.imgui.helpers)
	      (aod.imgui.macros)
	      (aod.c.gl)
	      (aod.c.string)
	      (aod.c.nfd)
	      (aod.c.curl)
	      (aod.c.json)
	      ))

(print "Writing namespaces documentation to ns-doc.md")

(call-with-output-file "docs/ns-doc.md"
  (lambda (out)
    (for-each (lambda (ns)
		;; lol.. aod.c.gl gets rendered as a url
		;; soo.. quoting
		(format out "# (ns `~A`)\n" (car ns))
		(format out "~A\n" ((*nss* (car ns)) '*ns-doc*))
		(for-each (lambda (ns-symbol)
			    ;; (print "ns-symbol " ns-symbol)
			    (when (or #f (ns-should-doc? (car ns-symbol)))
			      ;; quoting cause the ns-symbol might be *like-this*
			      ;; so, avoiding to show it as bold.. we WANT the stars
			      (format out "## `~A` <small>~A</small>\n" (car ns-symbol) (type-of (cdr ns-symbol)))
			      (if (let? (cdr ns-symbol))
				  (format out "value `~W`\n" (cdr ns-symbol))
				  (format out "~A\n" (documentation (cdr ns-symbol))))
			      ))
			  (*nss* (car ns)))
		
		)
	      *nss*)))

(exit)
