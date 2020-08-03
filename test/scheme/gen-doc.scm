(ns rootlet
    :require (
	      ;; (aod.colors)
	      ;; (aod.midi) ;; this contains only tests actually
	      (aod.c.midi)
	      (aod.c.imgui)
	      (aod.c.imgui-sdl)
	      (aod.c.imgui.window-flags)
	      (aod.imgui.helpers)
	      (aod.imgui.macros)
	      ;; (aod.components.input)
	      ;; (aod.components.piano-wheel)
	      (aod.c.gl)
	      (aod.c.nfd)))

(print "Writing namespaces documentation to ns-doc.el")
(call-with-output-file "docs/ns-doc.el"
  (lambda (out)
    (let-temporarily (((*s7* 'print-length) 100000000000000000))
		     (format out "~A"
			     (map (lambda (ns)
				    ;; (print "ns" ns)
				    (if (let? (cdr ns))
					(cons (car ns)
					      (ns-doc (car ns)))
					(values)))
				  *nss*)))))

(exit)
