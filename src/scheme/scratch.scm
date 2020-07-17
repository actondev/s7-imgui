;; Scratch file to load in the repl
;; it's a fix about putting the body of the file (its definitions etc)
;; inside the *ns* and not in the rootlet

(require aod.core)
(require aod.test)
(ns user)
(ns-require aod.sxs)
(ns aod.sxs)

(comment
 *ns*
 )
