;; let's see if I can get this to run on windows

(require libc.scm)
(require r7rs.scm)


(define (box-inc! b)
  (display "box-inc!\n")
  (set-box! b (+ 1 (unbox b)))
  )

(define counter-box (box 0))
