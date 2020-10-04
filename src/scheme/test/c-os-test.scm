(ns test.c-os-test
    :require ((aod.c.os :as os)))

(test "path filename"
      (is equivalent? "bar.txt"
	  (os/path-filename "/home/me/bar.txt")))

(test "temp dir"
      (let ((expected (cond ((provided? 'linux) "/tmp")
			    ((provided? 'windows) "C:/tmp/"))))
	(is equivalent? expected (os/temp-directory-path))))

(comment
 (os/temp-directory-path)
 )
