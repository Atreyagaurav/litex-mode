
(defun units-command (val from to)
  (shell-command-to-string
   (format "units \"%f %s\" \"%s\""
	   val from to)))

(defun units-convert (val from to)
  (let ((out-lines (split-string (units-command val from to) "\n")))
    (if (length= out-lines 3)
	(string-to-number (string-trim-left (car out-lines) "[^0-9.]+"))
      (user-error "%s" (string-join out-lines "\n")))))

