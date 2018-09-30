(defmacro pri (&rest args)
  `(princ (format ,@args)))

(defmacro expect (exp correct)
  `(let ((c ,correct)
         (result ,exp))
    (if (not (equal result c))
        (pri "%s NOT %s\n" (prin1-to-string 'exp) (prin1-to-string c)))))

(defun read-file (fn)
  (with-temp-buffer
    (insert-file-contents-literally fn)
    (buffer-string)))

(defun read-file-lines (fn)
  (split-string (read-file fn) "\n"))

(defun amb-test ()
  (expect (+ 1 2) 3))

(defun oneline (beg end)
  (interactive "r")
  (let ((str (let* ((lst (split-string (filter-buffer-substring beg end)))
                    (splst (mapcar #'(lambda (s) (concat " " s)) lst)))
               (apply #'concat splst))))
    (kill-region beg end)
    (insert (if (> (length str) 0) (substring str 1) str))))
