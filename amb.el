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

;; from https://www.emacswiki.org/emacs/ElispCookbook#toc6
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun reverse-args (string)
  (let* ((s (reverse (split-string string ",")))
         (out (chomp (car s))))
    (dolist (e (cdr s))
      (setf out (concat out "," (chomp e))))
    out))

(defun reverse-args-interactive (beg end)
  (interactive "r")
  (let ((str (filter-buffer-substring beg end)))
    (kill-region beg end)
    (insert (reverse-args str))))
