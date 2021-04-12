(defun set-home-dir (dir)
  "Set a new HOME directory. This is where Emacs will look for init files and
   where '~' will default to."
  (setenv "HOME" dir)
  (message (format "HOME location is %s" (getenv "HOME"))))

(set-home-dir "c:/users/goldm/")