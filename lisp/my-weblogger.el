
(setq system-time-locale "C")


;; scriptogram function


;; writing a post

(defun make-new-post-name ()
	"making a new post name"
	(format-time-string "%y%m%d-%H%M.md"))

(defun make-new-post-format()
	"making a new post format"
	(list
	 (insert "Title: \n")
	 (insert "Date: ")
	 (insert (format-time-string "%Y-%m-%d %H:%M\n"))
	 (insert "Tags: \n")
	 (insert "\n")))

(defun new-post () 
  "making a new post"
  (interactive)
  (list
   (find-file (make-new-post-name))
   (make-new-post-format)
   (goto-char 8)))

