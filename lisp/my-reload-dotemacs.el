(defun reload-init-file ()
  "Reload .emacs.d/init.el"
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)
