;;my-method.el

(setq my-command-buffer-hooks (make-hash-table))

(defun my-command-on-save-buffer (c)
  "Run a command <c> every time the buffer is saved "
  (interactive "sShell command: ")
  (puthash (buffer-file-name) c my-command-buffer-hooks))

