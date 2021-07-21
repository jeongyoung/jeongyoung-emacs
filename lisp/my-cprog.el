;; c-mode
(add-hook 'c-mode-common-hook 'jeongyoug-c-mode-init)
(defun jeongyoug-c-mode-init ()
  (c-set-style "stroustrup")
  (local-set-key "\C-c\C-c" 'compile)
)

