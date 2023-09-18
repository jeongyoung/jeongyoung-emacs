(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "s-p") `projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") `projectile-command-map))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

