(require 'python)
(require 'python-django)
(global-set-key (kbd "C-x j") 'python-django-open-project)


(load (concat my-root-dir "/vendor/nxhtml/autostart.el"))
;(autoload 'django-html-mumamo-mode (concat my-root-dir "/vendor/nxhtml/autostart.el"))
;(setq auto-mode-alist
;      (append '(("\\.html?$" . django-html-mumamo-mode)) auto-mode-alist))
;(setq mumamo-background-colors nil) 
;(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mumamo-mode))
