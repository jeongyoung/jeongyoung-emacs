;(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
;(require 'yasnippet)			; not yasnippet-bundle
;(yas/initialize)
;(yas-global-mode 1)
;(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
;setq yas/snippet-dirs '("~/.emacs.d/vendor/yasnippet/snippets" "~/.emacs.d/snippets"))
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

