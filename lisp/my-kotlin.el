(use-package kotlin-mode
  :ensure t
  :after (lsp-mode dap-mode)
  :config
  (require 'dap-kotlin)
  ;; should probably have been in dap-kotlin instead of lsp-kotlin
  (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
  :hook (kotlin-mode . lsp-deferred)
  :mode ("\\.kt\\'" "\\.kts\\'"))

