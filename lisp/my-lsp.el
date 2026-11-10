(use-package lsp-mode
  :ensure t
  :hook (
	 (web-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (kotlin-mode . lsp-deferred)
	 (js-ts-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands (lsp lsp-deferred))


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp)



;; DAP
(use-package dap-mode
  ;; Uncomment the config below if you xwant all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  (require 'dap-chrome)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  ;; (general-define-key
  ;;   :keymaps 'lsp-mode-map
  ;;   :prefix lsp-keymap-prefix
  ;;   "d" '(dap-hydra t :wk "debugger")))
)
