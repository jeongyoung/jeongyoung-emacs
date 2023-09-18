(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
	 ("\\.edn\\'" . clojure-mode)))



(use-package cider
  :ensure t)

(use-package paredit
  :ensure t
  :hook (clojure-mode . paredit-mode))
