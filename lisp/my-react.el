;; my-react.el --- description -*- lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; json-mode
(use-package json-mode
  :ensure t)

;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :config (progn
            (setq
             web-mode-markup-indent-offset 2
             web-mode-css-indent-offset 2
             web-mode-code-indent-offset 2
             web-mode-enable-auto-closing t
             web-mode-enable-auto-opening t
             web-mode-enable-auto-pairing t
             web-mode-enable-auto-indentation t
;             web-mode-enable-auto-quoting t
             web-mode-enable-current-column-highlight t
             web-mode-enable-current-element-highlight t
             web-mode-content-types-alist
             '(("jsx" . "/\\(components\\|containers\\|src\\)/.*\\.js[x]?\\'"))))
  :commands web-mode)


(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
