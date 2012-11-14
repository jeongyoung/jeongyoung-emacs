;; markdown mode [2012-11-15]

(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t) 

(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("md$" . org-markdown-mode))
(add-to-list 'auto-mode-alist '("markdown$" . org-markdown-mode))
(add-to-list 'auto-mode-alist '("README$" . markdown-mode))
