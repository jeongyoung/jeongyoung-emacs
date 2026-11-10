;; my-cmode.el --- description -*- lexical-binding: t; -*-


(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(setq c-default-style "stroustrup")

(defun my-c-c++-style ()
  "Set indentation and automatic filling for C and C++ buffers."
  (setq-local c-basic-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local fill-column 100)
  (auto-fill-mode 1)
  ;; c-context-line-break inserts the newline and lets cc-mode calculate the
  ;; indentation once.  Do not run electric-indent a second time here.
  (electric-indent-local-mode -1)
  (local-set-key (kbd "RET") #'c-context-line-break)
  (local-set-key (kbd "<return>") #'c-context-line-break)
  (local-set-key (kbd "C-j") #'c-context-line-break))

(defun my-c-ts-style ()
  "Set indentation and RET behavior for tree-sitter C/C++ buffers."
  (setq-local c-ts-mode-indent-offset 4)
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  (setq-local fill-column 100)
  (auto-fill-mode 1)
  (electric-indent-local-mode 1)
  (local-set-key (kbd "RET") #'newline-and-indent)
  (local-set-key (kbd "<return>") #'newline-and-indent)
  (local-set-key (kbd "C-j") #'newline-and-indent))

(add-hook 'c-mode-common-hook #'my-c-c++-style)

;; Emacs versions with tree-sitter use these modes instead of c-mode-common.
(add-hook 'c-ts-mode-hook #'my-c-ts-style)
(add-hook 'c++-ts-mode-hook #'my-c-ts-style)

(provide 'my-cmode)
;;; my-cmode.el ends here
