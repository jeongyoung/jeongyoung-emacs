;; auto-complete mode

;; ;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;; (require 'auto-complete-config)
;; (ac-config-default)

;; (setq ac-use-menu-map t)

;; (setq ac-auto-show-menu 0.3)

;; ;;
;; ;; Use C-n/C-p to select candidates
;; ;; --------------------------------

;; (define-key ac-completing-map "\C-n" 'ac-next)
;; (define-key ac-completing-map "\C-p" 'ac-previous)


;; (setq ac-auto-show-menu 0.3)

;; ;(setq-default ac-sources '(ac-source-semantic-raw))
;; (setq-default ac-sources '(ac-source-semantic ac-source-semantic-raw))


(use-package auto-complete-config
  :ensure auto-complete
  :bind ("M-<tab>" . my--auto-complete)
  :init
  (defun my--auto-complete ()
    (interactive)
    (unless (boundp 'auto-complete-mode)
      (global-auto-complete-mode 1))
    (auto-complete))
  )

