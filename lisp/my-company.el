;M-n							company-select-next
;M-p							company-select-previous


;;   (define-key company-active-map (kbd "\C-n") 'company-select-next)
;;   (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;;   (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;;   (define-key company-active-map (kbd "\C-v") 'company-show-location)
;;   (define-key company-active-map (kbd "<tab>") 'company-complete)
;;   (define-key company-active-map (kbd "\C-g") '(lambda ()
;;                                                  (interactive)
;;                                                  (company-abort)))

;; (define-key company-active-map (kbd "\C-n") 'company-select-next)
;; (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "\C-RET") 'company-complete)
;; (define-key company-active-map (kbd "\C-g") '(lambda ()
;;                                                   (interactive)
;;                                                   (company-abort)))

;;(global-set-key [(control return)] 'company-complete)

;;(global-company-mode t)

;;(setq company-semantic-modes '(c-mode c++-mode js-mode jde-mode java-mode emacs-lisp-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

