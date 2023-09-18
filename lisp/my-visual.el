;;inhibit the startup screen
(setq inhibit-startup-message t)
(setq truncate-lines nil)

(global-display-line-numbers-mode t)

(tool-bar-mode -1)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; (use-package humanoid-themes)
;; (use-package dracula-theme
;;   :config
;;   ;; 't' avoids prompting for loading theme each time
;;   (load-theme 'dracula t))



;; 모드 라인에 Display time mode 활성화
;(display-time-mode t)
;(setq display-time-day-and-date t)

