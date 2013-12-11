;;
;; ECB(Emacs Code Browser)
;; 

(require 'ecb)

;; off tip of the day
(setq ecb-tip-of-the-day nil)

;; 업그레이드 옵션 출력 안하기
(setq ecb-auto-compatibility-check nil)

;; customize the keys for ECB
(define-key ecb-mode-map (kbd "M-1") 'ecb-goto-window-directories)
(define-key ecb-mode-map (kbd "M-2") 'ecb-goto-window-sources)
(define-key ecb-mode-map (kbd "M-3") 'ecb-goto-window-methods)
(define-key ecb-mode-map (kbd "M-4") 'ecb-goto-window-history)
(define-key ecb-mode-map (kbd "M-5") 'ecb-goto-window-compilation)
(define-key ecb-mode-map (kbd "M-0") 'ecb-goto-window-edit1)

(ecb-layout-name "left1")
