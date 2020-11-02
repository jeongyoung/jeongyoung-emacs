
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

(setq ecb-layout-name "left3")
(setq ecb-layout-window-sizes (quote (("left3" (ecb-directories-buffer-name 0.16981132075471697 . 0.2835820895522388) (ecb-sources-buffer-name 0.16981132075471697 . 0.34328358208955223) (ecb-methods-buffer-name 0.16981132075471697 . 0.3582089552238806)))))



;; (defecb-window-dedicator-to-ecb-buffer ecb-set-django-buffer " *ECB django-buf*" nil
;; 	""
;; 	(dolist (buffer (buffer-list))
;; 		(let (name (buffer-name buffer))
;; 			(when (and name (not (string-equal name ""))
;; 								 (string-match "\\*Django:" name))				 
;; 				(switch-to-buffer (get-buffer-create name))
;; 				(switch-to-buffer (get-buffer-create " *ECB django-buf*"))))))


;; (defecb-window-dedicator-to-ecb-buffer ecb-set-django-buffer " *Messages*" nil
;; 	""
;; 	(switch-to-buffer (get-buffer-create " *Messages*")))




;; (ecb-layout-define "django" left
;; 	"This function create the python-djaong layout"
;;   (ecb-set-directories-buffer)
;;   (ecb-split-ver 0.3)
;;   (ecb-set-sources-buffer)
;;   (ecb-split-ver 0.35)
;;   (ecb-set-methods-buffer)
;;   (ecb-split-ver 0.65)
;; 	(ecb-set-django-buffer)
;;   (select-window (next-window)))



(setq ecb-layout-name "left3")

;; Disable buckets so that history buffer can display more entries
(setq ecb-history-make-buckets 'never)






