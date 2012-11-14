
;;inhibit the startup screen
(setq inhibit-startup-message t)

(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; set font
;(if window-system
;	(set-face-font 'default "-apple-NanumGothicCoding-medium-normal-normal-*-13-*-*-*-m-0-*-*"))


(set-face-font 'default "NanumGothicCoding-14") 
	
;(setq default-frame-alist '((font . "NanumGothicCoding-14")))

; (set-cursor-color 'red)
; (set-cursor-color 'red)                                                       
(setq default-frame-alist                                                       
          ' ((cursor-color . "red") (cursor-type . box) (width . 60) (height . 55) ))  

;; 모드 라인에 Display time mode 활성화
(display-time-mode t)
(setq display-time-day-and-date t)


;; color-theme
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
;(color-theme-deep-blue)
;(color-theme-midnight)
(color-theme-tty-dark)