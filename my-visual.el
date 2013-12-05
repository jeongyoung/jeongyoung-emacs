;;inhibit the startup screen
(setq inhibit-startup-message t)
(setq truncate-lines nil)

;(global-visual-line-mode t)


;; set font
;(if window-system
;	(set-face-font 'default "-apple-NanumGothicCoding-medium-normal-normal-*-13-*-*-*-m-0-*-*"))

(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])


(setq default-frame-alist 
      '((width . 100) (height . 55) 
;	(font . "-apple-NanumGothicCoding-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;	(font . "AppleGothic-14")
;	(font . "NanumGothicCoding-14")
;	(font . "MalgunGothic-14")
;	(font . "-apple-Malgun_Gothic-medium-normal-normal-*-*-*-*-*-p-0-iso10646-1")
;	(font . "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
				(font . "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
;				(font . "NanumGothicCoding-14")
				(scroll-bar-mode . 0)
				(cursor-color . "red")
				(cursor-type . "box")
				(menu-bar-lines . 0)))


;; color-theme

(require 'color-theme)

(color-theme-initialize)

;(color-theme-deep-blue)
;(color-theme-midnight)
;(color-theme-dark-blue2)
;(color-theme-gnome2)
(color-theme-classic)

;;
;; color-theme-solarized
;; http://ethanschoonover.com/solarized
;; 

;(add-to-list 'load-path (concat my-root-dir "/vendor/color-theme-solarized"))
;(require 'color-theme-solarized)
;(color-theme-solarized-dark)

(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))


;(if window-system (set-face-font 'default "NanumGothicCoding-14"))
 
;(set-fontset-font "fontset-default" 'kana '("AppleGothic" . "unicode-bmp"))
;(set-fontset-font "fontset-default" 'han '("AppleGothic" . "unicode-bmp"))
	


;(setq default-frame-alist '((font . "NanumGothicCoding-14")))

; (set-cursor-color 'red)
; (set-cursor-color 'red)                                                       
;(setq default-frame-alist                                                                ' ((cursor-color . "red") (cursor-type . box) (width . 60) (height . 55) ))  

;; 모드 라인에 Display time mode 활성화
(display-time-mode t)
(setq display-time-day-and-date t)


