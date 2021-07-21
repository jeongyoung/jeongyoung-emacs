;;inhibit the startup screen
(setq inhibit-startup-message t)
(setq truncate-lines nil)

;(global-visual-line-mode t)


;; set font
;(if window-system
;	(set-face-font 'default "-apple-NanumGothicCoding-medium-normal-normal-*-13-*-*-*-m-0-*-*"))

;(setq ansi-color-names-vector
;      ["black" "tomato" "PaleGreen2" "gold1"
;       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])


;(tool-bar-mode -1)


;; zenburn-theme by package
(load-theme 'zenburn t)

(setq default-frame-alist 
      '((width . 200) (height . 55) 
					;	(font . "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
					;				(font . "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
	(font . "-*-D2Coding-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;				(font . "NanumGothicCoding-14")
				(scroll-bar-mode . 0)
				(cursor-color . "red")
;				(cursor-type . "box")
				))



;; color-theme
;(add-to-list 'load-path (concat my-root-dir "/vendor/color-theme-6.6.0"))
;(require 'color-theme)
;(color-theme-deep-blue)
;(color-theme-midnight)
;(color-theme-dark-blue2)
;(color-theme-gnome2)
;(color-theme-classic)

;(add-to-list 'custom-theme-load-path (concat my-root-dir "/themes"))

;; (eval-after-load "color-theme"
;; 	'(progn
;; 		 (color-theme-initialize)
;; 		 (color-theme-hober)						 
;; 		 ))




;;
;; color-theme-solarized
;; http://ethanschoonover.com/solarized

;; (add-to-list 'load-path (concat my-root-dir "/vendor/color-theme-solarized"))
;; (add-to-list 'custom-theme-load-path (concat my-root-dir "/vendor/color-theme-solarized"))
;; (require 'color-theme-solarized)

;; (require 'solarized-definitions
;; 				 (locate-file "solarized-definitions.el" custom-theme-load-path
;; 											'("c" "")))
;; (create-solarized-theme dark)
;; (load-theme 'solarized-dark t)



;; solarized theme 
;; https://github.com/bbatsov/solarized-emacs

;; (add-to-list 'load-path (concat my-root-dir "/vendor/solarized-emacs"))
;; (require 'solarized)

;; (setq solarized-termcolor 256)
;; (deftheme solarized-dark "The dark variant of the Solarized colour theme")
;; (create-solarized-theme 'dark 'solarized-dark)
;; (provide-theme 'solarized-dark)
;; (load-theme 'solarized-dark t)

;; zenburn-theme by package


;; sanityinc-solarized by package
;(load-theme 'sanityinc-solarized-dark t)



;(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
;(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))


;(if window-system (set-face-font 'default "NanumGothicCoding-14"))
 
;(set-fontset-font "fontset-default" 'kana '("AppleGothic" . "unicode-bmp"))
;(set-fontset-font "fontset-default" 'han '("AppleGothic" . "unicode-bmp"))
	


;(setq default-frame-alist '((font . "NanumGothicCoding-14")))

;; 모드 라인에 Display time mode 활성화
(display-time-mode t)
(setq display-time-day-and-date t)
