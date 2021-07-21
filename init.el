(setq my-root-dir "~/.emacs.d")


;(defcustom elisp-root-dir "~/.emacs.d"
;  "The root directory where elisp is installed")

(add-to-list 'load-path "/usr/local/bin" )


;; Library Path
(add-to-list 'load-path (concat my-root-dir "/lisp"))
(add-to-list 'load-path (concat my-root-dir "/auto-install"))
(add-to-list 'load-path (concat my-root-dir "/vendor"))

;(add-to-list 'load-path (concat my-root-dir "/vendor/orgmode/lisp"))

; 2013-12-07 comment because org-mode built in emacs24

;(add-to-list 'load-path (concat my-root-dir "/vendor/org-toodledo"))
;(add-to-list 'load-path (concat my-root-dir "/vendor/org2blog"))

(setq mac-command-modifier 'meta)
(menu-bar-mode 1)

(setq default-directory "~/")

;; Customization



(load-library "my-reload-dotemacs")		;이맥스 설정파일 리로딩
(load-library "my-packages")	; emacs package management,
(load-library "my-hangul")		; hangul,ime,utf setting
(load-library "my-visual")		; color,font...
(load-library "my-company")		; company-mode
;(load-library "my-elget")	; emacs el-get
;(load-library "my-bindings")		;emacs keyboard shortcuts
;(load-library "my-system")		;emacs system configuration

;(load-library "my-misc")		; miscellaneous setting
;(load-library "my-private")			; private configuration

		
;(load-library "my-cedet")		; cedet setting	
;(load-library "my-ecb")			; ecb setting
;(load-library "my-python")		; python-mode, ipython,pylookup settings
;(load-library "my-django")		; python-django

;(load-library "my-dict")		; eng-han dict

;; ido completion 모드에서 한글파일생성이 안됨
;; 24버전에서 에러발생
;(load-library "my-ido")			; ido setting
;(load-library "my-pymacs")		; pymacs
;(load-library "my-ropemacs")		; ropemacs
;(load-library "my-yasnippet")		; yasnippet setting
;(load-library "my-autoinstall")		; auto-install
;(load-library "my-autocomplete")	; auto-complete
;(load-library "my-anything")		; anything
(load-library "my-org")			; org-mode
;(load-library "my-jde")			; jde-mode
;(load-library "my-eclim")			; emacs-eclim

;(load-library "my-maxframe")			; maxframe
;(load-library "my-wordpress")			; org2blog for wordpress
;(load-library "my-markdown")			; markdown-mode
;(load-library "my-objc")			; objc-mode
;(load-library "my-cprog")		; c-mode
;(load-library "my-mail")		; email setting
;(load-library "my-dayone")		; dayone-mode
;(load-library "my-git")			; egg for git
;(load-library "my-lua")			; lua-mode
;(load-library "my-html")		; html mode
;(load-library "my-gtags")		; global tag mode
;(load-library "my-weblogger")		; weblogger

;(load-library "my-js2")		; js2 


;(load-library "my-moz")		; 
;(load-library "my-flymake")		; flymake
;(load-library "my-nxhtml")		; nxhtml



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(org-bullets cider company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
