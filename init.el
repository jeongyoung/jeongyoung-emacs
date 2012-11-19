(setq my-root-dir "~/.emacs.d")
;; 명시적으로 선언하지 않을 경우, emacs-app에서 로딩이 안됨
(setq load-path (cons (expand-file-name my-root-dir) load-path))


;(defcustom elisp-root-dir "~/.emacs.d"
;  "The root directory where elisp is installed")

;; Library Path
;(add-to-list 'load-path my-root-dir)
(add-to-list 'load-path (concat my-root-dir "/auto-install"))
(add-to-list 'load-path (concat my-root-dir "/vendor"))
(add-to-list 'load-path (concat my-root-dir "/vendor/color-theme-6.6.0"))
(add-to-list 'load-path (concat my-root-dir "/vendor/orgmode/lisp"))
(add-to-list 'load-path (concat my-root-dir "/vendor/org-toodledo"))
(add-to-list 'load-path (concat my-root-dir "/vendor/org2blog"))


;; Customization

(load-library "my-system")      ;emacs system configuration
(load-library "my-visual")		; color,font...
(load-library "my-hangul")		; hangul,ime,utf setting
(load-library "my-misc")		; miscellaneous setting
(load-library "my-cedet")		; cedet setting
;(load-library "my-ecb")			; ecb setting
(load-library "my-python")		; python-mode, ipython,pylookup settings
;; ido completion 모드에서 한글파일생성이 안됨
;(load-library "my-ido")			; ido setting
;(load-library "my-pymacs")		; pymacs
;(load-library "my-ropemacs")		; ropemacs
(load-library "my-yasnippet")		; yasnippet setting
;(load-library "my-autoinstall")		; auto-install
(load-library "my-autocomplete")	; auto-complete
;(load-library "my-anything")		; anything
(load-library "my-org")			; org-mode
;(load-library "my-jde")			; jde-mode
(load-library "my-private")			; password
(load-library "my-maxframe")			; maxframe
(load-library "my-wordpress")			; org2blog for wordpress
(load-library "my-markdown")			; markdown-mode
;(load-library "my-objc")			; objc-mode
(load-library "my-cprog")		;c-mode