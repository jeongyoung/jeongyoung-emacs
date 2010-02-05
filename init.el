(setq my-root-dir "~/jeongyoung-emacs")
;; 명시적으로 선언하지 않을 경우, emacs-app에서 로딩이 안됨
(setq load-path (cons (expand-file-name my-root-dir) load-path))


;(defcustom elisp-root-dir "~/.emacs.d"
;  "The root directory where elisp is installed")

;; Library Path
;(add-to-list 'load-path my-root-dir)
(add-to-list 'load-path (concat my-root-dir "/auto-install"))
(add-to-list 'load-path (concat my-root-dir "/plugins"))



;; Customization

(load-library "my-emacs")      ;emacs system configuration
(load-library "my-visual")		; color,font...
(load-library "my-hangul")		; hangul,ime,utf setting
(load-library "my-misc")		; miscellaneous setting
(load-library "my-cedet")		; cedet setting
(load-library "my-ecb")			; ecb setting
(load-library "my-python")		; python-mode,ipython,pylookup settings
(load-library "my-ido")			; ido setting
;(load-library "my-pymacs")		; pymacs
(load-library "my-ropemacs")		; ropemacs
(load-library "my-yasnippet")		; yasnippet setting
(load-library "my-autoinstall")		; auto-install
(load-library "my-autocomplete")	; auto-complete
;(load-library "my-anything")		; anything
(load-library "my-org")			; org-mode
(load-library "my-jde")			; jde-mode


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(ecb-wget-setup (quote cons))
 '(jde-global-classpath (quote ("/System/Library/Frameworks/JavaVM.framework/Classes/" ".")))
 '(jde-jdk-registry (quote (("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0"))))
 '(org-agenda-files (quote ("/Users/scott/org/newgtd.org"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
