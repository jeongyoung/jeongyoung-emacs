;;
;;python mode
;; 
;; (load "python-mode" nil t)
;; (autoload 'python-mode "python-mode" "Python editing mode." t)

;; (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (setq interpreter-mode-alist (cons '("python" . python-mode) 
;; 				      interpreter-mode-alist))

;(require 'python-mode)
(add-to-list 'auto-mode-alist '("\.py$" . python-mode))
(autoload 'python-mode "python-mode" "Python editing mode." t)

; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
   
   
;(require 'ipython)
;(setq ansi-color-for-comint-mode t)
;(setenv "LC_CTYPE" "UTF-8")
;(setq py-python-command-args '("--colors" "NoColor"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; jedi 자동완성
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional


	;; 도움말 문서
(require 'info-look)
	(info-lookup-add-help
	 :mode 'python-mode
	 :regexp "[[:alnum:]_]+"
	 :doc-spec
	 '(("(python)Index" nil "")))
	 
	 
;(require 'virtualenv)


;; https://github.com/porterjamesj/virtualenvwrapper.el
;(require 'virtualenvwrapper)

;(venv-initialize-interactive-shells) ;; if you want interactive shell support
;(venv-initialize-eshell) ;; if you want eshell support
;(setq venv-location "/Users/jeongyoung/.virtualenvs/")


;; (defun insert-utf-8-encoding-string ()
;; 	(interactive)
;; 	(insert "# -*- coding: utf-8 -*-"))

;; (define-key py-mode-map "\C-c8" 'insert-utf-8-encoding-string)

;; (defun insert-python-beginning-string()
;; 	(interactive)
;; 	(insert "#!/usr/bin/env python"))

;; (define-key py-mode-map "\C-c1" 'insert-python-beginning-string)
