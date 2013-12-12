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


(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
   
   

;(setq ansi-color-for-comint-mode t)
;(setenv "LC_CTYPE" "UTF-8")
;(setq py-python-command-args '("--colors" "NoColor"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; jedi 자동완성
;; http://tkf.github.io/emacs-jedi/


(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(add-hook 'python-mode-hook 'auto-complete-mode)

(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional


	;; 도움말 문서
;; (require 'info-look)
;; 	(info-lookup-add-help
;; 	 :mode 'python-mode
;; 	 :regexp "[[:alnum:]_]+"
;; 	 :doc-spec
;; 	 '(("(python)Index" nil "")))


(add-to-list 'load-path (concat my-root-dir "/vendor/pydoc-info-0.2"))
(require 'pydoc-info)

(info-lookup-add-help
 :mode 'python-mode
 :parse-rule 'pydoc-info-python-symbol-at-point
 :doc-spec
 '(("(python)Index" pydoc-info-lookup-transform-entry)
	 ("(django)Index" pydoc-info-lookup-transform-entry)))

(setq pylookup-dir (concat my-root-dir "/vendor/pylookup"))
(add-to-list 'load-path pylookup-dir)

; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup" 
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(global-set-key "\C-ch" 'pylookup-lookup)

;(setq browse-url-browser-function 'w3m-browse-url) ;; w3m
;(setq browse-url-default-browser "firefox.exe")

	 
;(require 'virtualenv)


;; https://github.com/porterjamesj/virtualenvwrapper.el
;(require 'virtualenvwrapper)

;(venv-initialize-interactive-shells) ;; if you want interactive shell support
;(venv-initialize-eshell) ;; if you want eshell support
;(setq venv-location "/Users/jeongyoung/.virtualenvs/")


(defun insert-utf-8-encoding-string ()
	(interactive)
	(insert "# -*- coding: utf-8 -*-"))

(defun insert-python-beginning-string()
	(interactive)
	(insert "#!/usr/bin/env python"))

(add-hook 'python-mode-hook 
					(lambda ()						
						(define-key python-mode-map "\C-c8" 'insert-utf-8-encoding-string)
						(define-key python-mode-map "\C-c1" 'insert-python-beginning-string)))


