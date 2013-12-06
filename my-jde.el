
;; (custom-set-variables
;;  '(jde-global-classpath (quote ("/System/Library/Frameworks/JavaVM.framework/Classes/" ".")))
;;  '(jde-jdk-registry (quote (("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0"))))
;; )

;; (add-to-list 'load-path "~/.emacs.d/jde/lisp")


;(load "jde-autoload")
;(require 'jde)


(autoload 'jde-mode "jde" "JDE mode" t)


(add-hook 'jde-mode-hook
          (lambda()
    (local-set-key [(control return)] 'jde-complete)
    (local-set-key [(shift return)] 'jde-complete-minibuf)
    (local-set-key [(meta return)] 'jde-complete-in-line)))




(add-to-list 'ac-modes 'jde-mode)


(defun java-my-minor () 
	(progn
;		(gtags-mode t)
;		(glasses-mode t)
		(auto-complete-mode t)
;		(add-to-list 'ac-sources ac-source-gtags)
		(local-set-key [(control return)] 'jde-complete)
		(local-set-key [(shift return)] 'jde-complete-minibuf)
		(local-set-key [(meta return)] 'jde-complete-in-line)))

(add-hook 'jde-mode-hook 'java-my-minor)

(setq auto-mode-alist
			(append '(("\\.java\\'" . jde-mode)) auto-mode-alist))


